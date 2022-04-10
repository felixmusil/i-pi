! The main program which runs our driver test case potentials
!
! Copyright (C) 2013, Joshua More and Michele Ceriotti
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be included
! in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
! IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
! TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
! SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!
! Currently the potentials implemented are the Lennard-Jones
! potential, the Silvera-Goldman para-hydrogen potential and
! the ideal gas (i.e. no interaction at all)

      PROGRAM DRIVER
        USE pes_shell
        USE F90SOCKETS, ONLY : open_socket, writebuffer, readbuffer
     IMPLICIT NONE

     ! SOCKET VARIABLES
     INTEGER, PARAMETER :: MSGLEN=12   ! length of the headers of the driver/wrapper communication protocol
     INTEGER socket, inet, port        ! socket ID & address of the server
     CHARACTER(LEN=1024) :: host

     ! COMMAND LINE PARSING
     CHARACTER(LEN=1024) :: cmdbuffer
     INTEGER ccmd, vstyle
     INTEGER verbose
     INTEGER commas(4), par_count      ! stores the index of commas in the parameter string
     DOUBLE PRECISION vpars(4)         ! array to store the parameters of the potential

     ! SOCKET COMMUNICATION BUFFERS
     CHARACTER(LEN=12) :: header
     LOGICAL :: isinit=.false., hasdata=.false.
     INTEGER cbuf, rid
     CHARACTER(LEN=2048) :: initbuffer      ! it's unlikely a string this large will ever be passed...
     DOUBLE PRECISION, ALLOCATABLE :: msgbuffer(:)

     ! PARAMETERS OF THE SYSTEM (CELL, ATOM POSITIONS, ...)
     DOUBLE PRECISION sigma, eps, rc, rn, ks ! potential parameters
     DOUBLE PRECISION stiffness ! lennard-jones polymer
     INTEGER n_monomer ! lennard-jones polymer
     INTEGER nat
     DOUBLE PRECISION pot, dpot, dist
     DOUBLE PRECISION, ALLOCATABLE :: atoms(:,:), forces(:,:), datoms(:,:)
     DOUBLE PRECISION cell_h(3,3), cell_ih(3,3), virial(3,3), mtxbuf(9), dip(3), charges(3), dummy(3,3,3), vecdiff(3)
     DOUBLE PRECISION volume
     DOUBLE PRECISION, PARAMETER :: fddx = 1.0d-5

     !Parameters for AIMB
     Integer :: nbead,natm

     ! NEIGHBOUR LIST ARRAYS
     INTEGER, DIMENSION(:), ALLOCATABLE :: n_list, index_list
     DOUBLE PRECISION init_volume, init_rc ! needed to correctly adjust the cut-off radius for variable cell dynamics
     DOUBLE PRECISION, ALLOCATABLE :: last_atoms(:,:) ! Holds the positions when the neighbour list is created
     DOUBLE PRECISION displacement ! Tracks how far each atom has moved since the last call of nearest_neighbours

     ! DMW
     DOUBLE PRECISION efield(3)
     INTEGER i, j
     real::tstart,tend

     ! parse the command line parameters
     ! intialize defaults
     ccmd = 0
     inet = 1
     host = "localhost"//achar(0)
     port = 31415
     verbose = 0
     par_count = 0
     vstyle = -1
     rc = 0.0d0
     init_rc = 0.0d0
     volume = 0.0d0
     init_volume = 0.0d0

     DO i = 1, COMMAND_ARGUMENT_COUNT()
        CALL GET_COMMAND_ARGUMENT(i, cmdbuffer)
        IF (cmdbuffer == "-u") THEN ! flag for unix socket
           inet = 0
           ccmd = 0
        ELSEIF (cmdbuffer == "-h") THEN ! read the hostname
           ccmd = 1
        ELSEIF (cmdbuffer == "-p") THEN ! reads the port number
           ccmd = 2
        ELSEIF (cmdbuffer == "-m") THEN ! reads the style of the potential function
           ccmd = 3
        ELSEIF (cmdbuffer == "-o") THEN ! reads the parameters
           ccmd = 4
        ELSEIF (cmdbuffer == "-v") THEN ! flag for verbose standard output
           verbose = 1
        ELSEIF (cmdbuffer == "-vv") THEN ! flag for verbose standard output
           verbose = 2
        ELSE
           IF (ccmd == 0) THEN
              WRITE(*,*) " Unrecognized command line argument", ccmd
              CALL helpmessage
              STOP "ENDED"
           ENDIF
           IF (ccmd == 1) THEN
              host = trim(cmdbuffer)//achar(0)
           ELSEIF (ccmd == 2) THEN
              READ(cmdbuffer,*) port
           ELSEIF (ccmd == 3) THEN
              IF (trim(cmdbuffer) == "pwater") THEN
                 vstyle = 1
              ELSE
                 WRITE(*,*) " Unrecognized potential ", trim(cmdbuffer)
                 WRITE(*,*) " Use -m [pwater] "
                 STOP "ENDED"
              ENDIF
           ELSEIF (ccmd == 4) THEN
              par_count = 1
              commas(1) = 0
              DO WHILE (index(cmdbuffer(commas(par_count)+1:), ',') > 0)
                 commas(par_count + 1) = index(cmdbuffer(commas(par_count)+1:), ',') + commas(par_count)
                 READ(cmdbuffer(commas(par_count)+1:commas(par_count + 1)-1),*) vpars(par_count)
                 par_count = par_count + 1
              ENDDO
              READ(cmdbuffer(commas(par_count)+1:),*) vpars(par_count)
           ENDIF
           ccmd = 0
        ENDIF
     ENDDO

     IF (vstyle == -1) THEN
        WRITE(*,*) " Error, type of potential not specified."
        CALL helpmessage
        STOP "ENDED"
     ELSEIF (1 == vstyle) THEN
        IF (par_count /= 0) THEN
           WRITE(*,*) "Error: no initialization string needed."
           STOP "ENDED"
        ENDIF
        natm = 768
        nw = natm/3
        pbc = 1
        box = 0.d0
        if (pbc.eq.1) then
          box(1) = 19.7295/0.5291772083 !35.23300
          box(5) = 19.7295/0.5291772083 !35.23300
          box(9) = 19.7295/0.5291772083 !35.23300
         !  box(1) = 15.6593/0.5291772083
         !  box(5) = 15.6593/0.5291772083
         !  box(9) = 15.6593/0.5291772083
         ! box(1) = 35.23300
         ! box(5) = 35.23300
         ! box(9) = 35.23300
        end if
        call pes_init(nw)

        isinit = .true.
     ENDIF

     IF (verbose > 0) THEN
        WRITE(*,*) " DRIVER - Connecting to host ", trim(host)
        IF (inet > 0) THEN
           WRITE(*,*) " on port ", port, " using an internet socket."
        ELSE
           WRITE(*,*) " using an UNIX socket."
        ENDIF
     ENDIF

     ! Calls the interface to the POSIX sockets library to open a communication channel
     CALL open_socket(socket, inet, port, host)
     nat = -1
     DO WHILE (.true.) ! Loops forever (or until the wrapper ends!)

        ! Reads from the socket one message header
        CALL readbuffer(socket, header, MSGLEN)
        IF (verbose > 0) WRITE(*,*) " Message from server: ", trim(header)

        IF (trim(header) == "STATUS") THEN
           ! The wrapper is inquiring on what we are doing
           IF (.not. isinit) THEN
              CALL writebuffer(socket,"NEEDINIT    ",MSGLEN)  ! Signals that we need initialization data
              IF (verbose > 1) WRITE(*,*) "    !write!=> ", "NEEDINIT    "
           ELSEIF (hasdata) THEN
              CALL writebuffer(socket,"HAVEDATA    ",MSGLEN)  ! Signals that we are done computing and can return forces
              IF (verbose > 1) WRITE(*,*) "    !write!=> ", "HAVEDATA    "
           ELSE
              CALL writebuffer(socket,"READY       ",MSGLEN)  ! We are idling and eager to compute something
              IF (verbose > 1) WRITE(*,*) "    !write!=> ", "READY       "
           ENDIF
        ELSEIF (trim(header) == "INIT") THEN     ! The driver is kindly providing a string for initialization
           CALL readbuffer(socket, rid)
           IF (verbose > 1) WRITE(*,*) "    !read!=> RID: ", rid
           CALL readbuffer(socket, cbuf)
           IF (verbose > 1) WRITE(*,*) "    !read!=> init_lenght: ", cbuf
           CALL readbuffer(socket, initbuffer, cbuf)
           IF (verbose > 1) WRITE(*,*) "    !read!=> init_string: ", cbuf
           IF (verbose > 0) WRITE(*,*) " Initializing system from wrapper, using ", trim(initbuffer)
           isinit=.true. ! We actually do nothing with this string, thanks anyway. Could be used to pass some information (e.g. the input parameters, or the index of the replica, from the driver
        ELSEIF (trim(header) == "POSDATA") THEN  ! The driver is sending the positions of the atoms. Here is where we do the calculation!

           ! Parses the flow of data from the socket
           CALL readbuffer(socket, mtxbuf, 9)  ! Cell matrix
           IF (verbose > 1) WRITE(*,*) "    !read!=> cell: ", mtxbuf
           cell_h = RESHAPE(mtxbuf, (/3,3/))
           CALL readbuffer(socket, mtxbuf, 9)  ! Inverse of the cell matrix (so we don't have to invert it every time here)
           IF (verbose > 1) WRITE(*,*) "    !read!=> cell-1: ", mtxbuf
           cell_ih = RESHAPE(mtxbuf, (/3,3/))

           ! The wrapper uses atomic units for everything, and row major storage.
           ! At this stage one should take care that everything is converted in the
           ! units and storage mode used in the driver.
           cell_h = transpose(cell_h)
           cell_ih = transpose(cell_ih)
           ! We assume an upper triangular cell-vector matrix
           volume = cell_h(1,1)*cell_h(2,2)*cell_h(3,3)

           CALL readbuffer(socket, cbuf)       ! The number of atoms in the cell
           IF (verbose > 1) WRITE(*,*) "    !read!=> cbuf: ", cbuf
           IF (nat < 0) THEN  ! Assumes that the number of atoms does not change throughout a simulation, so only does this once
              nat = cbuf
              IF (verbose > 0) WRITE(*,*) " Allocating buffer and data arrays, with ", nat, " atoms"
              ALLOCATE(msgbuffer(3*nat))
              ALLOCATE(atoms(nat,3), datoms(nat,3))
              ALLOCATE(forces(nat,3))
              atoms = 0.0d0
              datoms = 0.0d0
              forces = 0.0d0
              msgbuffer = 0.0d0
           ENDIF

           CALL readbuffer(socket, msgbuffer, nat*3)
           IF (verbose > 1) WRITE(*,*) "    !read!=> positions: ", msgbuffer
           DO i = 1, nat
              atoms(i,:) = msgbuffer(3*(i-1)+1:3*i)
           ENDDO

           CALL readbuffer(socket, rid)
           IF (vstyle == 1) THEN ! AIMB water potential.
              !call cpu_time(tstart)
              call fg_all(atoms,pot,forces)
              !call cpu_time(tend)
              !write(*,*) tend-tstart,pot


!               write(*,*) pot*627.51
!               do i = 1,768
!                  write(*,'(I4,3F15.8)') i,-forces(i,:)
!               end do
!
              ! do not compute the virial term
           ENDIF
           hasdata = .true. ! Signal that we have data ready to be passed back to the wrapper
        ELSEIF (trim(header) == "GETFORCE") THEN  ! The driver calculation is finished, it's time to send the results back to the wrapper

           ! Data must be re-formatted (and units converted) in the units and shapes used in the wrapper
           DO i = 1, nat
              msgbuffer(3*(i-1)+1:3*i) = forces(i,:)
           ENDDO
           virial = transpose(virial)

           CALL writebuffer(socket,"FORCEREADY  ",MSGLEN)
           IF (verbose > 1) WRITE(*,*) "    !write!=> ", "FORCEREADY  "
           CALL writebuffer(socket,pot)  ! Writing the potential
           IF (verbose > 1) WRITE(*,*) "    !write!=> pot: ", pot
           CALL writebuffer(socket,nat)  ! Writing the number of atoms
           IF (verbose > 1) WRITE(*,*) "    !write!=> nat:", nat
           CALL writebuffer(socket,msgbuffer,3*nat) ! Writing the forces
           IF (verbose > 1) WRITE(*,*) "    !write!=> forces:", msgbuffer
           CALL writebuffer(socket,reshape(virial,(/9/)),9)  ! Writing the virial tensor, NOT divided by the volume
           IF (verbose > 1) WRITE(*,*) "    !write!=> strss: ", reshape(virial,(/9/))

           IF (vstyle==5 .or. vstyle==6 .or. vstyle==8) THEN ! returns the dipole
              initbuffer = " "
              WRITE(initbuffer,*) dip(1:3)
              cbuf = LEN_TRIM(initbuffer)
              CALL writebuffer(socket,cbuf) ! Writes back the molecular dipole
              IF (verbose > 1) WRITE(*,*) "    !write!=> extra_lenght: ", cbuf
              CALL writebuffer(socket,initbuffer,cbuf)
              IF (verbose > 1) WRITE(*,*) "    !write!=> extra: ", initbuffer
           ELSE
              cbuf = 7 ! Size of the "extras" string
              CALL writebuffer(socket,cbuf) ! This would write out the "extras" string, but in this case we only use a dummy string.
              IF (verbose > 1) WRITE(*,*) "    !write!=> extra_lenght: ", cbuf
              CALL writebuffer(socket,"nothing",7)
              IF (verbose > 1) WRITE(*,*) "    !write!=> extra: nothing"
           ENDIF
           hasdata = .false.
        ELSE
           WRITE(*,*) " Unexpected header ", header
           STOP "ENDED"
        ENDIF
     ENDDO
     IF (nat > 0) DEALLOCATE(atoms, forces, msgbuffer)

   CONTAINS
     SUBROUTINE helpmessage
        ! Help banner
        WRITE(*,*) " SYNTAX: driver.x [-u] -h hostname -p port -m [pwater] "
        WRITE(*,*) "         -o 'comma_separated_parameters' [-v] "
        WRITE(*,*) ""
      END SUBROUTINE helpmessage

  END PROGRAM
