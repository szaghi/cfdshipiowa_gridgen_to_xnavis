module type_mesh_t
USE IR_Precision     ! Integers and reals precision definition.
USE Data_Type_Vector ! Definition of type Type_Vector.
implicit none
private
! Derived type for mesh information.
type, public :: Type_Mesh
  integer(I4P)::                   Ni,Nj,Nk     !< Number of cell in i, j, k.
  type(Type_Vector), allocatable:: nodes(:,:,:) !< Nodes coordinates [1:Ni,1:Nj,1:Nk].
  contains
    procedure, pass(self) :: scale_grid_mesh !< Scale grid points accordingly *grid_ratio*.
endtype Type_Mesh
contains
  elemental subroutine scale_grid_mesh(self, scale_ratio)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Scale grid points (number) accordingly *grid_ratio*.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(Type_Mesh), intent(inout) :: self         !< Mesh data.
  integer(I4P),     intent(in)    :: scale_ratio  !< Mesh data.
  type(Type_Vector), allocatable  :: nodes(:,:,:) !< Nodes coordinates [1:Ni,1:Nj,1:Nk].
  integer(I4P)                    :: i            !< Counter.
  integer(I4P)                    :: j            !< Counter.
  integer(I4P)                    :: k            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(nodes(1:self%Ni, 1:self%Nj, 1:self%Nk))
  self%Ni = self%Ni / scale_ratio
  self%Nj = self%Nj / scale_ratio
  self%Nk = self%Nk / scale_ratio
  nodes = self%nodes
  deallocate(self%nodes)
  allocate(self%nodes(1:self%Ni, 1:self%Nj, 1:self%Nk))
  do k=1, size(nodes, dim=3), scale_ratio
    do j=1, size(nodes, dim=2), scale_ratio
      do i=1, size(nodes, dim=1), scale_ratio
        self%nodes(i/scale_ratio, j/scale_ratio, k/scale_ratio) = nodes(i, j, k)
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine scale_grid_mesh

endmodule type_mesh_t

!> @brief gridgen2mbinfo is a converter between the Pointwise Gridgen output file format (CFDShipIowa format) and the Ansys
!> ICEM-CFD multiblock one.
program cfdshipiowa2xnavis
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision     ! Integers and reals precision definition.
USE Data_Type_Tensor ! Definition of type Type_Tensor.
USE Data_Type_Vector ! Definition of type Type_Vector.
USE Lib_IO_Misc      ! Input/Output miscellanea module.
use Data_Type_OS, only : Type_OS
use type_mesh_t , only : Type_Mesh
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
! Derived type for patch information.
type Type_Patch
  integer(I4P):: p                    !< Patch global number.
  integer(I4P):: tp                   !< Face bc id-type.
  integer(I4P):: norm                 !< Face normal id.
  integer(I4P):: i1,i2                !< Indexes extent in i dir of reference block.
  integer(I4P):: j1,j2                !< Indexes extent in j dir of reference block.
  integer(I4P):: k1,k2                !< Indexes extent in k dir of reference block.
  integer(I4P):: b_adj                !< Adjacent block index.
  integer(I4P):: p_adj                !< Adjacent patch index.
  integer(I4P):: norm_adj             !< Face normal id of adjacent block.
  integer(I4P):: i1_adj,i2_adj,is_adj !< Indexes extent in i dir of adjacent block and sign (1,-1).
  integer(I4P):: j1_adj,j2_adj,js_adj !< Indexes extent in j dir of adjacent block and sign (1,-1).
  integer(I4P):: k1_adj,k2_adj,ks_adj !< Indexes extent in k dir of adjacent block and sign (1,-1).
endtype Type_Patch
! Derived type for boundary conditions information.
type Type_BC
  integer(I4P)::                  Np       !< Number of assigned patch.
  type(Type_Patch), allocatable:: patch(:) !< Assigned patch information [1:Np].
endtype Type_BC
! Derived type containing blocks information.
type Type_Block
  integer(I4P)::                 Nb      !< Number of blocks of current input file.
  type(Type_Mesh), allocatable:: mesh(:) !< Mesh data [1:Nb].
  type(Type_BC),   allocatable:: bc(:)   !< Boundary conditions information [1:Nb].
endtype Type_Block
! Derived type containing rototranslation information.
type Type_RotoTran
  character(1)::              raxis     !< Rotation axis ('x', 'y' or 'z').
  type(Type_Vector)::         rcenter   !< Rotation center.
  real(R_P)::                 rangle    !< Rotation angle (degrees).
  type(Type_Tensor)::         rtensor   !< Rotation tensor.
  type(Type_Vector)::         tvec      !< Translation vector (x,y,z modules).
  integer(I4P)::              Nb        !< Number of blocks to rototranslate.
  integer(I4P), allocatable:: blocks(:) !< Indexes of blocks to rototranslate.
endtype Type_RotoTran
! variables
real(R8P)::                     scale_factor = 1._R8P    !< Scale factor for scaling geometry.
real(R8P)::                     delta_bl     = 1.D-3     !< Estimation of boundary layer thickness.
integer(I4P)::                  Nf     = 0               !< Number of input files.
integer(I4P)::                  Nb_tot = 0               !< Total number of blocks.
integer(I4P)::                  Np_tot = 0               !< Global number of patches (assigned faces bc).
type(Type_Block), allocatable:: blocks(:)                !< Blocks information [1:Nf].
integer(I4P)::                  Nfb     = 0              !< Number of files containing boxes.
integer(I4P)::                  Nbx_tot = 0              !< Total number of boxes.
type(Type_Block), allocatable:: boxes(:)                 !< Boxes information [1:Nbx].
integer(I4P)::                  Nca = 0                  !< Number of command line arguments.
character(2)::                  switch                   !< Command line switch string.
character(100)::                nstring                  !< String for inquiring input numbers.
character(99)::                 pathinput ="./"          !< Path of input files (*.grd, *.bcs).
character(99)::                 pathoutput = "./output/" !< Path of output files (cc.par, *.grd).
character(99)::                 ogrdfname  = "xship.grd" !< Ouptut grd file name.
integer(I4P)::                  b,c,f                    !< Counters.
integer(I4P)::                  err                      !< Error trapping flag: 0 no errors, >0 error occurs.
type(Type_OS) :: OS
integer(I4P):: scale_ratio
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! parsing command line for getting basename of input/output files
Nca = command_argument_count ()
if (Nca==0) then
  write(stderr,'(A)') ' A valid number of input files must be provided as a command line argument.'
  write(stderr,'(A)') ' Correct use are:'
  write(stderr,*)
  write(stderr,'(A)') '  1) cfdshipiowa2xnavis -n n_files [options];'
  write(stderr,'(A)') '  2) cfdshipiowa2xnavis => print this help message.'
  write(stderr,*)
  write(stderr,'(A)') ' Options:'
  write(stderr,'(A)') '  -i input_path          ; default  ./'
  write(stderr,'(A)') '  -o output_path         ; default  ./output/'
  write(stderr,'(A)') '  -g output_grd_file_name; default  xship.grd'
  write(stderr,'(A)') '  -s scale_f             ; default  1.'
  write(stderr,'(A)') '  -d delta_bl            ; default  10^-3'
  write(stderr,'(A)') '  -b n_boxes             ; default  0'
  write(stderr,'(A)') '  --for_andrea input.grd output.grd scale_ratio'
  write(stderr,*)
  stop
else
  c = 0
  do while (c<Nca)
    c = c + 1
    call get_command_argument(c,switch)
    select case(trim(adjustl(switch)))
    case('-i')
      call get_command_argument(c+1,pathinput) ; pathinput = trim(adjustl(pathinput))
      c = c + 1
    case('-o')
      call get_command_argument(c+1,pathoutput) ; pathoutput = trim(adjustl(pathoutput))
      c = c + 1
    case('-g')
      call get_command_argument(c+1,ogrdfname) ; ogrdfname = trim(adjustl(ogrdfname))
      c = c + 1
    case('-n')
      call get_command_argument(c+1,nstring) ; nstring = trim(adjustl(nstring))
      read(nstring,*)Nf
      c = c + 1
    case('-s')
      call get_command_argument(c+1,nstring) ; nstring = trim(adjustl(nstring))
      read(nstring,*)scale_factor
      c = c + 1
    case('-d')
      call get_command_argument(c+1,nstring) ; nstring = trim(adjustl(nstring))
      read(nstring,*)delta_bl
      c = c + 1
    case('-b')
      call get_command_argument(c+1,nstring) ; nstring = trim(adjustl(nstring))
      read(nstring,*)Nfb
      c = c + 1
    case('--for_andrea') ! input.grd outpu.grd scale_grid
      call get_command_argument(c+1,ogrdfname) ; ogrdfname = trim(adjustl(ogrdfname))
      c = c + 1
      call get_command_argument(c+1,pathoutput) ; pathoutput = trim(adjustl(pathoutput))
      c = c + 1
      call get_command_argument(c+1,nstring) ; nstring = trim(adjustl(nstring))
      read(nstring,*)scale_ratio
      c = c + 1
      call for_andrea
    endselect
  enddo
endif
err = OS%make_dir(directory=pathoutput)
! allocating main array of blocks information
if (allocated(blocks)) deallocate(blocks) ; allocate(blocks(1:Nf))
! loading geometry
Nb_tot = 0
do f=1,Nf ! loop over files
  call grd_iowa_reader(grdfname=trim(pathinput)//'info-'//trim(strz(3,f))//'.grd',blocks=blocks(f))
  Nb_tot = Nb_tot + blocks(f)%Nb
  ! rototranslate the geometry if necessary
  call rototranslate(filename=trim(pathinput)//'rototranslate-'//trim(strz(3,f))//'.dat',blocks=blocks(f))
enddo
! loading boxes
if (Nfb>0) then
  if (allocated(boxes)) deallocate(boxes) ; allocate(boxes(1:Nfb))
  Nbx_tot = 0
  do f=1,Nfb ! loop over files
    call grd_iowa_reader(grdfname=trim(pathinput)//'box-'//trim(strz(3,f))//'.grd',blocks=boxes(f))
    Nbx_tot = Nbx_tot + boxes(f)%Nb
    ! rototranslate the boxes if necessary
    call rototranslate(filename=trim(pathinput)//'rototranslate-box-'//trim(strz(3,f))//'.dat',blocks=boxes(f))
  enddo
  call body_dat_writer(filename=trim(pathoutput)//'body.dat')
endif
! saving xnavis grd file
call grd_xnavis_writer(grdfname=trim(pathoutput)//trim(ogrdfname),Nf=Nf,Nb_tot=Nb_tot,blocks=blocks)
! saving tecplot grd file for checking purpose
call grd_tec_writer(grdfname=trim(pathoutput)//trim(ogrdfname)//'.dat',Nf=Nf,blocks=blocks)
! deallocating uncessary variables
do f=1,Nf ! loop over files
  do b=1,blocks(f)%Nb ! loop over blocks
    if (allocated(blocks(f)%mesh(b)%nodes)) deallocate(blocks(f)%mesh(b)%nodes)
  enddo
enddo
! loading boundary conditions
Np_tot = 0
do f=1,Nf ! loop over files
  call bcs_iowa_reader(bcsfname=trim(pathinput)//'info-'//trim(strz(3,f))//'.bcs',Np_tot=Np_tot,blocks=blocks(f))
enddo
! saving cc.par
call cc_par_writer(ccparfname=trim(pathoutput)//'cc.par',ogrdfname=ogrdfname, &
                   Nf=Nf,Nb_tot=Nb_tot,Np_tot=Np_tot,blocks=blocks,Nfb=Nfb,Nbx_tot=Nbx_tot,boxes=boxes)
! deallocating variables
do f=1,Nf ! loop over files
  do b=1,blocks(f)%Nb ! loop over blocks
    if (allocated(blocks(f)%bc(b)%patch)) deallocate(blocks(f)%bc(b)%patch)
  enddo
  if (allocated(blocks(f)%mesh)) deallocate(blocks(f)%mesh)
  if (allocated(blocks(f)%bc  )) deallocate(blocks(f)%bc  )
enddo
if (allocated(blocks)) deallocate(blocks)
do f=1,Nfb ! loop over files
  do b=1,boxes(f)%Nb ! loop over blocks
    if (allocated(boxes(f)%mesh(b)%nodes)) deallocate(boxes(f)%mesh(b)%nodes)
  enddo
  if (allocated(boxes(f)%mesh)) deallocate(boxes(f)%mesh)
enddo
if (allocated(boxes)) deallocate(boxes)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine for_andrea()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Run only Andrea stuff.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(blocks)) deallocate(blocks) ; allocate(blocks(1))
  call grd_iowa_reader(grdfname=trim(pathinput)//trim(ogrdfname), blocks=blocks(1))
  call blocks(1)%mesh%scale_grid_mesh(scale_ratio=scale_ratio)
  call grd_iowa_writer(grdfname=trim(pathoutput)//trim(ogrdfname), blocks=blocks(1))
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine for_andrea

  subroutine grd_iowa_writer(grdfname, blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Subroutine for writing nodes coordinates of the mesh from grd ascii file formatted as cfdship-iowa standard.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(in)::    grdfname  !< Grd file name.
  type(Type_Block), intent(in)::    blocks    !< Blocks information.
  integer(I4P)::                    ugrd      !< Unit of grd file.
  integer(I4P)::                    i,j,k,b   !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ugrd = Get_Unit() ; open(unit=ugrd,file=trim(grdfname))
  write(ugrd,*) blocks%Nb
  do b=1,blocks%Nb ! loop over blocks
    write(ugrd,*) blocks%mesh(b)%Ni,blocks%mesh(b)%Nj,blocks%mesh(b)%Nk
  enddo
  do b=1,blocks%Nb ! loop over blocks
    write(ugrd,*) (((blocks%mesh(b)%nodes(i, j, k)%x,i=1,blocks%mesh(b)%Ni), j=1, blocks%mesh(b)%Nj), k=1, blocks%mesh(b)%Nk),&
                  (((blocks%mesh(b)%nodes(i, j, k)%y,i=1,blocks%mesh(b)%Ni), j=1, blocks%mesh(b)%Nj), k=1, blocks%mesh(b)%Nk),&
                  (((blocks%mesh(b)%nodes(i, j, k)%z,i=1,blocks%mesh(b)%Ni), j=1, blocks%mesh(b)%Nj), k=1, blocks%mesh(b)%Nk)
  enddo
  close(ugrd)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine grd_iowa_writer

  !> @brief Subroutine for reading nodes coordinates of the mesh from grd ascii file formatted as cfdship-iowa standard.
  subroutine grd_iowa_reader(grdfname,blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN)::    grdfname  !< Grd file name.
  type(Type_Block), intent(INOUT):: blocks    !< Blocks information.
  type(Type_Vector), allocatable::  nodes(:)  !< Temporary array for loading nodes coordinates.
  integer(I4P)::                    ugrd      !< Unit of grd file.
  integer(I4P)::                    Ni,Nj,Nk  !< Temporary extents counters.
  integer(I4P)::                    i,j,k,b,c !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ugrd = Get_Unit() ; open(unit=ugrd,file=trim(grdfname))
  read(ugrd,*) blocks%Nb
  if (allocated(blocks%mesh)) deallocate(blocks%mesh) ; allocate(blocks%mesh(1:blocks%Nb))
  do b=1,blocks%Nb ! loop over blocks
    read(ugrd,*) blocks%mesh(b)%Ni,blocks%mesh(b)%Nj,blocks%mesh(b)%Nk
    Ni = blocks%mesh(b)%Ni
    Nj = blocks%mesh(b)%Nj
    Nk = blocks%mesh(b)%Nk
    if (allocated(blocks%mesh(b)%nodes)) deallocate(blocks%mesh(b)%nodes);allocate(blocks%mesh(b)%nodes(1:Ni,1:Nj,1:Nk))
  enddo
  do b=1,blocks%Nb ! loop over blocks
    Ni = blocks%mesh(b)%Ni
    Nj = blocks%mesh(b)%Nj
    Nk = blocks%mesh(b)%Nk
    if (allocated(nodes)) deallocate(nodes) ; allocate(nodes(1:Ni*Nj*Nk))
    read(ugrd,*) (nodes(c)%x,c=1,Ni*Nj*Nk),(nodes(c)%y,c=1,Ni*Nj*Nk),(nodes(c)%z,c=1,Ni*Nj*Nk)
    do c=1,Ni*Nj*Nk
      k = (c - 1)/(Ni*Nj) + 1
      j = (c - (k - 1)*(Ni*Nj) - 1)/Ni + 1
      i = (c - (k - 1)*(Ni*Nj) - (j-1)*Ni - 1) + 1
      blocks%mesh(b)%nodes(i,j,k) = nodes(c)
    enddo
  enddo
  close(ugrd)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine grd_iowa_reader

  !> @brief Subroutine for writing nodes coordinates of the mesh into grd binary file formatted as xnavis standard.
  subroutine grd_xnavis_writer(grdfname,Nf,Nb_tot,blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN):: grdfname     !< Grd file name.
  integer(I4P),     intent(IN):: Nf           !< Number of files.
  integer(I4P),     intent(IN):: Nb_tot       !< Total number of blocks.
  type(Type_Block), intent(IN):: blocks(1:Nf) !< Blocks information.
  integer(I4P)::                 ugrd         !< Unit of grd file.
  integer(I4P)::                 Ni,Nj,Nk     !< Temporary extents counters.
  integer(I4P)::                 i,j,k,b,f    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ugrd = Get_Unit() ; open(unit=ugrd,file=trim(grdfname),form='UNFORMATTED')
  write(ugrd) Nb_tot
  do f=1,Nf ! loop over files
    do b=1,blocks(f)%Nb ! loop over blocks
      write(ugrd) blocks(f)%mesh(b)%Ni-1,blocks(f)%mesh(b)%Nj-1,blocks(f)%mesh(b)%Nk-1
    enddo
  enddo
  do f=1,Nf ! loop over files
    do b=1,blocks(f)%Nb ! loop over blocks
      Ni = blocks(f)%mesh(b)%Ni
      Nj = blocks(f)%mesh(b)%Nj
      Nk = blocks(f)%mesh(b)%Nk
      write(ugrd)(((blocks(f)%mesh(b)%nodes(i,j,k)%x/scale_factor,i=1,Ni),j=1,Nj),k=1,Nk)
      write(ugrd)(((blocks(f)%mesh(b)%nodes(i,j,k)%y/scale_factor,i=1,Ni),j=1,Nj),k=1,Nk)
      write(ugrd)(((blocks(f)%mesh(b)%nodes(i,j,k)%z/scale_factor,i=1,Ni),j=1,Nj),k=1,Nk)
    enddo
  enddo
  close(ugrd)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine grd_xnavis_writer

  !> @brief Subroutine for writing body.dat file from boxes ones.
  subroutine body_dat_writer(filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN):: filename                      !< Output file name of body.dat.
  integer(I4P)::                 ubody                         !< Free unit for body.dat file.
  integer(I4P)::                 b,bb                          !< Boxes counters.
  integer(I4P)::                 i,j,k                         !< Space counters.
  integer(I4P), parameter::      conn(1:8) = [1,2,4,3,5,6,8,7] !< "False" connectivity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(unit=Get_Unit(ubody), file=trim(filename))
  write(ubody,'(A)')trim(str(.true.,Nbx_tot*8))
  do b=1,Nfb
    do bb=1,boxes(b)%Nb
      do k=1,boxes(b)%mesh(bb)%Nk
        do j=1,boxes(b)%mesh(bb)%Nj
          do i=1,boxes(b)%mesh(bb)%Ni
              write(ubody,'(3('//FR8P//',1X))')boxes(b)%mesh(bb)%nodes(i,j,k)%x/scale_factor,&
                                               boxes(b)%mesh(bb)%nodes(i,j,k)%y/scale_factor,&
                                               boxes(b)%mesh(bb)%nodes(i,j,k)%z/scale_factor
          enddo
        enddo
      enddo
    enddo
  enddo
  write(ubody,'(A)')trim(str(.true.,Nbx_tot))
  i = 0
  do b=1,Nfb
    do bb=1,boxes(b)%Nb
      i = i + 1
      write(ubody,'(A,8('//FI4P//'))')'1 ',(conn(j)+8*(i-1),j=1,8)
    enddo
  enddo

  close(ubody)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine body_dat_writer

  !> @brief Subroutine for writing nodes coordinates of the mesh into grd binary file formatted as xnavis standard.
  subroutine grd_tec_writer(grdfname,Nf,blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN):: grdfname     !< Grd file name.
  integer(I4P),     intent(IN):: Nf           !< Number of files.
  type(Type_Block), intent(IN):: blocks(1:Nf) !< Blocks information.
  integer(I4P)::                 utec         !< Unit of tecplot file.
  integer(I4P)::                 i,j,k,b,f    !< Counters.
  integer(I4P)::                 b_pre        !< Previous number of blocks.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  utec = Get_Unit() ; open(unit=utec,file=trim(grdfname))
  write(utec,*) ' VARIABLES= "x", "y", "z"'
  b_pre = 0
  do f=1,Nf ! loop over files
    do b=1,blocks(f)%Nb ! loop over blocks
      write(utec,*) ' ZONE  T="',b+b_pre,'" I=',blocks(f)%mesh(b)%Ni,' J= ',blocks(f)%mesh(b)%Nj,'K= ',blocks(f)%mesh(b)%Nk
      do k=1,blocks(f)%mesh(b)%Nk
        do j=1,blocks(f)%mesh(b)%Nj
          do i=1,blocks(f)%mesh(b)%Ni
            write(utec,'(3('//FR8P//',1X))') blocks(f)%mesh(b)%nodes(i,j,k)%x, &
                                             blocks(f)%mesh(b)%nodes(i,j,k)%y, &
                                             blocks(f)%mesh(b)%nodes(i,j,k)%z
          enddo
        enddo
      enddo
      b_pre = b_pre + blocks(f)%Nb
    enddo
  enddo
  close(utec)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine grd_tec_writer

  !> @brief Subroutine for reading boundary conditions of the mesh from bcs ascii file formatted as cfdship-iowa standard.
  subroutine bcs_iowa_reader(bcsfname,Np_tot,blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN)::    bcsfname !< Grd file name.
  integer(I4P),     intent(INOUT):: Np_tot   !< Total number of patches.
  type(Type_Block), intent(INOUT):: blocks   !< Blocks information.
  integer(I4P)::                    ubcs     !< Unit of bcs file.
  integer(I4P)::                    b,c,p    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(blocks%bc)) deallocate(blocks%bc) ; allocate(blocks%bc(1:blocks%Nb))
  ubcs = Get_Unit() ; open(unit=ubcs,file=trim(bcsfname))
  do b=1,blocks%Nb ! loop over blocks
    read(ubcs,*) blocks%bc(b)%Np
    if (allocated(blocks%bc(b)%patch)) deallocate(blocks%bc(b)%patch) ; allocate(blocks%bc(b)%patch(1:blocks%bc(b)%Np))
    call type_patch_init(blocks%bc(b)%patch)
    do p=1,blocks%bc(b)%Np ! loop over patches
      blocks%bc(b)%patch(p)%p = Np_tot + p
      read(ubcs,*) blocks%bc(b)%patch(p)%tp
      read(ubcs,*) blocks%bc(b)%patch(p)%norm
      read(ubcs,*) blocks%bc(b)%patch(p)%i1,blocks%bc(b)%patch(p)%i2
      read(ubcs,*) blocks%bc(b)%patch(p)%j1,blocks%bc(b)%patch(p)%j2
      read(ubcs,*) blocks%bc(b)%patch(p)%k1,blocks%bc(b)%patch(p)%k2
      read(ubcs,*) ! skipped record
      if (blocks%bc(b)%patch(p)%tp==92) then ! patched (adjacent bc) face
        read(ubcs,*) blocks%bc(b)%patch(p)%b_adj
        read(ubcs,*) blocks%bc(b)%patch(p)%norm_adj
        read(ubcs,*) blocks%bc(b)%patch(p)%i1_adj,blocks%bc(b)%patch(p)%i2_adj
        read(ubcs,*) blocks%bc(b)%patch(p)%j1_adj,blocks%bc(b)%patch(p)%j2_adj
        read(ubcs,*) blocks%bc(b)%patch(p)%k1_adj,blocks%bc(b)%patch(p)%k2_adj
        blocks%bc(b)%patch(p)%is_adj = sign(1,blocks%bc(b)%patch(p)%i2_adj - blocks%bc(b)%patch(p)%i1_adj)
        blocks%bc(b)%patch(p)%js_adj = sign(1,blocks%bc(b)%patch(p)%j2_adj - blocks%bc(b)%patch(p)%j1_adj)
        blocks%bc(b)%patch(p)%ks_adj = sign(1,blocks%bc(b)%patch(p)%k2_adj - blocks%bc(b)%patch(p)%k1_adj)
        call connect_iowa2xnavis(patch=blocks%bc(b)%patch(p))
        select case(blocks%bc(b)%patch(p)%norm_adj)
        case(-1)
          blocks%bc(b)%patch(p)%norm_adj = 2
        case(-2)
          blocks%bc(b)%patch(p)%norm_adj = 4
        case(-3)
          blocks%bc(b)%patch(p)%norm_adj = 6
        case(2)
          blocks%bc(b)%patch(p)%norm_adj = 3
        case(3)
          blocks%bc(b)%patch(p)%norm_adj = 5
        endselect
      endif
      select case(blocks%bc(b)%patch(p)%norm)
      case(-1)
        blocks%bc(b)%patch(p)%norm = 2
      case(-2)
        blocks%bc(b)%patch(p)%norm = 4
      case(-3)
        blocks%bc(b)%patch(p)%norm = 6
      case(2)
        blocks%bc(b)%patch(p)%norm = 3
      case(3)
        blocks%bc(b)%patch(p)%norm = 5
      endselect
    enddo
    Np_tot = Np_tot + blocks%bc(b)%Np
    ! sorting patches
    !call bubble_sort(b=bc(b)%Np,x=bc(b)%patch(1:bc(b)%Np))
  enddo
  close(ubcs)
  ! correcting patch indexes
  do b=1,blocks%Nb ! loop over blocks
    do p=1,blocks%bc(b)%Np ! loop over patches
      if (blocks%bc(b)%patch(p)%tp>=100) then
        do c=1,blocks%bc(blocks%bc(b)%patch(p)%b_adj)%Np
          if ((blocks%bc(blocks%bc(b)%patch(p)%b_adj)%patch(c)%norm==blocks%bc(b)%patch(p)%norm_adj).and. &
              (blocks%bc(blocks%bc(b)%patch(p)%b_adj)%patch(c)%b_adj==b)) then
            blocks%bc(b)%patch(p)%p_adj = blocks%bc(blocks%bc(b)%patch(p)%b_adj)%patch(c)%p
            exit
          endif
        enddo
      endif
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine bcs_iowa_reader

  !> @brief Subroutine for converting patches connectivity from cfdship-iowa to xnavis standard.
  subroutine connect_iowa2xnavis(patch)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Patch), intent(INOUT)::        patch                      !< Patch information.
  character(1)::                           or(1:3),or2(1:2)           !< Temporary strings for converting connectivity formats.
  character(3), dimension(24), parameter:: ijk=['236','253','245', &  !< List of right-hand oriented refirements.
                                                '264','415','452', &
                                                '426','461','614', &
                                                '631','623','642', &
                                                '135','154','146', &
                                                '163','316','351', &
                                                '325','362','513', &
                                                '532','524','541']
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(patch%norm)
  case(-1,1) ! i face of reference block
    select case(patch%norm_adj)
    case(-1,1) ! i face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(1) = '1'
      else
        or(1) = '2'
      endif
      ! checking the sign of other two directions
      if (patch%js_adj>0) then
        or2(1) = '3'
      else
        or2(1) = '4'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or(1)//or2(1)//or2(2))>0) then
        or(2) = or2(1)
        or(3) = or2(2)
      else
        or(2) = or2(2)
        or(3) = or2(1)
      endif
    case(-2,2) ! j face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(1) = '3'
      else
        or(1) = '4'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or(1)//or2(1)//or2(2))>0) then
        or(2) = or2(1)
        or(3) = or2(2)
      else
        or(2) = or2(2)
        or(3) = or2(1)
      endif
    case(-3,3) ! k face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(1) = '5'
      else
        or(1) = '6'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%js_adj>0) then
        or2(2) = '3'
      else
        or2(2) = '4'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or(1)//or2(1)//or2(2))>0) then
        or(2) = or2(1)
        or(3) = or2(2)
      else
        or(2) = or2(2)
        or(3) = or2(1)
      endif
    endselect
  case(-2,2) ! j face of reference block
    select case(patch%norm_adj)
    case(-1,1) ! i face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(2) = '1'
      else
        or(2) = '2'
      endif
      ! checking the sign of other two directions
      if (patch%js_adj>0) then
        or2(1) = '3'
      else
        or2(1) = '4'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or(2)//or2(2))>0) then
        or(1) = or2(1)
        or(3) = or2(2)
      else
        or(1) = or2(2)
        or(3) = or2(1)
      endif
    case(-2,2) ! j face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(2) = '3'
      else
        or(2) = '4'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or(2)//or2(2))>0) then
        or(1) = or2(1)
        or(3) = or2(2)
      else
        or(1) = or2(2)
        or(3) = or2(1)
      endif
    case(-3,3) ! k face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(2) = '5'
      else
        or(2) = '6'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%js_adj>0) then
        or2(2) = '3'
      else
        or2(2) = '4'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or(2)//or2(2))>0) then
        or(1) = or2(1)
        or(3) = or2(2)
      else
        or(1) = or2(2)
        or(3) = or2(1)
      endif
    endselect
  case(-3,3) ! k face of reference block
    select case(patch%norm_adj)
    case(-1,1) ! i face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(3) = '1'
      else
        or(3) = '2'
      endif
      ! checking the sign of other two directions
      if (patch%js_adj>0) then
        or2(1) = '3'
      else
        or2(1) = '4'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or2(2)//or(3))>0) then
        or(1) = or2(1)
        or(2) = or2(2)
      else
        or(1) = or2(2)
        or(2) = or2(1)
      endif
    case(-2,2) ! j face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(3) = '3'
      else
        or(3) = '4'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%ks_adj>0) then
        or2(2) = '5'
      else
        or2(2) = '6'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or2(2)//or(3))>0) then
        or(1) = or2(1)
        or(2) = or2(2)
      else
        or(1) = or2(2)
        or(2) = or2(1)
      endif
    case(-3,3) ! k face of adjacent block
      if (patch%norm*patch%norm_adj<0) then
        or(3) = '5'
      else
        or(3) = '6'
      endif
      ! checking the sign of other two directions
      if (patch%is_adj>0) then
        or2(1) = '1'
      else
        or2(1) = '2'
      endif
      if (patch%js_adj>0) then
        or2(2) = '3'
      else
        or2(2) = '4'
      endif
      ! selecting the right-hand oriented referement
      if (count(ijk==or2(1)//or2(2)//or(3))>0) then
        or(1) = or2(1)
        or(2) = or2(2)
      else
        or(1) = or2(2)
        or(2) = or2(1)
      endif
    endselect
  endselect
  patch%tp = cton(str=or(1)//or(2)//or(3), knd=1_I4P)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine connect_iowa2xnavis

  !> @brief Subroutine for writing boundary conditions of the mesh into ascii file formatted as xnavis standard (cc.par).
  subroutine cc_par_writer(ccparfname,ogrdfname,Nf,Nb_tot,Np_tot,blocks,Nfb,Nbx_tot,boxes)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),                  intent(IN):: ccparfname    !< cc.par file name.
  character(*),                  intent(IN):: ogrdfname     !< Output grd (xnavis format) file name.
  integer(I4P),                  intent(IN):: Nf            !< Number of files.
  integer(I4P),                  intent(IN):: Nb_tot        !< Total number of blocks.
  integer(I4P),                  intent(IN):: Np_tot        !< Total number of patches.
  type(Type_Block),              intent(IN):: blocks(1:Nf)  !< Blocks information.
  integer(I4P),                  intent(IN):: Nfb           !< Number of files containing boxes.
  integer(I4P),                  intent(IN):: Nbx_tot       !< Total Number of boxes.
  type(Type_Block), allocatable, intent(IN):: boxes(:)      !< Boxes information.
  integer(I4P)::                              uccpar        !< Unit of cc.par file.
  integer(I4P)::                              i,j,k,b,f,p   !< Counters.
  integer(I4P)::                              b_pre         !< Previous number of blocks.
  type Type_LGP
    integer(I4P)::  lgp(1:3) !< Level, group and priority of blocks.
    character(60):: name     !< Comment name associated to each block.
  endtype Type_LGP
  type(Type_LGP):: lgpmap(1:Nb_tot) !< Level, group and priority of blocks.
  logical::        is_lgpmap        !< Flag for inquiring the presence of file.
  integer(I_P)::   err              !< Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  inquire(file=trim(pathinput)//'blk-lgp-map.dat',exist=is_lgpmap,iostat=err)
  if (is_lgpmap) then
    open(unit=Get_Unit(uccpar),file=trim(pathinput)//'blk-lgp-map.dat')
    read(uccpar,*) err
    if (err /= Nb_tot) then
      write(stderr, '(A)') ' Number of blocks of blk-lgp-map.dat not matching the geometry files'
      write(stderr, '(A)') trim(str(.true.,err))//' /= '//trim(str(.true.,Nb_tot))
      close(uccpar)
      is_lgpmap = .false.
    else
      write(stdout,'(A)')' Find blk-lgp-map file '//trim(pathinput)//'blk-lgp-map.dat'
      read(uccpar,*)
      do b=1,Nb_tot
        read(uccpar,*)lgpmap(b)%lgp(1),lgpmap(b)%lgp(2),lgpmap(b)%lgp(3),lgpmap(b)%name
      enddo
      close(uccpar)
    endif
  endif
  open(unit=Get_Unit(uccpar),file=trim(ccparfname))
  write(uccpar,'(A)') "'"//trim(ogrdfname)//"' ! nome file reticolo overset"
  write(uccpar,'(A)') "'cc.'   ! nome file output"
  write(uccpar,'(A)') ".true.  ! scrittura reticoli con cornici"
  write(uccpar,'(A)') ".true.  ! aumenta la sovrapposizione tra reticoli"
  write(uccpar,'(A)') ".false. ! estensione pareti interne a celle chimera di contorno"
  write(uccpar,*)
  write(uccpar,'(A)') "4 1 ! numero livelli multigrid"
  write(uccpar,*)
  write(uccpar,'(A)') trim(str(.true.,delta_bl))//" ! Stima Boundary Layer"
  write(uccpar,'(A)') "-1.d0 ! Ampiezza spiaggia numerica"
  write(uccpar,*)
  write(uccpar,'(A)') trim(str(.true.,Nb_tot))//" ! numero blocchi"
  write(uccpar,*)
  if (is_lgpmap) then
    do b=1,Nb_tot
      write(uccpar,'(A)')trim(str(.true.,lgpmap(b)%lgp(1)))//' '//&
                         trim(str(.true.,lgpmap(b)%lgp(2)))//' '//&
                         trim(str(.true.,lgpmap(b)%lgp(3)))//' ! '//&
                         trim(str(.true.,b))//' '//lgpmap(b)%name
    enddo
  else
    b_pre = 0
    do f=1,Nf ! loop over files
      write(uccpar,'(3I3,A)') f,0,1," ! "//trim(str(.true.,b_pre+1))//": (livello gruppo priorita)"
      if (blocks(f)%Nb>=2) then
        do b=2,blocks(f)%Nb
          write(uccpar,'(3I3,A)') f,0,1," ! "//trim(str(.true.,b_pre+b))
        enddo
        b_pre = b_pre + blocks(f)%Nb
      endif
    enddo
  endif
  write(uccpar,*)
  write(uccpar,'(A)') trim(str(.true.,Np_tot))//" ! numero di patches"
  write(uccpar,*)
  b_pre = 0
  do f=1,Nf ! loop over files
    do b=1,blocks(f)%Nb ! loop over blocks
      do p=1,blocks(f)%bc(b)%Np ! loop over patches
        write(uccpar,'(I7,I3,I5,I9,6I6,A)') b_pre+b,                                                     &
                                            blocks(f)%bc(b)%patch(p)%norm,                               &
                                            blocks(f)%bc(b)%patch(p)%tp,                                 &
                                            blocks(f)%bc(b)%patch(p)%p_adj,                              &
                                            blocks(f)%bc(b)%patch(p)%i1-1,blocks(f)%bc(b)%patch(p)%i2-1, &
                                            blocks(f)%bc(b)%patch(p)%j1-1,blocks(f)%bc(b)%patch(p)%j2-1, &
                                            blocks(f)%bc(b)%patch(p)%k1-1,blocks(f)%bc(b)%patch(p)%k2-1, &
                                            ' ! '//trim(str(.true.,blocks(f)%bc(b)%patch(p)%p))
      enddo
    enddo
    b_pre = b_pre + blocks(f)%Nb
  enddo
  write(uccpar,*)
  write(uccpar,'(A)') "0 ! numero di edge di parete"
  write(uccpar,*)
  if (Nfb>0) then
    write(uccpar,'(A)') trim(str(.true.,Nbx_tot))//" ! numero di scatole"
    write(uccpar,*)
    do f=1,Nfb ! loop over files
      do b=1,boxes(f)%Nb ! loop over blocks
        write(uccpar,'(A)') "9 "//trim(str(.true.,f))//" ! tipo, blocco"
        do k=1,boxes(f)%mesh(b)%Nk
          do j=1,boxes(f)%mesh(b)%Nj
            do i=1,boxes(f)%mesh(b)%Ni
              write(uccpar,'(3('//FR8P//',1X))')boxes(f)%mesh(b)%nodes(i,j,k)%x/scale_factor,&
                                                boxes(f)%mesh(b)%nodes(i,j,k)%y/scale_factor,&
                                                boxes(f)%mesh(b)%nodes(i,j,k)%z/scale_factor
            enddo
          enddo
        enddo
      enddo
    enddo
  else
    write(uccpar,'(A)') "0 ! numero di scatole"
  endif
  write(uccpar,*)
  write(uccpar,'(A)') "0 ! numero circuiti chiusi"
  close(uccpar)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine cc_par_writer

  !> @brief Subroutine for initializing the components of Type_Patch array.
  subroutine type_patch_init (x)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Patch), intent(INOUT):: x(:) !< Array of patches to be initialized.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  x%p = 0
  x%tp = 0
  x%norm = 0
  x%i1 = 0 ; x%i2 = 0
  x%j1 = 0 ; x%j2 = 0
  x%k1 = 0 ; x%k2 = 0
  x%b_adj = 0
  x%p_adj = 0
  x%norm_adj = 0
  x%i1_adj = 0 ; x%i2_adj = 0 ; x%is_adj = 0
  x%j1_adj = 0 ; x%j2_adj = 0 ; x%js_adj = 0
  x%k1_adj = 0 ; x%k2_adj = 0 ; x%ks_adj = 0
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine type_patch_init

  !> @brief Subroutine for sorting the pacthes by means of the face order.
  subroutine bubble_sort (n,x)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),     intent(IN)::    n
  type(Type_Patch), intent(INOUT):: x(n)
  type(Type_Patch)::                temp
  integer(I4P)::                    j,i,jmax
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  jmax=n-1
  do i=1,n-1
    do j=1,jmax
      if(x(j)%norm<x(j+1)%norm) exit
      temp   = x(j)
      x(j)   = x(j+1)
      x(j+1) = temp
    enddo
    jmax = jmax-1
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine bubble_sort

  !> @brief Subroutine for rotate & translate geometry
  subroutine rototranslate(filename,blocks)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),     intent(IN)::     filename     !< File name of rototranslations information.
  type(Type_Block), intent(INOUT)::  blocks       !< Blocks information.
  integer(I4P)::                     Nrott        !< Number of rototranslations.
  type(Type_RotoTran), allocatable:: rototran(:)  !< Rototranslations information.
  type(Type_Vector)::                node         !< Temporary node coordinates.
  integer(I4P)::                     urott        !< Unit of rototranslate file.
  integer(I4P)::                     i,j,k,b,r,bi !< Counters.
  logical::                          is_file      !< Flag for inquiring the presence of file.
  integer(I_P)::                     err          !< Error traping flag: 0 no errors, >0 error occours.
  character(60)::                    strlist      !< Temporary string for loading the list of blocks to rototranslate.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  inquire(file=trim(filename),exist=is_file,iostat=err)
  if (is_file) then
    write(stdout,'(A)') ' Find rototranslate file '//trim(filename)
    urott = Get_Unit() ; open(unit=urott,file=trim(filename),action='READ')
    read(urott,*) Nrott
    if (allocated(rototran)) deallocate(rototran) ; allocate(rototran(1:Nrott))
    do r=1,Nrott                                                                      ! number of rototranslations
      read(urott,*)
      read(urott,*) rototran(r)%raxis                                                 ! rotation axis ('x', 'y' or 'z')
      read(urott,*) rototran(r)%rcenter%x,rototran(r)%rcenter%y,rototran(r)%rcenter%z ! rotation center (x,y,z coordinates)
      read(urott,*) rototran(r)%rangle                                                ! rotation angle (degrees)
      read(urott,*) rototran(r)%tvec%x,rototran(r)%tvec%y,rototran(r)%tvec%z          ! translation vector (x,y,z modules)
      read(urott,*) rototran(r)%Nb                                                    ! number of blocks to rototranslate
      read(urott,*) strlist                                                           ! list of blocks to rototranslate
      if (allocated(rototran(r)%blocks)) deallocate(rototran(r)%blocks) ; allocate(rototran(r)%blocks(1:rototran(r)%Nb))
      rototran(r)%blocks = 0
      call get_blocks_list(strlist=trim(adjustl(strlist)),Nb=rototran(r)%Nb,list=rototran(r)%blocks)
      !do b=1,rototran(r)%Nb
      !  read(urott,*) rototran(r)%blocks(b)                                           ! indexes of blocks to rototranslate
      !enddo
      rototran(r)%rangle = rototran(r)%rangle*atan(1._R_P)/45._R_P ! degrees to radians
      ! computing rotation tensor
      select case(rototran(r)%raxis)
      case('x')
        call rototran(r)%rtensor%rotox(ang=rototran(r)%rangle)
      case('y')
        call rototran(r)%rtensor%rotoy(ang=rototran(r)%rangle)
      case('z')
        call rototran(r)%rtensor%rotoz(ang=rototran(r)%rangle)
      case default
        write(stderr,'(A)') ' Rotation axis unknow! Valid options are: "x", "y" or "z"'
        stop
      endselect
    enddo
    close(urott)
    do r=1,Nrott ! loop over rototranslations
      do b=1,rototran(r)%Nb ! loop over blocks
        bi = rototran(r)%blocks(b)
        do k=1,blocks%mesh(bi)%Nk
          do j=1,blocks%mesh(bi)%Nj
            do i=1,blocks%mesh(bi)%Ni
              ! rotation
              node = rototran(r)%rcenter + (rototran(r)%rtensor.dot.(blocks%mesh(bi)%nodes(i,j,k)-rototran(r)%rcenter))
              ! rotation + traslation
              blocks%mesh(bi)%nodes(i,j,k) = node + rototran(r)%tvec
            enddo
          enddo
        enddo
      enddo
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine rototranslate

  !> @brief Subroutine for parsing string in order to obtain the list of blocks to be rototranslate.
  subroutine get_blocks_list(strlist,Nb,list)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=*), intent(IN)::             strlist        !< String containing the list.
  integer(I4P),     intent(IN)::             Nb             !< Number of blocks.
  integer(I4P),     intent(INOUT)::          list(1:Nb)     !< List of blocks to rototranslate.
  integer(I4P)::                             Nt             !< Number of tokens.
  character(len=len(strlist)), allocatable:: token(:)       !< Tokens [1:Nt].
  character(len=len(strlist))::              sublist        !< Temporary string.
  integer(I4P)::                             b,b1,b2,ba,c,t !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! computing the number of tokens
  Nt = 1
  do c=1,len_trim(strlist) ! loop over string characters
    if (strlist(c:c)==',') Nt = Nt + 1
  enddo
  if (allocated(token)) deallocate(token) ; allocate(token(1:Nt))
  ! parsing tokens
  sublist = trim(adjustl(strlist))
  do t=1,Nt ! loop over tokens
    c = index(sublist,',')
    if (c>0) then
      token(t) = sublist(1:c-1)
      sublist = sublist(c+1:)
    else
      token(t) = trim(adjustl(sublist))
    endif
  enddo
  ! computing the list
  ba = 0 ! initilize list element counter
  do t =1,Nt ! loop over tokens
    sublist = trim(adjustl(token(t)))
    c = index(sublist,'-')
    if (c>0) then
      b1 = cton(str=sublist(1:c-1), knd=1_I4P)
      sublist = sublist(c+1:)
      b2 = cton(str=sublist, knd=1_I4P)
      do b=b1,b2
        ba = ba + 1 ! update list element counter
        if (ba>Nb) then
          write(stderr,'(A)') ' Attention: the number of blocks to rototranslate is higher than the one declared!'
          write(stderr,'(A)') ' Check the blocks list'
          write(stderr,'(A)') ' Number of blocks declared: '//trim(str(.true.,Nb))
          write(stderr,'(A)') ' Blocks list: '//trim(adjustl(strlist))
          stop
        endif
        list(ba) = b ! storing block index into the list
      enddo
    else
      ba = ba + 1 ! update list element counter
        if (ba>Nb) then
          write(stderr,'(A)') ' Attention: the number of blocks to rototranslate is higher than the one declared!'
          write(stderr,'(A)') ' Check the blocks list'
          write(stderr,'(A)') ' Number of blocks declared: '//trim(str(.true.,Nb))
          write(stderr,'(A)') ' Blocks list: '//trim(adjustl(strlist))
          stop
        endif
      list(ba) = cton(str=sublist, knd=1_I4P) ! storing block index into the list
    endif
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_blocks_list
endprogram cfdshipiowa2xnavis
