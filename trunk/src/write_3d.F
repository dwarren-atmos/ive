#define ive_ptr integer*8

      subroutine write_3d(varpt, nx, ny, nz, nt, error)

      use lib_vtk_io
      use, intrinsic::ieee_arithmetic
      include 'message.icl'
      
      ive_ptr:: varpt
      integer:: nx, ny, nz, nt
      logical:: error
      
      
!     Local valiable declarations.
!
      ive_ptr::           slicer3d
      integer::           ni, nj, nk, nter
      integer::           fix(4), nteri, nterk, i, j, k, l, m, imap
      integer::           width, height, npts, ibeg, iend, sint, dims
      integer::           buttons, strbeg, strend, nblank
      logical::           phys, lval, linlog
      ive_ptr::           volume, topohghts, terrain
      double precision::  slope(4), intercept(4)
      real::              mins(4), maxs(4),  stag(4), delta(4), pt(4)
      real::              EPSILON, SPECIAL
      real,pointer::      v(:)
      real,allocatable::  xarr(:,:,:), yarr(:,:,:), zarr(:,:,:)
      character*1024::    filename, varname
      logical first3dvar
      parameter (EPSILON = 1.0E-5)
      common / write3ddata / first3dvar, filename

!filename already set in common block
!      call ive_create_3dfile(error)
      call getavar('field', varname, error)
      call catnull(varname, len(varname), error)
      
!  get ive vars and flags
      call getivar('use_buttons', buttons, error)
      if(buttons .eq. 0) then
         call make_help_widget(
     &        '3d volumes must be orthagonal, and selected with '
     &//'the slicer widget')
         return
      endif
      call getivar('linlog3d', linlog, error)
      call getiarr('lock', fix, 4, error)
      call getrarr('plwmin_scaled',mins, 4, error)
      call getrarr('plwmax_scaled',maxs, 4, error)
      call getdarr('domain_slope', slope, 4, error)
      call getdarr('domain_intercept', intercept, 4, error)
      i=4
      call getrarr('stag',stag, 4, error)
      /*Get Volume*/
      pt(1)=0.0
      pt(2)=0.0
      pt(3)=0.0
      pt(4)=0.0
      call getivar('num_dims', dims, error)
      call getrvar('spval', special, error)
      call getrvar('sint', sint, error)
      j = 0
      do i=1,4
         if(fix(i) .ne. 0)j=i
      enddo
      select case (j)
      case(1)
         call getrvar('xloc', pt(1), error)
      case(2)
         call getrvar('yloc', pt(2), error)
      case(3)
         call getrvar('zloc', pt(3), error)
      case(4)
         call getrvar('tloc', pt(4), error)
      end select
      phys=0
      imap=2
      volume = slicer3d(varpt, nx, ny, nz, nt, dims, j, pt,
     &                  nk, nj, ni, special, phys)

      if(first3dvar) then
         allocate(xarr(ni,nj,nk))
         allocate(yarr(ni,nj,nk))
         allocate(zarr(ni,nj,nk))
         do k=1,nk
            do j=1,nj
               do i=1,ni
                  call cpmpxyz(imap,real(i-1),real(j-1),real(k-1),
     &                 xarr(i,j,k),yarr(i,j,k),
     &                 zarr(i,j,k),special)
                  if(xarr(i,j,k) .ne. special) then
                     call scale(xarr(i,j,k), 1, slope(1), intercept(1),
     &                    special)
                     call scale(yarr(i,j,k), 1, slope(2), intercept(2),
     &                    special)
                     call scale(zarr(i,j,k), 1, slope(3), intercept(3),
     &                    special)
                     if(ieee_is_nan(xarr(i,j,k)))xarr(i,j,k)=0.0
                     if(ABS(xarr(i,j,k)) .lt. EPSILON)xarr(i,j,k)=0.0
                     if(ieee_is_nan(yarr(i,j,k)))yarr(i,j,k)=0.0
                     if(ABS(yarr(i,j,k)) .lt. EPSILON)yarr(i,j,k)=0.0
                     if(ieee_is_nan(zarr(i,j,k)))zarr(i,j,k)=0.0
                     if(ABS(zarr(i,j,k)) .lt. EPSILON)zarr(i,j,k)=0.0
                  endif
               enddo
            enddo
         enddo
         call ive_prep_3dfile(ni*nj*nk, ni, nj, nk,
     &        error, reshape((xarr),(/ni*nj*nk/)),
     &        reshape((yarr),(/ni*nj*nk/)),
     &        reshape((zarr),(/ni*nj*nk/)))
         first3dvar=.false.
      endif
      call ive_write_3dvar(ni*nj*nk, %val(volume), trim(varname), error)
      if(volume .ne. 0)call freemem(volume);

      return
      end subroutine write_3d

      subroutine ive_close_3dfile(error)
      use lib_vtk_io
      logical error
      error = VTK_END()
      end
      
      subroutine ive_create_3dfile(error)
      use lib_vtk_io
      logical error
      character*1024::    filename
      logical first3dvar
      common / write3ddata / first3dvar, filename
      write(6,*)'create ',trim(filename)
      error = VTK_INI('ASCII',trim(filename),
     &     'VTK legacy file','STRUCTURED_GRID')
      first3dvar=.true.
      end subroutine ive_create_3dfile

      subroutine ive_prep_3dfile(size, i, j, k, error, x, y, z)
      use lib_vtk_io
      integer size, i, j, k, l
      logical error
      real:: x(size), y(size), z(size)
      do l=1,size
      enddo
      error = VTK_GEO(i,j,k,size,x, y, z)
      error = VTK_DAT(size,'node')
      end subroutine ive_prep_3dfile

      subroutine ive_write_3dvar(size, var, varname, error)
      use lib_vtk_io
      use, intrinsic::ieee_arithmetic

      integer size
      real:: var(size)
      character*(*) :: varname
      logical error
      integer i
      real EPSILON
      parameter (EPSILON = 1.0E-5)

      do i=1,size
         if(ieee_is_nan(var(i)))var(i)=0.0
         if(.not. ieee_is_finite(var(i)))var(i)=0.0
         if(ABS(var(i)) .lt. EPSILON)var(i)=0.0
      enddo
      error = VTK_VAR(size, varname, var)
      end subroutine ive_write_3dvar

      subroutine save_3d_filename(error)

      include 'incommands.icl'
      include 'message.icl'
      logical error
      logical first3dvar
      character*1024:: filename
      integer ibeg, iend, nblank,strbeg, strend
      common / write3ddata / first3dvar, filename
  
!get file and field names
      error=.false.
      ibeg  = strbeg(comand(FSTVAL))
      iend  = strend(comand(FSTVAL))
      if ( nblank(comand(FSTVAL)) .eq. 0 ) then
         write (message, 100) comand(FSTVAL)(ibeg:iend)
         call write_message
         error = .true.
         return
      endif
     
!     Set the file name.
!     
      filename(1:len(filename)) = char(0)
      filename = comand(FSTVAL)(ibeg:iend)
      call expand_path(filename)
 100  format (1x,'Command ',a,' requires a value.')
      return
      end subroutine save_3d_filename
