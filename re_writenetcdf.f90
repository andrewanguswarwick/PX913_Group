!some kind of 2D array
!>@Author: I Ismail
MODULE write_netcdf
  USE netcdf


IMPLICIT NONE

CONTAINS

SUBROUTINE visualiser(filename,density,pot,pos,vel,acc,e_fieldx,e_fieldy,rundat,ts)


  IMPLICIT NONE
  integer   :: ierr, fid, i, j,var_twod_id
  integer, dimension(rundat%nx) :: gridax
  integer, dimension(rundat%ny) :: griday
  TYPE(run_data) :: rundat
  REAL(REAL64), DIMENSION(rundat%nx,rundat%ny), INTENT(IN) :: density,pot,e_fieldx,e_fieldy
  REAL(REAL64), DIMENSION(2,0:ts), INTENT(IN) :: pos,vel,acc
  INTEGER(INT32),INTENT(IN) :: ts 
  integer, parameter :: ndims = 2 ! an X,Y thing
  integer, dimension(ndims) :: Array_size1, Array_size2,dim_twod_ids,var_twoax_ids
  character(len=1) , DIMENSION(ndims) :: griddims=(/"x","y"/)
  character(len=100) :: filename
! CHARACTER(LEN=*), INTENT(IN) :: filename




 !Find out the size of your array. To do that, use the shape function as seen below.
 Array_size1 = SIZE(pot)
 Array_size2 = SIZE(pos)

 Do i = 1, rundat%nx
   gridax(i) = i
 END DO

 Do i = 1, rundat%ny
   griday(i) = i
 END DO



 !create the file and overwrite if it exists
 ierr = nf90_create(filename, NF90_CLOBBER, fid)
 IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
 !print*, 'FILE ID = ', fid

 !put attribute NetCDF template format
 ierr = nf90_put_att(fid,NF90_GLOBAL,"name",rundat%prob)
 IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))

 ierr = nf90_put_att(fid,NF90_GLOBAL,"gridsize_x",rundat%nx)
 IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))

 ierr = nf90_put_att(fid,NF90_GLOBAL,"gridsize_y",rundat%ny)
 IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))



 !define dimension netcdf block
 do i = 1, ndims
   ierr = nf90_def_dim(fid, griddims(i), Array_size1(i), dim_twod_ids(i))
   if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))
   ierr = nf90_def_var(fid, griddims(i), NF90_INT, dim_twod_ids(i), var_twoax_ids(i))
   if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))
 enddo

 ! define grid variable made from two DOF
 ierr = nf90_def_var(fid, "gridfinal", NF90_INT, dim_twod_ids, var_twod_id)
 if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))

 !Finish defining metadata
 ierr = nf90_enddef(fid)
 if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))

 !write data into variables "put var"
 ierr = nf90_put_var(fid,var_twod_id, density)
 if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))
 do i = 1, ndims
   if (i = 1) THEN
   ierr = nf90_put_var(fid, var_twoax_ids(i), gridax)
   ELSE    
   ierr = nf90_put_var(fid, var_twoax_ids(i), griday)
END IF
   if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))
 enddo

 ! Close the file
 ierr = nf90_close(fid)
 if (ierr /= nf90_noerr) print*, trim(nf90_strerror(ierr))

END SUBROUTINE visualiser

END MODULE write_netcdf
