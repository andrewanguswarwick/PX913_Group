!some kind of 2D array
!>@Author: I Ismail
MODULE write_netcdf
  USE netcdf
  USE kinds
  USE potential
  USE verlet
IMPLICIT NONE

CONTAINS

SUBROUTINE visualiser(filename, ierr, density, pot, electron, efield, rundata)
  ! here are the subroutine inputs
  character(len=8) :: filename  
  TYPE(particle) :: electron
  TYPE(electric_field) :: efield
  TYPE(run_data) :: rundata
  REAL(REAL64), DIMENSION(:, :), INTENT(IN) :: density, pot
  INTEGER :: ierr

  ! dimensions for variables 
  CHARACTER(LEN=10), DIMENSION(2) :: dims_density=(/"density_x", "density_y"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_pot=(/"pot_x", "pot_y"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_pos=(/"pos_grid", "pos_time"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_vel=(/"vel_grid", "vel_time"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_a=(/"a_grid", "a_time"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_efieldx=(/"efield_xx", "efield_xy"/)
  CHARACTER(LEN=10), DIMENSION(2) :: dims_efieldy=(/"efield_yx", "efield_yy"/)
  
  ! variable ids
  INTEGER :: var_id_density, var_id_pot, var_id_efieldx, var_id_efieldy
  INTEGER :: var_id_pos, var_id_vel, var_id_a
  ! sizes
  INTEGER, DIMENSION(2) :: sizes_density, sizes_pot, sizes_efieldx, sizes_efieldy
  INTEGER, DIMENSION(2) :: sizes_pos, sizes_vel, sizes_a
  ! dimension ids
  INTEGER, DIMENSION(2) :: dim_ids_density, dim_ids_pot, dim_ids_efieldx, dim_ids_efieldy
  INTEGER, DIMENSION(2) :: dim_ids_pos, dim_ids_vel, dim_ids_a

  ! file id and loop variable
  INTEGER :: fid, i


  ! Create the file or overwrite if it exists
  ierr = nf90_create(filename, NF90_CLOBBER, fid)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))

  ! variable sizes 
  sizes_density = SHAPE(density)
  sizes_pot = SHAPE(pot)
  sizes_efieldx = SHAPE(efield%x)
  sizes_efieldy = SHAPE(efield%y)
  sizes_pos = SHAPE(electron%pos)
  sizes_vel = SHAPE(electron%vel)
  sizes_a = SHAPE(electron%a)
    
  
  ! global attributes in run_data
  ! nx
  ierr = nf90_put_att(fid, NF90_GLOBAL,"nx",rundata%nx)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! ny
  ierr = nf90_put_att(fid,NF90_GLOBAL,"ny",rundata%ny)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! problem
  ierr = nf90_put_att(fid,NF90_GLOBAL,"problem",rundata%prob)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
   
  
  ! data for density
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid,dims_density(i),sizes_density(i),dim_ids_density(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO
  ierr = nf90_def_var(fid, "charge_density", NF90_DOUBLE, dim_ids_density, var_id_density)
  
  ! data for potential      
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid, dims_pot(i),sizes_pot(i),dim_ids_pot(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO  
  ierr = nf90_def_var(fid, "electric_potential", NF90_DOUBLE, dim_ids_pot, var_id_pot)
  
  ! data for electric fields  
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid, dims_efieldx(i), sizes_efieldx(i), dim_ids_efieldx(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO
  ierr = nf90_def_var(fid,"Ex_field",NF90_DOUBLE,dim_ids_efieldx, var_id_efieldx)

  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))  
  DO i=1,2
     ierr = nf90_def_dim(fid, dims_efieldy(i), sizes_efieldy(i), dim_ids_efieldy(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO
  ierr = nf90_def_var(fid,"Ey_field", NF90_DOUBLE, dim_ids_efieldy, var_id_efieldy)
  
  ! data for electron position, velocity and acceleration
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid, dims_pos(i), sizes_pos(i), dim_ids_pos(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO 
  ierr = nf90_def_var(fid, "electron_position", NF90_DOUBLE,dim_ids_pos, var_id_pos)

  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid, dims_vel(i), sizes_vel(i), dim_ids_vel(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO 
  ierr = nf90_def_var(fid,"electron_velocity",NF90_DOUBLE,dim_ids_vel,var_id_vel)
  
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  DO i=1,2
     ierr = nf90_def_dim(fid,dims_a(i),sizes_a(i),dim_ids_a(i))
     IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  END DO
  ierr = nf90_def_var(fid,"electron_acceleration",NF90_DOUBLE,dim_ids_a,var_id_a)
  
  ! end metadata definition
  ierr = nf90_enddef(fid)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))


  ! Write all data into file
  ! density
  ierr = nf90_put_var(fid, var_id_density, density)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! pot
  ierr = nf90_put_var(fid, var_id_pot, pot)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr)) 
  ! efield_x
  ierr = nf90_put_var(fid,var_id_efieldx, efield%x)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! efield_y
  ierr = nf90_put_var(fid,var_id_efieldy, efield%y)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! electron position
  ierr = nf90_put_var(fid,var_id_pos, electron%pos)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! electron velocity
  ierr = nf90_put_var(fid,var_id_vel, electron%vel)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))
  ! electron acceleration
  ierr = nf90_put_var(fid,var_id_a, electron%a)
  IF (ierr /= nf90_noerr)  PRINT*, TRIM(nf90_strerror(ierr))  

  ! Close the file
  ierr = nf90_close(fid)
  IF (ierr /= nf90_noerr) PRINT*, TRIM(nf90_strerror(ierr))

END SUBROUTINE visualiser

END MODULE write_netcdf
