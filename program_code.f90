PROGRAM main

USE potential
USE verlet
USE write_netcdf

IMPLICIT NONE

TYPE(run_data) :: rundata
TYPE(xy) :: axis
TYPE(particle) :: electron
TYPE(electric_field) :: e_field
REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: density, pot
REAL(REAL64) :: execution_time, dx_, dy_
INTEGER(INT32), PARAMETER :: timesteps = 1000_INT32
INTEGER :: ierr

! Read inputs from command line
CALL user_inputs(rundata)
! Initialise axes, charge density, and particle position/velocity
CALL problem_setup(rundata, axis, electron, density, dx_, dy_, timesteps)
! Use Gauss-Seigel method to evaluate grid potential
CALL calc_potential(rundata, density, dx_, dy_, pot)
! Calculate electric field
CALL calc_e_field(rundata,dx_,dy_,pot,e_field)
! Run 1000 steps of particle movement
CALL move_particle(e_field, electron, timesteps, dx_, dy_)
! Write output file
CALL visualiser("px913.nc", ierr, density, pot, electron, e_field, rundata)

! Deallocate arrays
DEALLOCATE(density,pot,electron%pos,electron%vel,electron%a,e_field%x,e_field%y)

! Print execution time
CALL CPU_TIME(execution_time)
print '(A,F0.2,A)', "Execution time: ", execution_time, "s"

END PROGRAM
