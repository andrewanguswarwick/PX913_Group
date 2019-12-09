PROGRAM main

USE potential
USE verlet

IMPLICIT NONE

TYPE(run_data) :: rundata
TYPE(xy) :: axis
TYPE(particle) :: particle_
TYPE(electric_field) :: e_field
REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: density, pot
REAL(REAL64) :: execution_time, dx_, dy_
INTEGER(INT32) :: timesteps

! Set number of timesteps and problem dimensions
timesteps = 1000_INT32


! Read inputs from command line
CALL user_inputs(rundata)

! Initialise axes, charge density, and particle position/velocity
CALL problem_setup(rundata, axis, particle_, density, dx_, dy_, timesteps)

! Use Gauss-Seigel method to evaluate grid potential
CALL calc_potential(rundata, density, dx_, dy_, pot)

! Calculate electric field
CALL calc_e_field(rundata,dx_,dy_,pot,e_field)

! Run 1000 steps of particle movement
CALL move_particle(e_field, particle_, timesteps, dx_, dy_)

! Write output file

! Print execution time
CALL CPU_TIME(execution_time)
print '(A,F0.2,A)', "Execution time: ", execution_time, "s"

END PROGRAM
