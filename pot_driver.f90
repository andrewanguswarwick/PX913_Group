PROGRAM main

USE potential

IMPLICIT NONE

TYPE(run_data) :: rundata
TYPE(axes) :: axes_
TYPE(particle) :: particle_
REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: density, pot
REAL(REAL64) :: execution_time, dx_, dy_

! Read inputs from command line
CALL user_inputs(rundata)

! Initialise axes, charge density, and particle position/velocity
CALL problem_setup(rundata, axes_, particle_, density, dx_, dy_)

! Use Gauss-Seigel method to evaluate grid potential
CALL calc_potential(rundata, density, dx_, dy_, pot)

! Print execution time
CALL CPU_TIME(execution_time)
print '(A,F0.2,A)', "Execution time: ", execution_time, "s"

END PROGRAM
