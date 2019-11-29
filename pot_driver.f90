PROGRAM main

USE potential

IMPLICIT NONE

TYPE(run_data) :: rundata

CALL user_inputs(rundata)

CALL make_grid(rundata)

!CALL calc_potential()

END PROGRAM
