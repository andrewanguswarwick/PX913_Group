! Author: Andrew Angus
! Description: This module takes in user command line inputs, establishes
! the grid with initial charge density, and evaluates the electric potential on
! the grid using Gauss-Seigel method
MODULE potential

USE kinds
USE command_line
USE domain_tools

IMPLICIT NONE

! Command-line inputs
TYPE run_data
  INTEGER(INT32) :: nx, ny
  CHARACTER(LEN=10) :: prob
END TYPE

CONTAINS

SUBROUTINE user_inputs(rundat)

  TYPE(run_data), INTENT(OUT) :: rundat
  LOGICAL :: success

  !Parse all the command line args
  CALL parse_args

  !Take in grid size nx and check for success and validity
  success = get_arg("nx", rundat%nx)
  IF (.NOT. success) THEN
    PRINT '(A)', "Failed to grid dimension nx from command-line."
    STOP
  ELSE IF (rundat%nx < 3) THEN
    PRINT '(A)', "Invalid value of grid dimension nx entered."
    STOP
  END IF

  !Take in grid size ny and check for success and validity
  success = get_arg("ny", rundat%ny)
  IF (.NOT. success) THEN
    PRINT '(A)', "Failed to grid dimension ny from command-line."
    STOP
  ELSE IF (rundat%ny < 3) THEN
    PRINT '(A)', "Invalid value of grid dimension ny entered."
    STOP
  END IF

  !Take in problem string and check for success
  success = get_arg("problem", rundat%prob)
  IF (.NOT. success) THEN
    PRINT '(A)', "Failed to read problem specification from command-line."
    STOP
  END IF
  rundat%prob = TRIM(rundat%prob)

END SUBROUTINE user_inputs

SUBROUTINE make_grid(rundat)

  TYPE(run_data), INTENT(IN) :: rundat
  REAL(REAL64), DIMENSION(2) :: axes_range
  REAL(REAL64), DIMENSION(:), ALLOCATABLE :: xaxis, yaxis
  INTEGER(INT32) :: num_ghosts, i

  ! Create axes
  axes_range = (/ -1.0_REAL64, 1.0_REAL64 /)
  num_ghosts = 1_INT32
  CALL create_axis(xaxis, rundat%nx, axes_range, num_ghosts)
  CALL create_axis(yaxis, rundat%ny, axes_range, num_ghosts)

  PRINT *, SIZE(xaxis)

  DO i = 0, rundat%nx+1
    PRINT '(F5.2)', xaxis(i)
  END DO

END SUBROUTINE

!SUBROUTINE calc_potential

!END SUBROUTINE

END MODULE
