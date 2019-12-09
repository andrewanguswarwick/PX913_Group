! Author: Andrew Angus
! Description: This module takes in user command line inputs, establishes
! the grid with initial charge density, and evaluates the electric potential on
! the grid using Gauss-Seigel method
MODULE potential

USE kinds
USE command_line
USE domain_tools

IMPLICIT NONE
SAVE

TYPE xy
  REAL(REAL64), DIMENSION(:), ALLOCATABLE :: x, y
END TYPE

TYPE run_data
  INTEGER(INT32) :: nx, ny
  CHARACTER(LEN=10) :: prob
END TYPE

TYPE particle
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: pos, vel, a
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
    PRINT '(A)', "Failed to read grid dimension nx from command-line."
    STOP
  ELSE IF (rundat%nx < 3) THEN
    PRINT '(A)', "Invalid value of grid dimension nx entered."
    STOP
  END IF

  !Take in grid size ny and check for success and validity
  success = get_arg("ny", rundat%ny)
  IF (.NOT. success) THEN
    PRINT '(A)', "Failed to read grid dimension ny from command-line."
    STOP
  ELSE IF (rundat%ny < 3) THEN
    PRINT '(A)', "Invalid value of grid dimension ny entered."
    STOP
  END IF

  !Take in problem string and check for success and validity
  success = get_arg("problem", rundat%prob)
  IF (.NOT. success) THEN
    PRINT '(A)', "Failed to read problem specification from command-line."
    STOP
  END IF
  IF (rundat%prob .NE. 'null' .AND. rundat%prob .NE. 'single'&
   .AND. rundat%prob .NE. 'double') THEN
    PRINT '(A)', "Invalid problem specification read from command-line."
    STOP
  END IF
  rundat%prob = TRIM(rundat%prob)

  ! Print run_data
  PRINT '(A,I0)', "Number of grid squares in x-dimension: " , rundat%nx
  PRINT '(A,I0)', "Number of grid squares in y-dimension: " , rundat%ny
  PRINT '(A,A)', "Problem selected is: ", rundat%prob

END SUBROUTINE user_inputs

SUBROUTINE problem_setup(rundat, axis, part, rho, dx, dy, ts)

  TYPE(run_data), INTENT(IN) :: rundat
  TYPE(xy), INTENT(OUT) :: axis
  TYPE(particle), INTENT(OUT) :: part
  REAL(REAL64), DIMENSION(2) :: axes_range
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: rho
  REAL(REAL64), INTENT(OUT) :: dx, dy
  INTEGER(INT32) :: num_ghosts, i, j
  INTEGER(INT32), INTENT(IN) :: ts

  ! Create axes
  axes_range = (/ -1.0_REAL64, 1.0_REAL64 /)
  num_ghosts = 1_INT32
  CALL create_axis(axis%x, rundat%nx, axes_range, num_ghosts)
  CALL create_axis(axis%y, rundat%ny, axes_range, num_ghosts)

  ! Calculate dx and dy
  dx = axis%x(2)-axis%x(1)
  dy = axis%y(2)-axis%y(1)

  ! Allocate arrays
  ALLOCATE(rho(rundat%nx,rundat%ny))
  ALLOCATE(part%pos(2,0:ts))
  ALLOCATE(part%vel(2,0:ts))

  ! Define initial density, particle position and velocity based on problem
  IF (rundat%prob == 'null') THEN
    rho = 0
    part%pos(:,0) = (/ 0.0_REAL64, 0.0_REAL64 /)
    part%vel(:,0) = (/ 0.1_REAL64, 0.1_REAL64 /)
  ELSE IF (rundat%prob == 'single') THEN
    DO j = 1, rundat%ny
      DO i = 1, rundat%nx
        rho(i,j) = EXP(-(axis%x(i)*10.0_REAL64)**2-(axis%y(j)*10.0_REAL64)**2)
      END DO
    END DO
    part%pos(:,0) = (/ 0.1_REAL64, 0.0_REAL64 /)
    part%vel(:,0) = (/ 0.0_REAL64, 0.0_REAL64 /)
  ELSE
    DO j = 1, rundat%ny
      DO i = 1, rundat%nx
        rho(i,j) = EXP(-((axis%x(i)+0.250_REAL64)*10.0_REAL64)**2- &
          ((axis%y(j)+0.250_REAL64)*10.0_REAL64)**2) + &
          EXP(-((axis%x(i)-0.750_REAL64)*5.0_REAL64)**2- &
          ((axis%y(j)-0.750_REAL64)*5.0_REAL64)**2)
      END DO
    END DO
    part%pos(:,0) = (/ 0.0_REAL64, 0.5_REAL64 /)
    part%vel(:,0) = (/ 0.0_REAL64, 0.0_REAL64 /)
  END IF

  ! Print problem setup
  !PRINT '(A)', "Problem initialised, charge density on grid calculated."
  PRINT '(2(A,F4.1))', "Initial particle position is: ", part%pos(1,0), ", ", &
    part%pos(2,0)
  PRINT '(2(A,F4.1))', "Initial particle velocity is: ", part%vel(1,0), ", ", &
    part%vel(2,0)

END SUBROUTINE

SUBROUTINE calc_potential(rundat, rho, dx, dy, phi)

  TYPE(run_data), INTENT(IN) :: rundat
  REAL(REAL64), DIMENSION(:,:), INTENT(IN) :: rho
  REAL(REAL64), INTENT(IN) :: dx, dy
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: phi
  REAL(REAL64), PARAMETER :: tol = 1e-5_REAL64 ! tolerance
  REAL(REAL64) :: e_tot, d_rms, deriv_xx, deriv_yy
  INTEGER(INT32) :: i, j

  ! Allocate potential array with bounds that account for ghost cells
  ! and initialise to zero (ghost cells will remain at this value)
  ! this initialisation also constitutes intial guess
  ALLOCATE(phi(0:rundat%nx+1,0:rundat%ny+1))
  phi = 0.0_REAL64

  ! Iteratatively calculate phi until normalised error less than tolerance
  e_tot = 1.0_REAL64 ; d_rms = 1.0_REAL64
  DO WHILE ((e_tot/MAX(d_rms,1.0_REAL64)) > tol)
    ! Evaluate all phi once, using updated values of phi as it goes
    DO j = 1, rundat%ny
      DO i = 1, rundat%nx
        phi(i,j) = -(rho(i,j)-((phi(i+1,j)+phi(i-1,j))/dx**2)- &
          ((phi(i,j+1)+phi(i,j-1))/dy**2))/((2/dx**2)+(2/dy**2))
      END DO
    END DO
    ! Evaluate total error and root mean squared of second derivatives
    e_tot = 0.0_REAL64
    d_rms = 0.0_REAL64
    DO j = 1, rundat%ny
      DO i = 1, rundat%nx
        deriv_xx = (phi(i-1,j)-2.0_REAL64*phi(i,j)+phi(i+1,j))/dx**2
        deriv_yy = (phi(i,j-1)-2.0_REAL64*phi(i,j)+phi(i,j+1))/dy**2
        e_tot = e_tot + ABS(deriv_xx+deriv_yy-rho(i,j))
        d_rms = d_rms + (deriv_xx+deriv_yy)**2
      END DO
    END DO
    d_rms = SQRT(d_rms)
  END DO

  PRINT '(A)', "Grid potential converged."

END SUBROUTINE

END MODULE
