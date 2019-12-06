! Author: Andrew Angus
! Description: This module takes in the electric potential of the grid,
! calculates the electric field, and moves a particle 1000 steps in this field
MODULE verlet

USE kinds
USE potential

IMPLICIT NONE

CONTAINS

SUBROUTINE delete(rundat, axis, part, rho, dx, dy)

  TYPE(run_data), INTENT(IN) :: rundat
  TYPE(axes), INTENT(OUT) :: axis
  TYPE(particle), INTENT(OUT) :: part
  REAL(REAL64), DIMENSION(2) :: axes_range
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: rho
  REAL(REAL64), INTENT(OUT) :: dx, dy
  INTEGER(INT32) :: num_ghosts, i, j

END SUBROUTINE

SUBROUTINE move_particle(rundat, rho, dx, dy, phi)

  TYPE(run_data), INTENT(IN) :: rundat
  REAL(REAL64), DIMENSION(:,:), INTENT(IN) :: rho
  REAL(REAL64), INTENT(IN) :: dx, dy
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: phi
  REAL(REAL64), PARAMETER :: tol = 1e-5_REAL64 ! tolerance
  REAL(REAL64) :: e_tot, d_rms, deriv_xx, deriv_yy
  INTEGER(INT32) :: i, j

END SUBROUTINE

END MODULE
