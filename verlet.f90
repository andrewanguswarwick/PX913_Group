! Author: Andrew Angus
! Description: This module takes in the electric potential of the grid,
! calculates the electric field, and moves an electron 1000 steps in this field
MODULE verlet

USE kinds
USE potential

IMPLICIT NONE
SAVE

TYPE electric_field
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: x,y
END TYPE

CONTAINS

SUBROUTINE calc_e_field(rundat, dx, dy, phi, e_)

  TYPE(run_data), INTENT(IN) :: rundat
  REAL(REAL64), INTENT(IN) :: dx, dy
  REAL(REAL64), DIMENSION(0:,0:), INTENT(IN) :: phi
  TYPE(electric_field), INTENT(OUT) :: e_
  INTEGER(INT32) :: i, j

  ! Allocate E field arrays
  ALLOCATE(e_%x(rundat%nx,rundat%ny))
  ALLOCATE(e_%y(rundat%nx,rundat%ny))

  ! Loop over grid and calculate E
  DO j = 1, rundat%ny
    DO i = 1, rundat%nx
      e_%x(i,j) = (phi(i+1,j) - phi(i-1,j)) / (2*dx)
      e_%y(i,j) = (phi(i,j+1) - phi(i,j-1)) / (2*dy)
    END DO
  END DO
  
  ! Print completion
  PRINT '(A)', "Evaluated E field."

END SUBROUTINE

SUBROUTINE move_particle(e, part, ts, dx, dy)

  TYPE(electric_field), INTENT(IN) :: e
  TYPE(particle), INTENT(INOUT) :: part
  REAL(REAL64), INTENT(IN) :: dx, dy
  REAL(REAL64) :: dt
  INTEGER(INT32), INTENT(IN) :: ts
  INTEGER(INT32) :: i, cell_x, cell_y

  ! Allocate acceleration array and initialise
  ALLOCATE(part%a(2,0:ts))
  cell_x = FLOOR((part%pos(1,0)+1.0_REAL64)/dx)+1
  cell_y = FLOOR((part%pos(2,0)+1.0_REAL64)/dy)+1
  part%a(1,0) = -e%x(cell_x,cell_y)
  part%a(2,0) = -e%y(cell_x,cell_y)

  ! Conduct velocity verlet integration
  dt = 0.01_REAL64
  DO i = 1, ts
    ! Positions at this timestep
    part%pos(1,i) = part%pos(1,i-1) + part%vel(1,i-1)*dt + (0.5*part%a(1,i-1)**2)*dt**2
    part%pos(2,i) = part%pos(2,i-1) + part%vel(2,i-1)*dt + (0.5*part%a(2,i-1)**2)*dt**2

    ! Locate particle cell and calculate acceleration at this timestep
    cell_x = FLOOR((part%pos(1,i)+1.0_REAL64)/dx)+1
    cell_y = FLOOR((part%pos(2,i)+1.0_REAL64)/dy)+1
    part%a(1,i) = -e%x(cell_x,cell_y)
    part%a(2,i) = -e%y(cell_x,cell_y)

    ! Calculate velocity at this timestep
    part%vel(1,i) = part%vel(1,i-1) + dt*(part%a(1,i)+part%a(1,i-1))/2
    part%vel(2,i) = part%vel(2,i-1) + dt*(part%a(2,i)+part%a(2,i-1))/2
    
  END DO
  
  ! Print final particle position
  PRINT '(2(A,F5.2))', "Particle moved, final position is: ", part%pos(1,ts), ", ", part%pos(2,ts)

END SUBROUTINE

END MODULE
