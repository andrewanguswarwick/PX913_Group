# PX913_Group

potential.f90: takes in command line arguments for grid size and chosen problem; establishes initial electron position and velocity, as well as the grid with charge density, dependent on chosen problem; calculates the electric potential for the grid using the Gauss-Seigel method based on the charge density.  
  
verlet.f90: calculates electric field for the grid based on the potential; moves the electron 1000 timesteps using the velocity verlet algorithm and the Lorentz force.  
  
program_code.f90: main program where all subroutine calls are made.  
  
build: bash script which compiles relavent files; runs the executable with desired command line arguments; runs a python script to visualise the results.  
  
kinds.f90: module which defines variable kinds  
  
command_line.f90: module which facilitates reading of command line arguments  
  
create_axis.f90: module for creation of an axis of a specified number of cell-centred values between specified bounds, wih the addition of a specified number of ghost cells either side
