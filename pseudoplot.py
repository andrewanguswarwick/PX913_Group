#!/usr/bin/python
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as NC
from matplotlib.colors import LogNorm

# convert matrix to normal real coordinates
def convert_coords(N):
	a = N.shape
	b = np.zeros((a[1],a[0]))
	for i in range(a[1]):
		for j in range(a[0]):
			b[i,j] = N[j,a[1]-1-i]
	return b

# how to access netcdf
data = NC.Dataset("px913.nc","r",format="NETCDF4")

Ex = data.variables['Ex_field']
Ey = data.variables['Ey_field']

x_axis = np.linspace(-1,1,100)
y_axis = np.linspace(-1,1,100)


# Create one subplot
fig, ax = plt.subplots()

#Pseudoplot

c= ax.pcolor(x_axis,y_axis,convert_coords(Ex),cmap='RdGy',)
fig.colorbar(c,ax=ax)
ax.set_title('Electric field(x)')
ax.set_xlabel('x')
ax.set_ylabel('y')



plt.show()
#plt.savefig('pseudo_plot.pdf')


