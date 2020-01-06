# Scatterplot of particle position in x vs particle position in y
import matplotlib.pyplot as plt
import numpy as np
import netCDF4 as NC

# Use NetCDF
data = NC.Dataset("px913.nc","r",format="NETCDF4")
#fh = Dataset(my_example_nc_file, mode='r')

position = data.variables['electron_position']

print(position.shape)

# Range of x and y
yran = [ min(position[:,1]), max(position[:,1])]
xran = [ min(position[:,0]), max(position[:,0])]
print(yran)
print(xran)
print(position[0,1], position[0,0])
#quit()

# Create one subplot
fig, ax = plt.subplots()

#Scatter plot
c = ax.scatter(position[:,0],position[:,1])
#plt.scatter(position[:,0],position[:,1])
ax.set_title('Electron position')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_ylim(yran)
ax.set_xlim(xran)

#data.close()

plt.show()


