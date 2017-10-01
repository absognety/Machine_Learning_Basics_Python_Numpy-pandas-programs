#Two Dimensioanl Heat Conduction Equation Solution using Finite Difference Method
import numpy as np
import matplotlib.pyplot as plt

#Set Dimensions and delta (Rate Of Increment)
#Square mesh grid

A = int(input("Enter the length in X-direction..."))
B = int(input("Enter the length in Y-direction..."))
maxIter =int(input("Enter the number of Iterations..."))
Delta = int(input("Enter the step Size...."))
TherCon = int(input("Enter the thermal conductivity..."))

# Boundary condition user dependent
Tleft = int(input("Temperature at the left boundary......"))
Tright = int(input("Temperature at the right boundary....."))
Tbottom = int(input("Temperature at the bottom boundary....."))
Ttop = int(input("Temperature at the top boundary....."))

# Initial Temperature of Interior grid
Tinitial = int(input("Enter the initial guess of Interior grid here.."))

# Set colour interpolation and colour map
colorinterpolation = 50
colourMap = plt.cm.jet #you can try: colourMap = plt.cm.coolwarm

# Set meshgrid
X, Y = np.meshgrid(np.arange(0, A), np.arange(0, B))

# Set array size and set the interior value with Tguess
T = np.empty((A, B))
T.fill(Tinitial)

# Set Boundary condition
T[(B-1):, :] = Ttop
T[:1, :] = Tbottom
T[:, (A-1):] = Tright
T[:, :1] = Tleft

# Iteration (We assume that the iteration is convergence in maxIter)
print("Please wait for a moment")
for iteration in range(0, maxIter):
    for i in range(1, A-1, Delta):
        for j in range(1, B-1, Delta):
            if(A==B):
                T[i, j] = 0.25 * (T[i+1][j] + T[i-1][j] + T[i][j+1] + T[i][j-1])
            else:
                T[i, j] = 0.5 * ((T[i+1][j] + T[i-1][j])/(A**2)  + (T[i][j+1] + T[i][j-1])/(B**2)) * ((A*B)**2)/((A**2) + (B**2)) 

print(T)

print("Iteration finished")

# Configure the contour
plt.title("Temperature Contour")
plt.contourf(np.reshape(X, T.shape), np.reshape(Y, T.shape), T, colorinterpolation, cmap=colourMap)

# Set Colorbar
plt.colorbar()

# Show the result in the plot window
plt.show()

print("")