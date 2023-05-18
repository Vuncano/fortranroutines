import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("1-x.dat", unpack=True), color='blue', label="exp")
plt.plot(*np.loadtxt("1-x2.dat", unpack=True), color='blue')
plt.xlabel("x")
plt.ylabel("y")
plt.legend()
plt.show()
