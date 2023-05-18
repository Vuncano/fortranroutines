import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("sinx.dat", unpack=True), label="sin(x)")
plt.plot(*np.loadtxt("x.dat", unpack=True), label="x")
plt.xlabel("x")
plt.ylabel("y")
plt.legend()
plt.show()
