import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("exp.dat", unpack=True), label="exp")
plt.xlabel("x")
plt.ylabel("y")
plt.legend()
plt.show()
