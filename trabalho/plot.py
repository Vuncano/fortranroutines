import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("graph.dat", unpack=True), label="graph")
plt.xlabel("M")
plt.ylabel("y")
plt.xlim(0, 2)
plt.ylim(-2, 2)
plt.grid()
plt.legend()
plt.show()
