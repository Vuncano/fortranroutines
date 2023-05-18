import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("logx.dat", unpack=True), label="log")
plt.xlabel("x")
plt.ylabel("y")
plt.legend()
plt.show()
