import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("sin.dat", unpack=True), label="sin")
plt.plot(*np.loadtxt("taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("2taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("3taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("4taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("5taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("6taylor.dat", unpack=True))
# plt.plot(*np.loadtxt("7taylor.dat", unpack=True))
plt.xlabel("x")
plt.ylabel("y")
plt.ylim(-2, 2)
plt.xlim(-10, 10)
plt.legend()
plt.show()
