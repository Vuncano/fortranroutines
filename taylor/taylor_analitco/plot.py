import numpy as np
import matplotlib.pyplot as plt

plt.plot(*np.loadtxt("sin.dat", unpack=True), label="sin")
plt.plot(*np.loadtxt("taylor07.dat", unpack=True), label="taylor 7")
plt.plot(*np.loadtxt("taylor01.dat", unpack=True), label="taylor 1")
plt.plot(*np.loadtxt("taylor03.dat", unpack=True), label="taylor 3")
plt.plot(*np.loadtxt("taylor05.dat", unpack=True), label="taylor 5")
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