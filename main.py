import math
from matplotlib import pyplot as plt
from numpy import arange


def f(x, y):
    return y + 3.7*x*(1 - x) + 9.4

def correct_solution(x):
    return math.exp(x) + math.exp(-x) + 3.7*math.pow(x, 2) - 3.7*x - 2

def q(x):
    return 3.7*x*(1 - x) + 9.4

def p(x):
    return 1

y0 = 0
y1 = math.e + 1/math.e - 2

N10 = 10
N20 = 20
h10 = 1.0/10
h20 = 1.0/20

xs10 = []
for i in arange(0, 1 + h10, h10):
    xs10.append(i)
xs20 = []
for i in arange(0, 1 + h20, h20):
    xs20.append(i)

xs100 = []
for i in arange(0, 1 + 1.0/100, 1.0/100):
    xs100.append(i)
correct_y = []
for i in xs100:
    correct_y.append(correct_solution(i))

def euler(init_value, points, step):
    res = []
    res.append(init_value)
    for i in points:
        y = res.pop()
        res.append(y)
        res.append(y + step*f(i, y))
    return res

def adams2(y0, y1, points, step):
    pass

def prog(points, h):
    mu = [0]
    lam = [0]
    for i in range(len(points)):
        current_l = lam[-1]
        current_m = mu[-1]
        A = 2 + math.pow(h, 2)
        B = math.pow(h, 2)*q(points[i])
        next_l = 1/(A - current_l)
        next_m = (current_m - B)/(A - current_l)
        mu.append(next_m)
        lam.append(next_l)

    res = [y1]
    mu.reverse()
    lam.reverse()
    for i in range(len(points)):
        res.append(lam[i]*res[i] + mu[i])
    res.reverse()
    return res

res20 = prog(xs20, h20)
res10 = prog(xs10, h10)

fig, ax = plt.subplots(figsize=(10, 10))
ax.plot(xs100, correct_y, label="solution")

ax.plot(xs20, res20[1:], label="N20")
ax.plot(xs10, res10[1:], label="N10")
ax.legend()
plt.show()
