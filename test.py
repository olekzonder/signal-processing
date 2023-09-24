import matplotlib.pyplot as plt
import numpy as np

u0 = 0.024  # амплитуда сигнала в В
f0 = 140e6  # несущая частота сигнала в Гц
m1 = 1  # коэффициент модуляции
fm1 = 240e3  # частота модуляции в Гц

x = np.linspace(0, 3/fm1, 1000)  # временная ось от 0 до 1 секунды
y = u0 * np.cos(2 * np.pi * f0 * x) + (0.5 * m1 * u0) * np.cos(2 * np.pi * (f0 + fm1) * x) + (0.5 * m1 * u0) * np.cos(2 * np.pi * (f0 - fm1) * x)

plt.plot(x, y)
plt.xlabel('Время (сек)')
plt.ylabel('Амплитуда сигнала (В)')
plt.title('График амплитудно-модулированного сигнала')
plt.grid(True)
plt.show()
