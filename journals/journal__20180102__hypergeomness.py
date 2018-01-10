# 2018/01/02
# def journal__20180102__hypergeomness():
#
# By Spencer Hatch
# 2017/10/23
#

# Need these packages/functions
import numpy as np
import math
import matplotlib.pyplot as plt
import scipy

alpha = 0.5
beta = 3
gamma = 1.5

print scipy.special.hyp2f1(alpha, beta, gamma, -10000000)
