# -*- coding: utf-8 -*-
"""
Created on Thu Sep  1 01:26:58 2016

@author: pablo
"""
import pandas as pd
salarios = salarios_v2csv.iloc[:,(5,6)]

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model


# Create linear regression object
model = linear_model.LinearRegression(fit_intercept = True)

# Train the model using the training sets
salario = salarios.salario.reshape(len(salarios), 1)
salini = salarios.salini.reshape(len(salarios), 1)
# The coefficients
fit = model.fit(salini, salario)
print 'Intercept: %.4f, salini: %.4f' % (fit.intercept_, fit.coef_)