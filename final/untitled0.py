# -*- coding: utf-8 -*-
"""
Created on Sat May  6 21:48:47 2017

@author: XMKZ
"""

import numpy as np
np.random.seed(1337)  # for reproducibility
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
from keras.optimizers import SGD
import pandas as pd
data = pd.read_csv("D:/大四下/实验设计/final/k.csv",header=0)
data = np.array(data)
xdata, ydata = data[:,1:], data[:,0]
xdata.astype("float32")
ydata.astype("float32")
#stdsc = StandardScaler()
#X_train_std = stdsc.fit_transform(X_train)
#X_test_std = stdsc.fit_transform(X_test)

#X_train_std= X_train_std.astype('float32')
#X_test_std= X_test_std.astype('float32')


'''
ydata = np.array(ydata)
xdata = np.array(xdata)
ydata = ydata-1
xdata= xdata.astype('float32')
ydata= ydata.astype('float32')
X_train = xdata[0:7600,:]
X_test = xdata[7601:10671,:]
y_train = ydata[0:7600]
y_test = ydata[7601:10671]
'''

model = Sequential([
    Dense(5, input_dim=60),
    Activation('relu'),
    Dense(1),
    Activation('linear')
])

rmsprop = RMSprop(lr=0.001)

model.compile(optimizer=rmsprop,
              loss='mae'
              )

print('Training ------------')
# Another way to train the model
model.fit(xdata, ydata, epochs=100000, verbose=2, batch_size=10000)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(xdata,ydata)

print('test loss: ', loss)
print('test accuracy: ', accuracy)
'''
pre = model.predict_classes(X_test)
print(sum(pre==y_test)/3000)
'''
pre = model.predict(xdata)
np.savetxt('D:/大四下/实验设计/final/new.csv', pre, delimiter = ',')  