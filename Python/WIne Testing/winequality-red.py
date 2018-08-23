# -*- coding: utf-8 -*-
"""
Created on Fri Aug 17 21:09:51 2018

@author: Lenovo
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


data = pd.read_csv("winequality-red.csv")
X = data.iloc[:,:-1].values
y= data.iloc[:,11].values

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)


from sklearn.neighbors import KNeighborsClassifier
classifier = KNeighborsClassifier(n_neighbors=11,metric="minkowski", p=2)
classifier.fit(X_train,y_train)


#Predict
y_pred= classifier.predict(X_test)



'''
plt.scatter(y_test,colour='green')
plt.show()'''
from sklearn.metrics import confusion_matrix, accuracy_score

ac= accuracy_score(y_test, y_pred)



from sklearn.svm import SVC
classifier1 = SVC(kernel='rbf')