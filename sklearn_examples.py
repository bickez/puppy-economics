from sklearn import datasets, svm
from sklearn.cluster import KMeans
from sklearn.ensemble import AdaBoostClassifier, RandomForestClassifier, RandomForestRegressor, GradientBoostingRegressor
from sklearn.cross_validation import train_test_split
from sklearn.metrics import confusion_matrix, mean_squared_error
from sklearn.preprocessing import Imputer
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime

################################################################################
#  Scikit-Learn
#  1) Clustering: K-Means
#  2) Visualizing Clusters with Matplotlib
#  3) Train/Test Split and Confusion Matrix for Classification Error
#  4) Classification Example Comparing Different Algos
#     (a) AdaBoost
#     (b) SVM
#     (c) RandomForest
#  5) Visualizing Confusion Matrices with Matplotlib
#  6) 'Regression', MSE and RMSE
#  7) Pandas and Scikit-Learn
################################################################################

#  1)
iris = datasets.load_iris()  # load the iris dataset (there are other builtins)
print type(iris)
print type(iris.data)  # sklearn useses NumPy arrays (Pandas isn't integrated)
print iris.data[:10,:]  # print the first 10 lines of the dataset
print iris.feature_names

# KMeans clustering on the iris dataset
n_clusters = 3
k_means = KMeans(init='k-means++', n_clusters=n_clusters)
k_means.fit(iris.data)  # fit the KMeans algo to the iris data
print k_means.cluster_centers_  # get the cluster centers
print k_means.labels_  # cluster label for each data point
print np.unique(k_means.labels_)

#  2)
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111)
featurex = 0  # The first 'feature' in iris.feature_names
featurey = 1  # The second 'feature' in iris.feature_names
for k in range(n_clusters):
    clust_members = k_means.labels_ == k
    x = iris.data[clust_members, featurex]
    y = iris.data[clust_members, featurey]
    ax.plot(x, y, 'o', c=plt.cm.spring(k*100), label=k, markersize=10)
ax.set_xlabel(iris.feature_names[featurex])
ax.set_ylabel(iris.feature_names[featurey])
#  bump the axis values ~+/- 5%
ax.set_xlim(np.min(iris.data[:, featurex])*0.95, np.max(iris.data[:, featurex])*1.05)
ax.set_ylim(np.min(iris.data[:, featurey])*0.95, np.max(iris.data[:, featurey])*1.05)
ax.legend()
plt.show()

#  3)
#  train_test_split will split the data into training and testing data
X_train, X_test, y_train, y_test = train_test_split(iris.data, iris.target, train_size=0.7)
clf = AdaBoostClassifier()  # adaboost classifier
#  fit AdaBoost and then use the fitted model to predict on the test data
y_pred = clf.fit(X_train, y_train).predict(X_test)
print confusion_matrix(y_test, y_pred)  # confusion matrix

#  4)
n = 100  # num trials - run each algo 'n' times on diff random split of data
cm_ada = []  # list for adding AdaBoost confusion matrix from each trial
cm_svm = []
cm_rf = []
for i in range(n):
    X_train, X_test, y_train, y_test = train_test_split(iris.data, iris.target, train_size=0.7)
    clf = AdaBoostClassifier()  # 4(a)
    y_pred = clf.fit(X_train, y_train).predict(X_test)
    cm_ada.append(confusion_matrix(y_test, y_pred))
    clf = svm.SVC()  # 4(b)
    y_pred = clf.fit(X_train, y_train).predict(X_test)
    cm_svm.append(confusion_matrix(y_test, y_pred))
    clf = RandomForestClassifier()  # 4(c)
    y_pred = clf.fit(X_train, y_train).predict(X_test)
    cm_rf.append(confusion_matrix(y_test, y_pred))
#  average the confusion matrices across the 'n' trials
cm_ada_mu = np.mean(np.array(cm_ada), axis=0)
cm_svm_mu = np.mean(np.array(cm_svm), axis=0)
cm_rf_mu = np.mean(np.array(cm_rf), axis=0)
print cm_ada_mu
print cm_svm_mu
print cm_rf_mu

#  5)
#  function for plotting confusion matrix
def plot_cm(cm, target_names, title='Standardized Confusion Matrix',
            cmap=plt.cm.Greens):
    cm_norm = cm * 1. / cm.sum(axis=1)[:, np.newaxis]  # standardize the confusion matrix
    plt.imshow(cm_norm, interpolation='nearest', cmap=cmap)
    plt.colorbar()
    ticks = np.arange(len(target_names))
    plt.xticks(ticks, target_names, rotation=45)
    plt.yticks(ticks, target_names)
    plt.xlabel('Predicted')
    plt.ylabel('Actual')
    plt.title(title)
    plt.tight_layout()

plt.figure(figsize=(30, 10), facecolor='w')
plt.subplot(131)
plot_cm(cm_ada_mu, iris.target_names, title='AdaBoost')
plt.subplot(132)
plot_cm(cm_svm_mu, iris.target_names, title='SVM')
plt.subplot(133)
plot_cm(cm_rf_mu, iris.target_names, title='Random Forest')
plt.show()

#  6)
boston = datasets.load_boston()  # built-in boston dataset
print boston.DESCR  # description of the dataset
X_train, X_test, y_train, y_test = train_test_split(boston.data, boston.target, train_size=0.7)

reg = RandomForestRegressor()  # random forest regression (continuous target)
y_pred = reg.fit(X_train, y_train).predict(X_test)
print np.mean((y_pred - y_test)**2)  # MSE using numpy
print mean_squared_error(y_test, y_pred)  # MSE using sklearn built-in
print np.sqrt(np.mean((y_pred - y_test)**2))  # RMSE using numpy
print np.sqrt(mean_squared_error(y_test, y_pred))  # RMSE using sklearn
print np.sqrt(mean_squared_error(y_test, y_pred)) / np.mean(boston.target)

reg = GradientBoostingRegressor()  # gradient boosting for regression
y_pred = reg.fit(X_train, y_train).predict(X_test)
print np.sqrt(mean_squared_error(y_test, y_pred))
