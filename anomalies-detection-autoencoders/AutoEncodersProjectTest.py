import pandas as pd
from sklearn import preprocessing
from pandas.plotting import scatter_matrix
import numpy as np
import pickle
import matplotlib.pyplot as plt
from scipy import stats
import tensorflow as tf
import seaborn as sns
from pylab import rcParams
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error
import h5py
from keras.models import Model, load_model
from keras.layers import Input, Dense
from keras.callbacks import ModelCheckpoint, EarlyStopping
from keras import regularizers

RANDOM_SEED = 42
LABELS = ["normal.", "ipsweep."]

# Read Data
kddCupTrain = pd.read_csv('../data/kddCupTrain.csv',header=None)
kddCupTest = pd.read_csv('../data/kddCupTest.csv',header=None)

# Preparing the data
# Drop columns which don't contribute any information (SD = 0 and MIN=MAX)
columnsList = [7, 19]

kddCupTrain.drop([41], axis=1, inplace=True) #Remove Class column
kddCupTrain.drop(columnsList, axis=1, inplace=True)
kddCupTest.drop(columnsList, axis=1, inplace=True)

# One Hot Encode categorical columns
kddCupTrain = pd.get_dummies(kddCupTrain, columns = [1, 2, 3])
kddCupTest = pd.get_dummies(kddCupTest, columns = [1, 2, 3])

# One Hot Encoding might generate a different set of columns since the categorical values in the columns for train and test may be different
# So remove the extra columns and set the missing columns to 0
missing_cols = set(kddCupTrain.columns) - set(kddCupTest.columns)

for c in missing_cols:
    kddCupTest[c] = 0

kddCupTest = kddCupTest[kddCupTrain.columns].copy()

# Standardize data by z-scoring

featuresList = [col for col in kddCupTest if col != 'Class']
scaler = preprocessing.StandardScaler()
scaler.fit(kddCupTest[featuresList]);
kddCupTest[featuresList] = scaler.transform(kddCupTest[featuresList])

autoencoder = load_model('autoencoder.h5')

testPredictions = autoencoder.predict(kddCupTest)

testMSE = mean_squared_error(X_test.transpose(), testPredictions.transpose(), multioutput='raw_values')

result_df = pd.DataFrame({'reconstruction_error': testMSE})
result_df.to_csv('submission.csv')