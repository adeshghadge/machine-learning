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

# Preparing the data
kddCupTrain.rename(columns={41:'Class'}, inplace=True)
kddCupTrain['Class'] = np.where(kddCupTrain['Class'] == 'normal.', 0, 1)

# Drop columns which don't contribute any information (SD = 0 and MIN=MAX)
columnsList = [7, 19]

kddCupTrain.drop(columnsList, axis=1, inplace=True)

# One Hot Encode categorical columns
kddCupTrain = pd.get_dummies(kddCupTrain, columns = [1, 2, 3])


# Standardize data by z-scoring

featuresList = [col for col in kddCupTrain if col != 'Class']
scaler = preprocessing.StandardScaler()
scaler.fit(kddCupTrain[featuresList]);
kddCupTrain[featuresList] = scaler.transform(kddCupTrain[featuresList])

# Split data into train and test
X_train_split, X_test_split = train_test_split(kddCupTrain, test_size=0.2,
                                   stratify=kddCupTrain['Class'], 
                                   random_state=RANDOM_SEED)

y_train = X_train_split['Class']
X_train = X_train_split.drop(['Class'], axis=1) #keep only normal data

y_test = X_test_split['Class']
X_test = X_test_split.drop(['Class'], axis=1) #keep only normal data

X_trainNorm = X_train[y_train == 0]
X_trainNorm_val = X_trainNorm.values # Only values, axis labels removed. This is input for the Autoencoder
X_testNorm_val = X_test[y_test == 0].values # The validation data    



# Create Model

input_dim = X_trainNorm_val.shape[1]
layer1_dim = 14
encoder_dim = 7

input_layer = Input(shape=(input_dim, ))

encoder1 = Dense(layer1_dim, activation="tanh", activity_regularizer=regularizers.l1(10e-5))(input_layer)
encoder2 = Dense(encoder_dim, activation="relu")(encoder1)
decoder1 = Dense(layer1_dim, activation='tanh')(encoder2)
decoder2 = Dense(input_dim, activation='linear')(decoder1)

autoencoder = Model(inputs=input_layer, outputs=decoder2)


# Fit the model

nb_epoch = 100
batch_size = 32

autoencoder.compile(optimizer='adam', 
                    loss='mean_squared_error')

checkpointer = ModelCheckpoint(filepath="autoencoder.h5",
                               verbose=0,
                               save_best_only=True)

earlystopping = EarlyStopping(monitor='val_loss', patience=1, verbose=0) # 'patience' number of not improving epochs

history = autoencoder.fit(X_trainNorm_val, X_trainNorm_val,
                    epochs=nb_epoch,
                    batch_size=batch_size,
                    shuffle=True,
                    validation_data=(X_testNorm_val, X_testNorm_val),
                    verbose=2,
                    callbacks=[checkpointer, #tensorboard, 
                               earlystopping]).history


# Test the model

testPredictions = autoencoder.predict(X_test)

testMSE = mean_squared_error(X_test.transpose(), testPredictions.transpose(), multioutput='raw_values')
error_df = pd.DataFrame({'reconstruction_error': testMSE,'true_class': y_test})

from sklearn.metrics import (confusion_matrix, auc, roc_curve, cohen_kappa_score, accuracy_score)

fpr, tpr, thresholds = roc_curve(error_df.true_class, error_df.reconstruction_error)
roc_auc = auc(fpr, tpr)

threshold = normal_error_df.reconstruction_error.quantile(q=0.995)
y_pred = [1 if e > threshold else 0 for e in error_df.reconstruction_error.values]

ck_score = cohen_kappa_score(error_df.true_class, y_pred)
acc_score = accuracy_score(error_df.true_class, y_pred)

print('AUC: ', roc_auc)
print('Cohen Kappa: ', ck_score)
print('Accuracy: ', acc_score)


with open('/trainHistoryDict', 'wb') as file_history:
        pickle.dump(history.history, file_history)

error_df.to_csv('error_df.csv')
error_df.to_pickle('error_df.pkl')

