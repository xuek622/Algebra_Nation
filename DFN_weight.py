
import numpy as np
import pandas as pd
import os
import time
import sys

os.environ["KERAS_BACKEND"] = "plaidml.keras.backend"



from keras.layers import Input, Dense, Dropout, Concatenate, Lambda, Add
from keras.models import Model
from keras import regularizers, optimizers, callbacks, initializers
from keras.constraints import min_max_norm, unitnorm
import keras.backend as K
import tensorflow as tf
import matplotlib.pyplot as plt

from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error


def create_dfn(input_shape):

    
    inputs = Input(shape = (input_shape,))
    # first sub net

    hidden_1 = Dense(units= 200, activation='relu', # 100
                 activity_regularizer = regularizers.l1(1e-10))(inputs)
    #drop1 = Dropout(.1)(hidden_1)
    hidden_2 = Dense(units= 200, activation='relu', # 100
                 activity_regularizer = regularizers.l1(1e-10))(hidden_1)
    hidden_3 = Dense(units= 100, activation='relu', # 60
                 activity_regularizer = regularizers.l1(1e-10))(hidden_2)


    theta_layer = Dense(1, activation='linear')(hidden_3)

    theta_outputs = Dense(1, activation='linear', 
                          kernel_constraint= min_max_norm(max_value=1, min_value= 1),
                          bias_constraint = min_max_norm(max_value = 1, min_value = -1),
                          kernel_initializer= 'ones', 
                          bias_initializer = 'zeros')(theta_layer)
                          #bias_initializer= initializers.Constant(value= -0.05 )
                          
    
    response_outputs = Dense(input_shape, 
    kernel_constraint= min_max_norm(max_value = 3, min_value = 1),
    bias_constraint = min_max_norm(max_value = 3, min_value = -3),
    kernel_initializer= initializers.Constant(value=1.5), 
    bias_initializer= initializers.Constant(value=0), activation = 'sigmoid')(theta_outputs)
    
    deepFowardFeedNets = Model(inputs, output = [theta_outputs, response_outputs])
    projectTheta = Model(inputs, theta_layer)
    
    return deepFowardFeedNets, projectTheta


    

if sys.platform == 'win32':
    Data_path = os.path.join('C:/', 'Users','Kang Xue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Simulated_Data")
    Save_path = os.path.join('C:/', 'Users','Kang Xue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Refined_Theta")
else:
    Data_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Simulated_Data")

    Save_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Refined_Theta")



files = os.listdir(Data_path)
domains = ["571", "572", "573", "574", "577", "578", "579", "580", "581", "582"]
#weights = [45, 30, 11, 35, 40, 30, 30, 15, 23, 14] # best weight ofter selecting

for domain_index in range(0, 10):
    domain = domains[domain_index]
    print(domain)
    response = np.array(pd.read_csv(os.path.join(Data_path, domain +'_est_theta_response.csv'), 
                                index_col=0))

    index = ~np.apply_along_axis(arr = np.isnan(response), axis=1, func1d=np.any)
    X = response[index, 3:].copy()


    Theta_true = response[index, 0].copy().reshape((-1,1))
    Theta_est = response[index, 1].copy().reshape((-1,1))

    index_split = np.random.permutation(X.shape[0])
    split = int(.8*X.shape[0])
    #len(index)
    X_train = X[index_split[:split], ]
    Y_train = Theta_est[index_split[:split]]

    X_test = X[index_split[split:], ]
    Y_test = Theta_est[index_split[split:]]



    #outputs = Concatenate()([theta_outputs, response_outputs])

       
    
    for weight in range(0, 101, 5):
        weight_theta = weight
        X_pred = Theta_est.copy()
        iteration = 0
        iter_max = 20
        validation_loss_theta = []
        validation_loss_response = []
        training_loss_theta = []
        training_loss_response = []
        tf.reset_default_graph()
        start_time = time.time()
        while iteration < iter_max:
            print(weight_theta, iteration)
            
            deepFowardFeedNets, projectTheta = create_dfn(X.shape[1])

            earlyStopping = callbacks.EarlyStopping(monitor='val_loss', patience=2, verbose=0, mode='auto')

            deepFowardFeedNets.compile(optimizer=optimizers.Adam(), 
                                    loss=["mean_squared_error", "binary_crossentropy"], 
                                    loss_weights=[weight_theta, 100 - weight_theta], 
                                    metrics=['accuracy'])
            max_epochs = 200
            batch_size = 128

            history = deepFowardFeedNets.fit(X_train, [Y_train,  X_train], 
                                             epochs=max_epochs, batch_size=batch_size,
                                             shuffle=True, verbose = 0, 
                                             validation_data = (X_test, [Y_test, X_test]),
                                             callbacks=[earlyStopping])
            
            validation_loss_theta.append(np.mean(history.history['val_dense_5_loss'][-5:]))
            validation_loss_response.append(np.mean(history.history['val_dense_6_loss'][-5:]))
            training_loss_theta.append(np.mean(history.history['dense_5_loss'][-5:]))
            training_loss_response.append(np.mean(history.history['dense_6_loss'][-5:]))
        #X_pred = np.hstack((X_pred, deepFowardFeedNets.predict_on_batch(X)[0]))
            X_pred = np.hstack((X_pred, projectTheta.predict_on_batch(X)))
            
            
            del deepFowardFeedNets, projectTheta
            K.clear_session()
            
            iteration += 1
            
        X_pred_1 = np.apply_along_axis(arr = X_pred[:, 1:], 
                                       axis= 1, func1d= np.mean)

        print(mean_squared_error(Theta_true, Theta_est), 
              mean_squared_error(Theta_true, X_pred_1),
              np.mean(validation_loss_theta), np.mean(validation_loss_response),
              np.mean(training_loss_theta), np.mean(training_loss_response))
        
        print(Theta_true.mean(), Theta_est.mean(), X_pred_1.mean())

        print(Theta_true.var(), Theta_est.var(), X_pred_1.var())
        
        print('computing time:', time.time() - start_time)
        
        if(weight == 0):
            X_pred_weight = np.array([Theta_true.mean(),
                                      X_pred_1.mean(), 
                                      np.mean(validation_loss_theta), 
                                      np.mean(validation_loss_response), 
                                      np.mean(training_loss_theta), 
                                      np.mean(training_loss_response),
                                      weight])
        else:
            X_pred_weight = np.vstack((X_pred_weight, 
                                       np.array([Theta_true.mean(), 
                                       X_pred_1.mean(),
                                       np.mean(validation_loss_theta), 
                                       np.mean(validation_loss_response),
                                       np.mean(training_loss_theta),
                                       np.mean(training_loss_response),
                                       weight])))
        
    np.savetxt(X=X_pred_weight,
               delimiter=',', 
               header = 'ability_true, ability_avg, validation_loss_theta,validation_loss_response, train_loss_theta, train_loss_response,  weight',
               fname=os.path.join(Save_path, domain+'_weighted_estimate.csv'))




