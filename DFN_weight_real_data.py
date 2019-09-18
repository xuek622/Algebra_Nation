
import numpy as np
import pandas as pd
import os
import sys


#os.environ["KERAS_BACKEND"] = "plaidml.keras.backend"



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
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data")
    Save_path = os.path.join('C:/', 'Users','Kang Xue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data_output")
else:
#    Data_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
#                       "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data")
#    Save_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
#                       "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data_output")
    
    Data_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data")
    
    Save_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data_output")
    

files = os.listdir(Data_path)
domains = ["571", "572", "573", "574", "577", "578", "579", "580", "581", "582"]
weights = [45, 30, 11, 35, 40, 30, 30, 15, 23, 14]

#weights = [0, 30, 11, 35, 40, 30, 30, 15, 23, 14]

for domain_index in range(0, 10):
    domain = domains[domain_index]
    #print(domain)
#    response = np.array(pd.read_csv(os.path.join(Data_path, 'tys_1718_' + domain + '_anchor_student_response.csv'),
#                                    index_col = 0))
#    ability = np.array(pd.read_csv(os.path.join(Data_path, 'tys_1718_' + domain + '_ability_mirt.csv')))
    response = np.array(pd.read_csv(os.path.join(Data_path, 'anchor_responses', domain + '_new_anchor.csv')))[:, 2:]
    ability = np.array(pd.read_csv(os.path.join(Data_path, 'mirt_output', domain + '_new_anchor_ability.csv')))
    
    X = response.copy()


    #Theta_true = response[index, 0].copy().reshape((-1,1))
    Theta_est = ability[:, 0].copy()

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
        ability_pred = Theta_est.copy().reshape(-1, 1)
        iteration = 0
        iter_max = 20
        validation_loss_theta = []
        validation_loss_response = []
        training_loss_theta = []
        training_loss_response = []
        while iteration < iter_max:

            print(domain, weight_theta, iteration)
            deepFowardFeedNets, projectTheta = create_dfn(X.shape[1])

            earlyStopping = callbacks.EarlyStopping(monitor='val_loss', patience=2, verbose=0, mode='auto')

            deepFowardFeedNets.compile(optimizer=optimizers.Adam(), 
                                    loss=["mean_squared_error", "binary_crossentropy"], 
                                    loss_weights=[weight_theta, 100 - weight_theta], 
                                    metrics=['accuracy'])
            max_epochs = 30
            batch_size = 32

            train_history = deepFowardFeedNets.fit(X_train, [Y_train,  X_train], 
                                             epochs=max_epochs, batch_size=batch_size,
                                             shuffle=True, verbose = 0, 
                                             validation_data = (X_test, [Y_test, X_test]),
                                             callbacks=[earlyStopping])
            
            history_keys = list(train_history.history.keys())
            validation_loss_theta.append(np.mean(train_history.history[history_keys[1]][-5:]))
            validation_loss_response.append(np.mean(train_history.history[history_keys[2]][-5:]))
            training_loss_theta.append(np.mean(train_history.history[history_keys[6]][-5:]))
            training_loss_response.append(np.mean(train_history.history[history_keys[7]][-5:]))

        #X_pred = np.hstack((X_pred, deepFowardFeedNets.predict_on_batch(X)[0]))
            ability_pred = np.hstack((ability_pred, projectTheta.predict_on_batch(X)))
            
            del deepFowardFeedNets, projectTheta
            K.clear_session()
            
            iteration += 1

            
        ability_pred_1 = np.apply_along_axis(arr =ability_pred[:, 1:],
                                             axis= 1, func1d= np.mean)


        print(np.median(validation_loss_theta), np.median(validation_loss_response),
              np.median(training_loss_theta), np.median(training_loss_response),
              np.median(validation_loss_theta)*weight_theta, np.median(validation_loss_response)*(100-weight_theta),
              ability_pred_1.mean())
        
        if(weight == 0):
            X_pred_weight = np.array([np.median(validation_loss_theta), 
                                      np.median(validation_loss_response),
                                      np.median(training_loss_theta), 
                                      np.median(training_loss_response), 
                                      np.median(validation_loss_theta)*weight_theta, np.median(validation_loss_response)*(100-weight_theta),
                                      ability_pred_1.mean(), 
                                      weight])
        else:
            X_pred_weight = np.vstack((X_pred_weight, 
                                      np.array([np.median(validation_loss_theta),
                                                np.median(validation_loss_response),
                                                np.median(training_loss_theta),
                                                np.median(training_loss_response),
                                                np.median(validation_loss_theta)*weight_theta, np.median(validation_loss_response)*(100-weight_theta),
                                                ability_pred_1.mean(),
                                                weight])))
        
    np.savetxt(X=X_pred_weight,
               delimiter=',', 
               header = 'validation_loss_theta, validation_loss_response, train_loss_theta, training_loss_response, ability_avg, weight',
               fname=os.path.join(Save_path, domain+'_weighted_estimate.csv'))




