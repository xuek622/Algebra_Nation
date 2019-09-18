
import numpy as np
import pandas as pd
import os


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
                          bias_constraint = min_max_norm(max_value = 0, min_value = -0.4),
                          kernel_initializer= 'ones', 
                          bias_initializer = 'zeros')(theta_layer)
                          #bias_initializer= initializers.Constant(value= -0.05 )
                          
    
    response_outputs = Dense(input_shape, 
    kernel_constraint= min_max_norm(max_value = 3, min_value = 1),
    bias_constraint = min_max_norm(max_value = 0, min_value = -3),
    kernel_initializer= initializers.Constant(value=1.5), 
    bias_initializer= initializers.Constant(value=0), activation = 'sigmoid')(theta_outputs)
    
    deepFowardFeedNets = Model(inputs, output = [theta_outputs, response_outputs])
    projectTheta = Model(inputs, theta_layer)
    
    return deepFowardFeedNets, projectTheta




Data_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Simulated_Data")

Save_path = os.path.join('/Users','kangxue', "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Refined_Theta")

files = os.listdir(Data_path)
domains = ["571", "572", "573", "574", "577", "578", "579", "580", "581", "582"]

#weights = [45, 30, 11, 35, 40, 30, 30, 15, 23, 14] # best weight after selecting



for domain_index in range(0,1):
    domain = domains[domain_index]
    weight_theta = weights[domain_index]
    #weight_theta = 50


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

    X_pred = Theta_est.copy()
    iteration = 0
    iter_max = 5


    while iteration < iter_max:
        print(iteration)
        deepFowardFeedNets, projectTheta = create_dfn(X.shape[1])

        earlyStopping = callbacks.EarlyStopping(monitor='val_loss', patience=2, verbose=0, mode='auto')

        deepFowardFeedNets.compile(optimizer=optimizers.Adam(), 
                                    loss=["mean_squared_error", "binary_crossentropy"], 
                                    loss_weights=[weight_theta, 100 - weight_theta], 
                                    metrics=['accuracy'])
        max_epochs = 300
        batch_size = 200

        history = deepFowardFeedNets.fit(X_train, [Y_train,  X_train], 
                                             epochs=max_epochs, batch_size=batch_size,
                                             shuffle=True, verbose = 0, 
                                             validation_data = (X_test, [Y_test, X_test]),
                                             callbacks=[earlyStopping])

        #X_pred = np.hstack((X_pred, deepFowardFeedNets.predict_on_batch(X)[0]))
        X_pred = np.hstack((X_pred, projectTheta.predict_on_batch(X)))
        iteration += 1

    X_pred_1 = np.apply_along_axis(arr = X_pred[:, 1:], axis= 1, func1d= np.mean)

    print(mean_squared_error(Theta_true, Theta_est), mean_squared_error(Theta_true, X_pred_1))
    print(Theta_true.mean(), Theta_est.mean(), X_pred_1.mean())

    print(Theta_true.var(), Theta_est.var(), X_pred_1.var())

   # np.savetxt(X=X_pred_1,delimiter=',', fname=os.path.join(Save_path, domain +'_refined_theta_preliminary.csv'))




