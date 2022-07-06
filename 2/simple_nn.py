#=======================================#
#         Simple NN with TF             #
#          Regression Case              #
#           2022-07-06                  #
#=======================================#



#%%
import pandas as pd 
import numpy as np
import tensorflow as tf 
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings("ignore")
## define variables -----

y =  np.array([4.1,4.0,4.19,4.2],dtype=float)
x =  np.array([1.0,2.0,3.0,4.0],dtype=float)

## Define model -------
def random_model(x,y):
    model = tf.keras.Sequential([tf.keras.layers.Dense(units=3, input_shape=[1])])
    model.compile(optimizer=tf.keras.optimizers.Adam(0.3), loss='mean_squared_error')
    model.fit(x, y, epochs=3000)
    return model

model = random_model(x,y)

plt.xlabel("# Epochs")
plt.ylabel("Loss dim")
plt.plot(model.history.history['loss'])

new_y = 5.0
prediction = model.predict([new_y])[0]
print(prediction[0])


# %%
