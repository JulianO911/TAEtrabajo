import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import io

def lecturaDatos(datos):
  accidentes = pd.read_csv(datos,encoding="latin-1")
  accidentes = accidentes.drop("CBML",axis=1)
  return accidentes

#seleccion de datos que tienen barrio y eliminación de NAs
def accidentesConBarrio(accidentes):
  accidentesBarrio = accidentes[~accidentes["BARRIO"].isnull()]
  accidentesBarrio = accidentesBarrio.dropna()
  return accidentesBarrio

from sklearn import preprocessing
#Conversión de los datos que son texto a número
def convert(Data):
  for column in Data.columns:
    number = preprocessing.LabelEncoder()
    if column != "NRO_RADICADO":
      Data[column] = number.fit_transform(Data[column])
  return Data

def prediccionDeBarrios(accidentesBarrio,accidentes):
# Selección de conjuntos de entrada y salida para entrenamiento y validación
  X = accidentesBarrio[["AÑO","CLASE_ACCIDENTE","COMUNA","HORA","LONGITUD","LATITUD"]]
  y = accidentesBarrio["BARRIO"]
  X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=0,train_size=0.7)
  y_train = y_train.astype("category")
  X_train = convert(X_train)
  #Normalización de datos de entrenamiento y validación
  from sklearn.preprocessing import StandardScaler
  scale = StandardScaler()
  X_train = scale.fit_transform(X_train) 
  X_test = convert(X_test)
  scale = StandardScaler()
  X_test = scale.fit_transform(X_test) 
  from sklearn.neighbors import KNeighborsClassifier
  #Entrenamiento y validación con KNN
  knn = KNeighborsClassifier(n_neighbors = 5)
  knn.fit(X_train, y_train)
  knn.score(X_train, y_train)
  knn.score(X_test, y_test)
  #Seleccion de observaciones que no tienen barrio y normalización de estos
  accidentesBarrioNull = accidentes[accidentes["BARRIO"].isnull()]
  accidentesBarrioNull = accidentesBarrioNull[["AÑO","CLASE_ACCIDENTE","COMUNA","HORA","LONGITUD","LATITUD"]]
  accidentesBarrioNull = convert(accidentesBarrioNull)
  scale = StandardScaler()
  accidentesBarrioNull = scale.fit_transform(accidentesBarrioNull) 
  #Predicción para imputar los barrios
  barriosPred = knn.predict(accidentesBarrioNull)
  return barriosPred

#Imputación de los barrios en el dataset original y escritura en un archivo .csv"
def imputacionBarrios(barriosPred,accidentes):
  accidentes.loc[accidentes["BARRIO"].isnull(),"BARRIO"] = barriosPred
  accidentes[accidentes["BARRIO"].isnull()]
  accidentes["BARRIO"] = accidentes.BARRIO.astype(str)
  accidentesNuevos = accidentes.iloc[:,1:]
  accidentesNuevos.to_csv('accidentesConBarrio3.csv',index=False)
