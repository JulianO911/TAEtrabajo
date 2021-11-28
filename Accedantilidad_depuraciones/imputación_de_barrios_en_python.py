
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import io
from sklearn import preprocessing
def convert(Data):
  for column in Data.columns:
    number = preprocessing.LabelEncoder()
    if column != "NRO_RADICADO":
      Data[column] = number.fit_transform(Data[column])
  return Data
def modelo():

  accidentes = pd.read_csv('accidentes.csv',encoding="latin-1")

  accidentesBarrio = accidentes[~accidentes["BARRIO"].isnull()]
  accidentesBarrio = accidentesBarrio.drop(["Unnamed: 0","CBML","DIRECCION.ENCASILLADA","DISEÑO","EXPEDIENTE","LOCATION"],axis=1)
  accidentesBarrio = accidentesBarrio.dropna()

  X = accidentesBarrio[["AÑO","CLASE_ACCIDENTE","COMUNA","HORA","X","Y"]]
  y = accidentesBarrio["BARRIO"]
  X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=0,train_size=0.7)

  return X_test
