#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 28 13:35:15 2022

@author: andres
"""

#El procesamiento de la información fue realizado en python y las estimaciiones en R
#Librerias utilizadas para el preprocesamiento 
import pandas as pd 
import numpy as np 
from datetime import datetime, timedelta
from sklearn.preprocessing import MinMaxScaler
#%%

#Variables 
#Control 
#En este parte se separar las variables según el uso que se les daran (Control, separacion e índices)

otras = { 'DIAREAL':'DIAREAL', 'MESREAL':'MESREAL', 'IDENPA':'IDENPA'}

control = {'Gobierno_Opocicion':'PERPART',
           'Edad':'REEDAD', 'Educacion_Entrevistado': 'REEDUC.1', 
           'Educacion_padres': 'REEDUC.2', 'Nivel_Socioeconomico (Entrevistado)':'S26',
           'Estudios_padres_2': 'S11', 'Estudio_entrevistado':'S10', 
           'Raza(Negro)': 'S6', 'Edad_2':'EDAD', 'SEXO':'SEXO', 'LUZ': 'P31NI', 
           'Clase_Sub': 'S1', 'Ocupación':'S14A', 'Ocupación activos':'S15', 
           'Estado_Civil':'S23', 'Beneficiario': 'S25', 'Migracion_Cambio_1_2a_0_1':'S16', 'ponde':'WT', 
           'Principal_ingreso':'S8'
           }
extras = ['S21', 'S12'] # Bienes, redes sociales

#Gobierno

gobierno = {'Confianza_Fuerzas_Armadas':'P15STGBSC.A','Confianza_Policía':'P15STGBSC.B',
            'Confianza_Iglesia':'P15STGBSC.C','Confianza_Congreso':'P15STGBSC.D',
            'Confianza_Gobierno':'P15STGBSC.E','Confianza_Poder_Judicial':'P15STGBSC.F',
            'Confianza_Partidos_Políticos':'P15STGBSC.G','Confianza_Electoral':'P15STGBSC.H',
            'Confianza_Bancos':'P16NC.F',
            'Aprobación_gobierno_presidente(nombre)':'P20STGBSC',
            }
#Economia 
economia = {'Situacion_Economica':'P6STGBSC', 'Situacion_Economia_Futura':'P8STIC',
            'Confianza_interpersonal': 'P11STGBS', 'Satisfaccion_funcionamiento_economia':'P13STGBS.B',
            'Integracion_politca_sino':'P39ST.B',
            'Nac_menor_inter_mayor':'P52N.C'}
#%%
# En las partes de las funciones, se encuentran diferentes funcion que se basaron en la segmentacion
# para el pais, la fecha y el cambio en la denominacion de algúnas preguntas.
#Fuciones 
def Transform_date(df):
    df.reset_index(inplace =True, drop =True)
    df['Fecha_lar'] = int()
    for i in range(df.shape[0]): 
                day = df.at[i, 'DIAREAL']
                mes = df.at[i, 'MESREAL']
                año = 18
                dte = '{}-{}-{}'.format(day, mes, año)
                df.at[i, 'Fecha_lar'] = datetime.strptime(dte, '%d-%m-%y')


def PaisNum(df):
    df['IDENPA_num'] = int()
    for i in range(df.shape[0]):    
        if df.at[i, 'IDENPA'] == 170:
            df.at[i, 'IDENPA_num'] = 1
        elif df.at[i, 'IDENPA'] == 218:
            df.at[i, 'IDENPA_num'] = 2
        elif df.at[i, 'IDENPA'] == 862:
            df.at[i, 'IDENPA_num'] = 3
        elif df.at[i, 'IDENPA'] == 604:
            df.at[i, 'IDENPA_num'] = 4
            
def FechaNum(dfx):
    #Nueva estructura del codigo:
    u = list(gobierno.values()) + list(economia.values()) + list(control.values())+list(otras.values())
    dta = data[u]
    Transform_date(dta)
    valores = dta['Fecha_lar'].value_counts()
    valores = valores.reset_index()
    valores.sort_values('index',inplace =True)
    valores = valores.reset_index(drop = True)
    valores = valores.reset_index(drop = False)
    dfx['Fecha_num'] = int()

    for i in range(valores.shape[0]): 
        for j in range(dfx.shape[0]): 
            if dfx.at[j, 'Fecha_lar'] ==  valores.at[i, 'index']:
                dfx.at[j,'Fecha_num'] = valores.at[i,'level_0']
def Dummy(df): 
    df['T_C'] = int()
    df['B_A'] = int()
    df['hombre'] = int()
    df['mujer'] = int()
    df['SEXO'].replace(2, 0, inplace= True)
    df['Estrato'] = int()
    df['S25'].replace(2,0, inplace =True)
    fecha_importante = fecha_importante = '03-07-18'
    p = datetime.strptime(fecha_importante, '%d-%m-%y')
    for i in range(df.shape[0]): 
        if df.at[i, 'Fecha_lar'] >= p: 
            df.at[i, 'B_A'] = 1
        else: 
            df.at[i, 'B_A'] = 0
        if df.at[i, 'IDENPA_num'] == 1: 
            df.at[i, 'T_C'] = 1
        else:
            df.at[i, 'T_C'] = 0

        if df.at[i,'SEXO'] == 1: 
            df.at[i, 'hombre'] = 1
            df.at[i, 'mujer'] = 0
        else: 
            df.at[i, 'hombre'] = 0
            df.at[i, 'mujer'] = 1
        if df.at[i, 'S1'] >3: #Esto depende de la variable que se utilice S1. Subjetiva S26 Contestada por el encuestados 
            df.at[i, 'Estrato'] = 1
        else: 
            df.at[i, 'Estrato'] = 0
def Educ(df): 
    df['prima'] = np.where(df['REEDUC.1'] ==1,1, 0)
    df['sec'] = np.where(df['REEDUC.1'] ==2,1, 0)
    df['Sup'] = np.where(df['REEDUC.1'] ==3,1, 0)

def Nulos(df):
    for i in df.columns: 
        print('Col: {} / Num Nan: {}'.format(i, df[i].isnull().sum()))
            
#%%
data = pd.read_csv('data_baro.csv')
#Segmentacion de los datos con las nuevas variables
nombres = list(control.keys()) + list(gobierno.keys()) + list(economia.keys()) + list(otras.keys())


#%%

#Con la finalidad de estimar el impacto utilizando las preguntas por separado, 
#se realizaron una base de datos por pregunta. La segmentacion se evidencia en el 
#bloque de codigo de abajo.
#Tener una base de datos por preguntas. 
lista = []
cols = list(gobierno.values()) + list(economia.values()) + list(control.values())
for i in cols: 
    cols2 = np.append(np.array(list(otras.values())),i)
    cols2 = cols2
    d = pd.DataFrame(data[cols2])
    d = d[(d.IDENPA == 218) | (d.IDENPA == 170) | (d.IDENPA == 604) | (d.IDENPA == 862)]
    # scaler = MinMaxScaler().fit(pd.DataFrame(d[i]))
    # d[i] = pd.DataFrame(scaler.transform(pd.DataFrame(d[i])), columns = [i])
    for j in d.columns: 
        #Remplazar por pregunta
        d[j] = d[j].replace([-1, -2, -3, -4], [None, None, None, None]) 
    d.dropna(inplace =True, how = 'any')
    d.reset_index(drop=True, inplace=True)
    if i == 'SEXO' or i == 'P20STGBSC': 
        d[i].replace(2, 0, inplace= True)
    if i not in np.array(list(control.values())):
        d[i] = (d[i] - d[i].min()) / (d[i].max() - d[i].min())
    
     
    Transform_date(d)
    dt = d.groupby(['Fecha_lar', 'IDENPA']).mean()
    dt = pd.DataFrame(dt[i])
    lista.append(d)

for i in range(len(lista)):
    if i >= 24: 
        break
    if i == 0:
        prueba = lista[0].merge(lista[1], how='left',left_index=True, right_index=True)
    prueba = prueba.merge(lista[i+2], how='left',left_index=True, right_index=True)

prueba.reset_index(inplace = True)

FechaNum(prueba)
PaisNum(prueba)
Dummy(prueba)
prueba = prueba[(prueba.Fecha_num >=8) &(prueba.Fecha_num <=30)]

prueba.to_excel('Base_Datos_01042022.xlsx')


#%%

#Bloque donde se evidencia la base de datos por pregunta
lista = []
cols = list(gobierno.values()) + list(economia.values())
for i in cols: 
    cols2 = np.append(np.array(list(otras.values())+list(control.values())),i)
    cols2 = cols2
    d = pd.DataFrame(data[cols2])
    d = d[(d.IDENPA == 170) | (d.IDENPA == 604)]
    # scaler = MinMaxScaler().fit(pd.DataFrame(d[i]))
    # d[i] = pd.DataFrame(scaler.transform(pd.DataFrame(d[i])), columns = [i])
    for j in d.columns: 
        #Remplazar por pregunta
        d[j] = d[j].replace([-1, -2, -3, -4], [None, None, None, None]) 
    d.dropna(inplace =True, how = 'any')
    d.reset_index(drop=True, inplace=True)
    if i not in list(control.values()):
        d[i] = (d[i] - d[i].min()) / (d[i].max() - d[i].min())
    Transform_date(d)
    PaisNum(d)
    FechaNum(d)
    p = datetime.strptime('03-07-18', '%d-%m-%y')
    d = d[(d.Fecha_lar >= p - timedelta(days=4)) & (d.Fecha_lar <= p + timedelta(days=5))]
    d.reset_index(drop=True, inplace =True)
    Dummy(d)
    d.to_excel('./Bases_Dif_n1/{}.xlsx'.format(i))
    
    lista.append(d)
    

#%%
#Buscando realizar le balance de las medias, se creo una base de datos para 
#estimar las medias de diferentes variables demograficas (Control) para 
#todos los individuos de la muestra.
#Crear una base de datos unicamente para realizar la medicion de las medias
import math
medias =  data[(data.IDENPA == 604) | (data.IDENPA == 170)]# |(data.IDENPA == 218) ]#data[(data.IDENPA == 218) | (data.IDENPA == 170) | (data.IDENPA == 604) | (data.IDENPA == 862)]
medias = medias[list(control.values())+list(otras.values())]
for j in medias.columns: 
        medias[j] = medias[j].replace([-1, -2, -3, -4], [None, None, None, None]) 
medias['Id'] = np.where(medias['IDENPA'] !=170, 'Control', 'Tratment')
medias['SEXO'].replace(2,0, inplace =True)
# medias['P31NI'] =  np.log2(medias['P31NI'])
medias['S23'].replace([2,3], [0,0], inplace = True)
medias['S14A'].replace([2,3,4,5,6,7], [1,1,0,0,0,0], inplace =True)
medias['S25'].replace(2,0, inplace = True)
medias['S16'].replace([2], [0], inplace = True)
medias['S8'].replace(2,0, inplace = True)
Educ(medias)
medias.to_excel('Prueba_Medias.xlsx')

for i in medias.columns: 
    print('Col: {} / Num Nan: {}'.format(i, medias[i].isnull().sum()))

#%%
#Indice con las pregumtas que fueron significativas
#Utilizando las variables que obtuvieron una significancia estadistica, 
#se realizaron los indices sin tener en cuenta variables de control
#Primero las de gobierno, y luego economia
preguntas = {'Aprobacion(.)': 'P20STGBSC' ,'Confianza_Partidos(**)':'P15STGBSC.G',
             'Confianza_PoderJ(*)':'P15STGBSC.F', 'Confianza_FuerzasA(*)':'P15STGBSC.A'
             
      #'Aprobacion(.)': 'P20STGBSC',       
             }
p_e= {'Situacion_eco_actual':'P6STGBSC', 'Situacion_futura':'P8STIC', 'Satisfaccion_Eco':'P13STGBS.B'}



def IndicePrep(dicc): 
    
    indice = data[(data.IDENPA == 604) | (data.IDENPA == 170)]
    indice = indice[list(control.values())+list(otras.values())+list(dicc.values())]
    for j in indice.columns: 
        #Remplazar por pregunta
        
        if j in list(dicc.values()):
            indice[j] = indice[j].replace([-1, -2, -3, -4], [None, None, None, None])
            indice.dropna(inplace =True, how = 'any')
            indice.reset_index(drop=True, inplace=True)
            indice[j] = (indice[j] - indice[j].min()) / (indice[j].max() - indice[j].min())
    return indice

indice_gov = IndicePrep(preguntas)
        


indice_gov['Gov'] = int()
indice_gov['Gov'] = (indice_gov['P20STGBSC']+ indice_gov['P15STGBSC.G']+ indice_gov['P15STGBSC.F']+ indice_gov['P15STGBSC.A'])/4



Transform_date(indice_gov)
FechaNum(indice_gov)
PaisNum(indice_gov)
Dummy(indice_gov)
Educ(indice_gov)
indice_gov = indice_gov[( indice_gov.Fecha_num>=15)&( indice_gov.Fecha_num<=26)]
indice_gov['Id'] = np.where(indice_gov['IDENPA'] !=170, 'Control', 'Tratment')
indice_gov['S23'].replace([2,3], [0,0], inplace = True)
indice_gov['S14A'].replace([2,3,4,5,6,7], [1,1,0,0,0,0], inplace =True)
indice_gov['S25'].replace(2,0, inplace = True)
indice_gov['S16'].replace([2], [0], inplace = True)
indice_gov['S8'].replace(2,0, inplace = True)

indice_gov.to_excel('indice_gov.xlsx')

indice_eco = IndicePrep(p_e)

indice_eco['Eco'] = (indice_eco['P6STGBSC']+ indice_eco['P8STIC']+ indice_eco['P13STGBS.B'])/3
Transform_date(indice_eco)
FechaNum(indice_eco)
PaisNum(indice_eco)
Dummy(indice_eco)
indice_eco= indice_eco[( indice_eco.Fecha_num>=15)&( indice_eco.Fecha_num<=26)]
indice_eco.to_excel('indice_eco.xlsx')

#%%
#Se realiza el mismo proceso de arriba pero teniendo en cuenta las variables de control
#Ahora una base de datos con los respectivos controles (Correr solo si se va a estimar la regresion con las variables de control)
control = {
            
            'Nivel_Socioeconomico (Entrevistado)':'S26',
           'Estudios_padres_2': 'S11', 'Estudio_entrevistado':'S10', 
           'Raza(Negro)': 'S6', 'Edad_2':'EDAD', 'SEXO':'SEXO', 
           'Clase_Sub': 'S1', 'Ocupación':'S14A', 
           'Estado_Civil':'S23', 'Edu_seg':'REEDUC.1', 'Beneficiario':'S25', 'Principal_Ingreso':'S8'
           }
preguntas = {'Aprobacion(.)': 'P20STGBSC' ,'Confianza_Partidos(**)':'P15STGBSC.G',
             'Confianza_PoderJ(*)':'P15STGBSC.F', 'Confianza_FuerzasA(*)':'P15STGBSC.A'
             
      #'Aprobacion(.)': 'P20STGBSC',       
             }
p_e= {'Situacion_eco_actual':'P6STGBSC', 'Situacion_futura':'P8STIC', 'Satisfaccion_Eco':'P13STGBS.B'}

# bienes_col = {'Casa propia':'S21.A', 'Computador':'S21.B', 'Lavarropas':'S21.C', 'Teléfono Red Fija':'S21.E',
# 'Teléfono celular/móvil':'S21.F', 'Auto':'S21.G', 'Agua caliente':'S21.I', ' Alcantarillado/Cloacas':'S21.J',
# 'Al menos una comida caliente al día':'S21.K', 'Agua potable':'S21.L', ' Smartphone':'S21.M',
# 'Conexión a Internet en el hogar':'S21.N', 'Conexión a Internet en el hogar':'S21.O', 'Calefaccion':'S21.P'}

def IndicePrep(dicc): 
    
    indice = data[(data.IDENPA == 604) | (data.IDENPA == 170)]
    indice = indice[list(control.values())+list(otras.values())+list(dicc.values())]
    base = list(dicc.values()) + list(control.values())
    for j in indice.columns: 
        #Remplazar por pregunta
        
        if j in base:
            
            indice[j] = indice[j].replace([-1, -2, -3, -4], [None, None, None, None])
            indice.dropna(inplace =True, how = 'any')
            indice.reset_index(drop=True, inplace=True)
            if j in list(dicc.values()):
                indice[j] = (indice[j] - indice[j].min()) / (indice[j].max() - indice[j].min())
    indice['S23'].replace([2,3], [0,0], inplace = True)
    indice['S14A'].replace([2,3,4,5,6,7], [1,1,0,0,0,0], inplace =True)
    
    return indice

indice_gov = IndicePrep(preguntas)
        


indice_gov['Gov'] = int()
indice_gov['Gov'] = (indice_gov['P20STGBSC']+ indice_gov['P15STGBSC.G']+ indice_gov['P15STGBSC.F']+ indice_gov['P15STGBSC.A'])/4



Transform_date(indice_gov)
FechaNum(indice_gov)
PaisNum(indice_gov)
Dummy(indice_gov)
indice_gov = indice_gov[( indice_gov.Fecha_num>=16)&( indice_gov.Fecha_num<=26)]
Educ(indice_gov)
indice_gov.to_excel('indice_gov_co.xlsx')
indice_eco = IndicePrep(p_e)

indice_eco['Eco'] = (indice_eco['P6STGBSC']+ indice_eco['P8STIC']+ indice_eco['P13STGBS.B'])/3
Transform_date(indice_eco)
FechaNum(indice_eco)
PaisNum(indice_eco)
Dummy(indice_eco)
indice_eco= indice_eco[( indice_eco.Fecha_num>=16)&( indice_eco.Fecha_num<=26)]
Educ(indice_eco)
indice_eco.to_excel('indice_eco_co.xlsx')
# medias['S23'].replace([2,3], [0,0], inplace = True)
# medias['S14A'].replace([2,3,4,5,6,7], [1,1,0,0,0,0], inplace =True)

#%%

#En esta parte se obtienen la base de datos sin valores nulos.

preguntas = {'Aprobacion(.)': 'P20STGBSC' ,'Confianza_Partidos(**)':'P15STGBSC.G',
             'Confianza_PoderJ(*)':'P15STGBSC.F', 'Confianza_FuerzasA(*)':'P15STGBSC.A'
             
      #'Aprobacion(.)': 'P20STGBSC',       
             }
p_e= {'Situacion_eco_actual':'P6STGBSC', 'Situacion_futura':'P8STIC', 'Satisfaccion_Eco':'P13STGBS.B'}
def IndicePrep(dicc): 
    lista = []
    indice = data[(data.IDENPA == 604) | (data.IDENPA == 170)]
    indice = indice[list(control.values())+list(otras.values())+list(dicc.values())]
    Transform_date(indice)
    PaisNum(indice)
    FechaNum(indice)
    Dummy(indice)
    pre = []
    for j in indice.columns: 
        #Remplazar por pregunta
        
        if j in list(dicc.values()):
            indice[j] = indice[j].replace([-1, -2, -3, -4], [None, None, None, None])
            # indice.dropna(inplace =True, how = 'any')
            # indice.reset_index(drop=True, inplace=True)
            indice[j] = (indice[j] - indice[j].min()) / (indice[j].max() - indice[j].min())
            
            # u = pd.DataFrame(indice[[j,'Fecha_num', 'IDENPA_num', 'Fecha_lar', 'IDENPA']])
            # p = datetime.strptime('03-07-18', '%d-%m-%y')
            # u = u[(u.Fecha_lar >= p - timedelta(days=4)) & (u.Fecha_lar <= p + timedelta(days=5))]
            # u['Pregunta'] = '{}'.format(j)
            # u.reset_index(drop =True, inplace = True)
            pre.append(j)
    cul = pre + ['Fecha_num', 'T_C']
    base = indice[cul]

    return base
l = IndicePrep(p_e)
g = IndicePrep(preguntas)

l.to_excel('Economia.xlsx')
g.to_excel('Gobierno.xlsx')



#%%
#Paso extra donde queriamos ver la posibilidad de utilizar otras variables
#Base de datos de los bienes 

bienes_col = {'Casa propia':'S21.A', 'Computador':'S21.B', 'Lavarropas':'S21.C', 'Teléfono Red Fija':'S21.E',
'Teléfono celular/móvil':'S21.F', 'Auto':'S21.G', 'Agua caliente':'S21.I', ' Alcantarillado/Cloacas':'S21.J',
'Al menos una comida caliente al día':'S21.K', 'Agua potable':'S21.L', ' Smartphone':'S21.M',
'Conexión a Internet en el hogar':'S21.N', 'Conexión a Internet en el hogar':'S21.O', 'Calefaccion':'S21.P'}


bienes = data[(data.IDENPA == 604) | (data.IDENPA == 170)]
bienes = bienes[bienes_col]
Nulos(bienes)
for j in bienes.columns: 
    bienes[j] = bienes[j].replace([-1, -2, -3, -4], [None, None, None, None])
    bienes.dropna(inplace =True, how = 'any')
    bienes.reset_index(drop=True, inplace=True)
    bienes[j].replace(2,0, inplace =True)
    


#%%

#Agrupamos las medias de los valores por dia. Estos datos son utilizados para crear la grafica de tendencias paralelas.
colombia_gov = indice_gov[indice_gov.IDENPA == 170].groupby(['Fecha_lar']).mean()
peru_gov = indice_gov[indice_gov.IDENPA == 604].groupby(['Fecha_lar']).mean()

colombia_eco = indice_eco[indice_eco.IDENPA == 170].groupby(['Fecha_lar']).mean()
peru_eco = indice_eco[indice_eco.IDENPA == 604].groupby(['Fecha_lar']).mean()


colombia_gov.to_excel('col_gov.xlsx')
peru_gov.to_excel('per_gov.xlsx')

colombia_eco.to_excel('col_eco.xlsx')
peru_eco.to_excel('per_eco.xlsx')

#%%
