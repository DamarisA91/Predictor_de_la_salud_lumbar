# Predictor_de_la_salud_lumbar
Desarrollo de una aplicación web para la predicción de la salud lumbar, aplicando técnicas de aprendizaje automático sobre las características biomecánicas de pacientes ortopédicos.


Aplicación web:  https://damaris24.shinyapps.io/Predictor_de_salud_lumbar/

La carpeta "App" contiene todos los elementos de la aplicación, abrir app.R y correr la aplicación. 
El archivo modelos.R contiene el análisis de los algoritmos de aprendizaje automático evaluados.

![image](https://user-images.githubusercontent.com/81890504/147360267-09f66612-7a51-4800-aabc-435641b12be1.png)


Se utilizó la base de datos “column3Cweka.csv”. Contiene 310 pacientes de los cuales; 60 pacientes tienen hernia de disco, 150 pacientes tienen espondilolistesis y 100 están sanos, por lo que están etiquetados con un diagnóstico “normal”. Se presentan 7 variables: incidencia pélvica (IP), inclinación pélvica (PT), pendiente sacra (SS), lordosis lumbar (LA), radio pélvico (PR), grado de espondilolistesis y diagnóstico.

La condición ortopédica de una persona se puede determinar a partir de sus características biomecánicas. El aprendizaje automático en el campo de la salud permite convertir datos clínicos, desde mediciones hasta imágenes, en conclusiones importantes para la toma de decisiones sobre el diagnóstico de enfermedades. Utilizando una base de datos de características biomecánicas de la columna, se evaluaron varios algoritmos de aprendizaje automático, el mejor modelo predictivo se obtuvo con el algoritmo de máquina de vectores de soporte (SVM), con una precisión del 85%. Utilizando este modelo se desarrolló una aplicación web con Shiny, en la que el usuario ingresa seis características biomecánicas de la columna y la aplicación devuelve el diagnóstico predicho por el modelo seleccionado. Esta aplicación fue desarrollada para facilitar a los médicos el diagnóstico de estas enfermedades, de manera que los pacientes puedan empezar de inmediato con el tratamiento adecuado.


TRABAJO DE FIN DE MÁSTER EN BIOINFORMÁTICA Y BIOESTADÍSTICA DE LA UOC. 
Damaris Alarcón - damaris.alarcon242@gmail.com
