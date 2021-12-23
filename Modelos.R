### TFM 
### Desarrollo de una aplicación web para la predicción de la salud lumbar, aplicando técnicas de aprendizaje automático sobre las características biomecánicas de pacientes ortopédicos.

#Cargamos datos:

#Fuente de los datos:
#https://www.kaggle.com/uciml/biomechanical-features-of-orthopedic-patients#column_3C_weka.csv
file.choose()
med_biometricas<-read.csv("C:\\Users\\damar_000\\Documents\\UOC\\CUARTO SEMESTRE\\column_3C_weka.csv")


#ANALISIS Y PREPROCESAMIENTO DE LOS DATOS
str(med_biometricas)
summary(med_biometricas)

#Convertimos la clase en factor
med_biometricas$class<-as.factor(med_biometricas$class)
str(med_biometricas)
summary(med_biometricas)
head(med_biometricas)

#Cambiamos el nombre de los niveles de la clase a "Hernia", "Normal" y "Espondilolistesis"
med_biometricas$class<-factor(med_biometricas$class, labels = c("Hernia", "Normal", "Espondilolistesis"))
table(med_biometricas$class)# Hernia: 60, Normal: 100, Espondilo.: 150
#Porcentajes de cada diagnostico en los datos
round(prop.table(table(med_biometricas$class))*100, digits=1)# Hernia: 19.4%, Normal: 32.3%, Espondilo.: 48.4%

#Boxplots
#todos los parámetros:
boxplot(med_biometricas$pelvic_incidence,med_biometricas$pelvic_tilt,med_biometricas$lumbar_lordosis_angle,med_biometricas$sacral_slope,med_biometricas$pelvic_radius,med_biometricas$degree_spondylolisthesis, col="lightblue")
#Por diagnóstico:
boxplot(med_biometricas$pelvic_incidence~med_biometricas$class)
boxplot(med_biometricas$pelvic_tilt~med_biometricas$class)
boxplot(med_biometricas$lumbar_lordosis_angle~med_biometricas$class)
boxplot(med_biometricas$sacral_slope~med_biometricas$class)
boxplot(med_biometricas$pelvic_radius~med_biometricas$class)
boxplot(med_biometricas$degree_spondylolisthesis~med_biometricas$class)

#Histogramas
hist(med_biometricas$pelvic_incidence)
hist(med_biometricas$pelvic_tilt)
hist(med_biometricas$lumbar_lordosis_angle)
hist(med_biometricas$sacral_slope)
hist(med_biometricas$pelvic_radius)
hist(med_biometricas$degree_spondylolisthesis)


#Estructura final de los datos: 
str(med_biometricas)
summary(med_biometricas)

#Dividir la base de datos en train y test antes de normalizar
set.seed(240291) #semilla de aleatoriedad para garantizar resultados reproducibles
dat_aleat<-sample(2,nrow(med_biometricas),replace = T, prob = c(0.70,0.30))
table(dat_aleat)
train<-med_biometricas[dat_aleat==1,]
test<-med_biometricas[dat_aleat==2,]
table(train$class)# Hernia: 44, Normal: 66, Espondilo.: 105
table(test$class)# Hernia: 16, Normal: 34, Espondilo.: 45
round(prop.table(table(train$class))*100, digits = 1)# Hernia: 20.5, Normal: 30.7, Espondilo.: 48.8
round(prop.table(table(test$class))*100, digits = 1)# Hernia: 16.8, Normal: 35.8, Espondilo.: 47.4


#Balancear los datos

#Debido a que existen más observaciones de la clase "Espondilolistesis" (47%)
#"Normal" del 35,8% y "Hernia" del 16,8%, esto puede sesgar las predicciones
#de los modelos desarrollados.

#install.packages("scutr")
library(scutr)

#Balancear datos con SCUT
set.seed(240291)
train_balanc<- SCUT(train,"class",undersample = undersample_kmeans)
table(train_balanc$class)# Hernia: 72, Normal: 72, Espondilo.: 72
train_balanc$class<-as.factor(train_balanc$class)
str(train_balanc)
summary(train_balanc)


#Al analizar los datos, se observó que 
#la escala en la que se mide cada característica es muy diferente,
#por lo que se necesita normalizar los datos.
#Se normaliza los datos para que cada característica tenga 
#el mismo impacto en la predicción del diagnóstico.
#Normalizacion mediante Z-score, se utilizará los parámetros obtenidos
#del set de entrenamiento para posteriormente normalizar los datos de test
#y futuros datos

#Medias y desviación estándar del set de entrenamiento 
#medias:
med_v1<-round(mean(train_balanc$pelvic_incidence), digits = 2)
med_v2<-round(mean(train_balanc$pelvic_tilt), digits = 2)
med_v3<-round(mean(train_balanc$lumbar_lordosis_angle), digits = 2)
med_v4<-round(mean(train_balanc$sacral_slope), digits = 2)
med_v5<-round(mean(train_balanc$pelvic_radius), digits = 2)
med_v6<-round(mean(train_balanc$degree_spondylolisthesis), digits = 2)
#sd:
sd_v1<-round(sd(train_balanc$pelvic_incidence), digits = 2)
sd_v2<-round(sd(train_balanc$pelvic_tilt), digits = 2)
sd_v3<-round(sd(train_balanc$lumbar_lordosis_angle), digits = 2)
sd_v4<-round(sd(train_balanc$sacral_slope), digits = 2)
sd_v5<-round(sd(train_balanc$pelvic_radius), digits = 2)
sd_v6<-round(sd(train_balanc$degree_spondylolisthesis), digits = 2)

#Normalizacion Z score
#Train
train_balanc$pelvic_incidence<-(train_balanc$pelvic_incidence - med_v1)/sd_v1
train_balanc$pelvic_tilt<-(train_balanc$pelvic_tilt - med_v2)/sd_v2
train_balanc$lumbar_lordosis_angle<-(train_balanc$lumbar_lordosis_angle - med_v3)/sd_v3
train_balanc$sacral_slope<-(train_balanc$sacral_slope - med_v4)/sd_v4
train_balanc$pelvic_radius<-(train_balanc$pelvic_radius - med_v5)/ sd_v5
train_balanc$degree_spondylolisthesis<-(train_balanc$degree_spondylolisthesis - med_v6)/sd_v6
head(train_balanc)
#Test
test$pelvic_incidence<-(test$pelvic_incidence - med_v1)/sd_v1
test$pelvic_tilt<-(test$pelvic_tilt - med_v2)/sd_v2
test$lumbar_lordosis_angle<-(test$lumbar_lordosis_angle - med_v3)/sd_v3
test$sacral_slope<-(test$sacral_slope - med_v4)/sd_v4
test$pelvic_radius<-(test$pelvic_radius - med_v5)/sd_v5
test$degree_spondylolisthesis<-(test$degree_spondylolisthesis - med_v6)/sd_v6
head(test)

#Datos normalizados
summary(train_balanc)
summary(test)
boxplot(train_balanc$pelvic_incidence,train_balanc$pelvic_tilt,train_balanc$lumbar_lordosis_angle,train_balanc$sacral_slope,train_balanc$pelvic_radius,train_balanc$degree_spondylolisthesis, col = "lightblue")
boxplot(test$pelvic_incidence,test$pelvic_tilt,test$lumbar_lordosis_angle,test$sacral_slope,test$pelvic_radius,test$degree_spondylolisthesis, col = "lightblue")

#Guardamos la clase en un vector
train_lab<-train_balanc[,7]
test_lab<-test[,7]
head(train_lab)
head(test_lab)


####kNN

#Construcción del modelo kNN
#se seleccionó k=13 ya que es aproximadamente la raíz de n (215)
library(class)
set.seed(240291)
knn_mod<- knn(train = train_balanc[-7], test = test[-7],
              cl= train_balanc$class , k=13, prob = T)
#Evaluar el modelo
library(gmodels)
CrossTable(x=test_lab, y=knn_mod,
           pro.chisq = F,prop.c = F, prop.r = F)
agreement_knn<-knn_mod == test_lab
table(agreement_knn) 
round(prop.table(table(agreement_knn)), digits = 2)
library(caret)
confusionMatrix(knn_mod,test_lab)
######Accuracy:0.7895, kappa: 0.6688, 
#Sens:0.8125-0.6471-0.8889 , Esp:0.8734-0.8852-0.9400
#curva ROC
library(ROCR)
library(pROC)
pred_knn_prob<-attr(knn_mod, "prob")
c_knn<-multiclass.roc(test_lab,pred_knn_prob)
c_knn
auc(c_knn)
#AUC:0.612


####Mejorar el modelo 
#con k= 15 porque es impar y elimina la posibilidad de un empate de "votos"
set.seed(240291)
knn_mod_mej<- knn(train = train_balanc[-7], test = test[-7],
                  cl= train_balanc$class, k=15,prob = T)
CrossTable(x=test_lab, y=knn_mod_mej,
           pro.chisq = F,prop.c = F, prop.r = F)
agreement_knn_mej<-knn_mod_mej == test_lab
table(agreement_knn_mej)
round(prop.table(table(agreement_knn_mej)), digits = 2)
confusionMatrix(knn_mod_mej,test_lab)
###### Accuracy:0.8, kappa: 0.6843
#Sens:0.8125-0.6765-0.8889 , Esp:0.8861-0.8852-0.9400
#curva ROC
pred_knn_mej_prob<-attr(knn_mod_mej, "prob")
c_knn_mej<-multiclass.roc(test_lab,pred_knn_mej_prob)
c_knn_mej
auc(c_knn_mej)
#AUC:0.6568

#Para comprobar otras distancias aplicadas en el algoritmo de knn
#se desarrolló un algoritmo desde cero para probar las distancias: Manhattan y 
#similitud coseno, se comprobó el funcionamiento de la función aplicando la
#distancia euclídea que se emplea en el algortimo knn de la librería "class".
#Se obtuvo los mismos resultados, comprobando el funcionamiento de la función 
#knn_distancias:::


#FUNCIONES DE DISTANCIAS:

#Distancia Euclídea
euclidean_distance = function(a, b){
  # Comprobamos que tienen la misma cantidad de observaciones
  if(length(a) == length(b)){
    sqrt(sum((a-b)^2))  
  } else{
    stop('Vectores no tienen el mismo número de variables')
  }
}

#Distancia de Manhattan
manhattan_distance = function(a, b){
  # Comprobamos que tienen la misma cantidad de observaciones
  if(length(a) == length(b)){
    sum(abs(a-b))
  } else{
    stop('Vectores no tienen el mismo número de variables')
  }
}

#Similitud Coseno
cos_similarity = function(a,b){
  if(length(a) == length(b)){
    num = sum(a *b, na.rm = T)
    den = sqrt(sum(a^2, na.rm = T)) * sqrt(sum(b^2, na.rm = T)) 
    result = num/den
    
    return(1-result)
  } else{
    stop('Vectores no tienen el mismo número de variables')
  }
}



# Función para determinar vecinos cercanos:
nearest_neighbors = function(x,obs, k, FUN, p = NULL){
  
  # Comprobar que los datos tengan igual número de variables 
  if(ncol(x) != ncol(obs)){
    stop('Los datos no tienen el mismo número de variables')
  }
  if(is.null(p)){
    dist = apply(x,1, FUN,obs)  
  }else{
    dist = apply(x,1, FUN,obs,p)
  }
  # Determinar vecinos cercanos:
  distances = sort(dist)[1:k]
  neighbor_ind = which(dist %in% sort(dist)[1:k])
  
  if(length(neighbor_ind)!= k){
    warning(
      paste('Intentar con k:',length(neighbor_ind))
    )
  }
  
  ret = list(neighbor_ind, distances)
  return(ret)
}


#Funcion de predicción knn:
knn_prediction = function(x,y, weights = NULL){
  
  x = as.matrix(x)
  
  if(is.factor(x[,y]) | is.character(x[,y])){
    groups = table(x[,y])
    pred = names(groups[groups == max(groups)])
  } 
  
  if(is.numeric(x[,y])){
    
    # Calculate weighted prediction
    if(!is.null(weights)){
      w = 1/weights/ sum(weights)
      pred = weighted.mean(x[,y], w)
      
      # Calculate standard prediction  
    }else{
      pred = mean(x[,y])
    }
    
  }
  if(try(class(x[,y])) == 'try-error'){
    stop('Y should be factor or numeric.')
  }
  return(pred)
}


# PROBANDO KNN CON DIFERENTES DISTANCIAS
knn_distancias= function(x_fit, x_pred, y, k, 
                         func = euclidean_distance,weighted_pred = F, p = NULL){
  
  # Inicilizamos las predicciones
  predictions = c()
  
  y_ind = which(colnames(x_pred) == y)
  
  # Para cada observacion, obtenemos la prediccion
  for(i in 1:nrow(x_pred)){
    
    neighbors = nearest_neighbors(x_fit[,-y_ind], 
                                  x_pred[i,-y_ind],k,FUN = func)
    
    if(weighted_pred){
      pred = knn_prediction(x_fit[neighbors[[1]], ],y, neighbors[[2]])
    } else{
      pred = knn_prediction(x_fit[neighbors[[1]], ],y)
    }
    
    # Incrementar en k
    if(length(pred)>1){
      pred = knn_distancias(x_fit, x_pred[i,],y, k = k+1, 
                            func = func, weighted_pred = weighted_pred, p == p)
    }
    
    predictions[i] = pred
    
  }
  return(predictions)
  
}

#Construyendo modelos con diferentes distancias en la función desarrollada "knn_distancias":

#DISTANCIA EUCLÍDEA
#Entrenando el modelo
set.seed(240291)
prueba_ed = knn_distancias(train_balanc,test,"class",k=13,func = euclidean_distance)
#Evaluando el modelo
prueba_ed<-as.factor(prueba_ed)
confusionMatrix(prueba_ed,test_lab)
################ AC:0.7895, k:0.6688 , 
#Sens: 0.8125-0.6471-0.8889, Esp: 0.8734-0.8852-0.9400 

#Modelo mejorado
prueba_ed_mej = knn_distancias(train_balanc,test,'class',k=15, func = euclidean_distance)
prueba_ed_mej<-as.factor(prueba_ed_mej)
confusionMatrix(prueba_ed_mej,test_lab)
################ AC:0.8, k:0.6843, 
#Sens.: 0.8125-0.6765-0.8889 , Esp.:0.8861-0.8852-0.9400  


#DISTANCIA MANHATTAN
#Entrenamiento
prueba_md = knn_distancias(train_balanc,test,'class',k=13,func = manhattan_distance)
#Evaluación
prueba_md<-as.factor(prueba_md)
confusionMatrix(prueba_md,test_lab)
################ AC:0.7789, k:0.6526,
#Sens:0.8125-0.5882-0.9111, Esp:0.8608-0.8852-0.9400 

#Modelo mejorado
prueba_md_mej = knn_distancias(train_balanc,test,'class',k=15, func = manhattan_distance)
prueba_md_mej<-as.factor(prueba_md_mej)
confusionMatrix(prueba_md_mej,test_lab)
################ AC:0.8, k:0.6853, 
#Sens:0.8750-0.6471-0.8889, Esp:0.8861-0.8852-0.9400 


#DISTANCIA SIMILITUD COSENO Similitud Coseno
#Entrenamiento
prueba_cs = knn_distancias(train_balanc,test,'class',k=22, func = cos_similarity)
#Evaluación
prueba_cs<-as.factor(prueba_cs)
confusionMatrix(prueba_cs,test_lab)
################ AC:0.8211, k:0.7191,
#Sens: 0.9375-0.6176-0.9333, Esp:0.8734-0.9344-0.9400

#Modelo mejorado
prueba_cs_mej = knn_distancias(train_balanc,test,'class',k=23, func = cos_similarity)
prueba_cs_mej<-as.factor(prueba_cs_mej)
confusionMatrix(prueba_cs_mej,test_lab)
################ AC:  0.8105, k:0.7035,
#Sens:0.9375-0.5882-0.9333, Esp:0.8608-0.9344-0.9400



###ARBOL DE DECISION
install.packages("C50")
library(C50)

#Modelo
set.seed(240291)
dt_model<-C5.0(train_balanc[-7],train_balanc$class)
dt_model
summary(dt_model)
#Evaluar modelo
pred_dt<-predict(dt_model,test)
CrossTable(test$class,pred_dt,
           prop.chisq = F, prop.c = F, prop.r = F)

agreement_dt<-pred_dt == test_lab
table(agreement_dt)
round(prop.table(table(agreement_dt)), digits = 2)
confusionMatrix(pred_dt,test_lab)
################Ac:0.8211, kappa: 0.7213, 
#Sens:0.8750-0.6765-0.9111, Esp:0.8608-0.9016-1.0000
#curva ROC
pred_dt_prob<-predict(dt_model,test,type='prob')
pred_dt_prob
str(pred_dt_prob)
head(pred_dt_prob)
library(pROC)
c_dt<-multiclass.roc(test_lab,pred_dt_prob)
c_dt
auc(c_dt)
#AUC:0.9256


###Mejorar el modelo
set.seed(240291)
dt_model_mej<-C5.0(train_balanc[-7],train_balanc$class, trials = 27)
dt_model_mej
summary(dt_model_mej)
pred_dt_mej<-predict(dt_model_mej,test)
CrossTable(test$class,pred_dt_mej,
           prop.chisq = F,prop.c = F,prop.r = F)
agreement_dt_mej<-pred_dt_mej == test_lab
table(agreement_dt_mej)
round(prop.table(table(agreement_dt_mej)), digits = 2)
confusionMatrix(pred_dt_mej,test_lab)
################AC:0.8211,  Kappa :0.7167, 
#Sens:0.6875-0.7059-0.9556, Esp:0.8734-0.8852-1.0000
#curva ROC
pred_dt_mej_prob<-predict(dt_model_mej,test,type='prob')
str(pred_dt_mej_prob)
head(pred_dt_mej_prob)
c_dtm<-multiclass.roc(test_lab,pred_dt_mej_prob)
auc(c_dtm)
#AUC: 0.9036


###SVM
install.packages("kernlab")
library(kernlab)
#Modelo con kernel= rbfdot
set.seed(240291)
svm_model<-ksvm(train_balanc$class~.,data=train_balanc,
                kernel="rbfdot",prob.model=TRUE)
svm_model
pred_svm<-predict(svm_model,test)
table(test$class,pred_svm)
CrossTable(test$class,pred_svm,
           prop.chisq = F,prop.c = F,prop.r = F)

agreement_svm<-pred_svm == test_lab
table(agreement_svm)
round(prop.table(table(agreement_svm)), digits = 2)
confusionMatrix(pred_svm,test_lab)
#################AC:0.8211, Kappa :0.7161,
#Sens:0.7500-0.7059-0.9333, Esp:0.8861-0.9016-0.9600
#curva ROC
pred_svm_prob<-predict(svm_model,test,type='prob')
str(pred_svm_prob)
head(pred_svm_prob)
c_svm<-multiclass.roc(test_lab,pred_svm_prob)
auc(c_svm)
#AUC: 0.918


###MEJORAR EL MODELO
#######################################Modelo seleccionado
#kernel= vanilladot
set.seed(240291)
svm_model_mej<-ksvm(train_balanc$class~.,data=train_balanc,
                    kernel="vanilladot",prob.model=TRUE)
svm_model_mej
pred_svm_mej<-predict(svm_model_mej,test)
table(test$class,pred_svm_mej)
CrossTable(test$class,pred_svm_mej,
           prop.chisq = F,prop.c = F,prop.r = F)
agreement_svm_mej<-pred_svm_mej == test_lab
table(agreement_svm_mej)
round(prop.table(table(agreement_svm_mej)), digits = 2)
confusionMatrix(pred_svm_mej,test_lab)
#################AC:0.8526,  Kappa : 0.7667, 
#Sens.:  0.7500-0.7647-0.9556, Esp.:0.8861-0.9180-1.0000
#curva ROC
pred_svm_prob_mej<-predict(svm_model_mej,test,type='prob')
str(pred_svm_prob_mej)
head(pred_svm_prob_mej)
c_svm_m<-multiclass.roc(test_lab,pred_svm_prob_mej)
auc(c_svm_m)
#AUC: 0.9407

####Guardamos elementos para desarrollar la app#####################
save(train_balanc, svm_model_mej, file = "data.RData")
#############################################################################

### RANDOM FOREST
install.packages("randomForest")
library(randomForest)

#Modelo
set.seed(240291)
rf_model<-randomForest(train_balanc$class~., data = train_balanc)
rf_model
#Evaluar modelo
pred_rf<-predict(rf_model,test[-7])
CrossTable(test$class,pred_rf,
           prop.chisq = F,prop.c = F,prop.r = F)

agreement_rf<-pred_rf == test_lab
table(agreement_rf)
round(prop.table(table(agreement_rf)), digits = 2)
confusionMatrix(pred_rf,test_lab)
#################AC:0.8211 , Kappa : 0.7163 , 
#Sens:0.6875-0.7353-0.9333, Esp:0.8861-0.8689-1.0000
#curva ROC
pred_rf_prob<-predict(rf_model,test,type='prob')
str(pred_rf_prob)
head(pred_rf_prob)
c_rf<-multiclass.roc(test_lab,pred_rf_prob)
auc(c_rf)
#AUC: 0.9208


####Mejorar modelo
set.seed(240291)
rf_model_mej<-randomForest(train_balanc$class~., data = train_balanc, ntree = 100)
rf_model_mej
pred_rf_mej<-predict(rf_model_mej,test[-7])

CrossTable(test$class,pred_rf_mej,
           prop.chisq = F,prop.c = F,prop.r = F)

agreement_rf_mej<-pred_rf_mej == test_lab
table(agreement_rf_mej)
round(prop.table(table(agreement_rf_mej)), digits = 2)
confusionMatrix(pred_rf_mej,test_lab)
#################AC:0.8316,  Kappa :0.7338,
#Sens:0.7500-0.7353-0.9333, Esp: 0.8861-0.8852-1.0000
#curva ROC
pred_rf_prob_mej<-predict(rf_model_mej,test,type='prob')
str(pred_rf_prob_mej)
head(pred_rf_prob_mej)
c_rfm<-multiclass.roc(test_lab,pred_rf_prob_mej)
auc(c_rfm)
#AUC: 0.9172
