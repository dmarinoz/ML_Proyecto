library(shiny)
#library(rsconnect)
library(class)
library(caTools)
library(caret)
library(randomForest)
library(tree)
library(e1071)  
library(naivebayes)
library(C50) 
library(VGAM)
library(neuralnet)

####################### IRIS DATASET ####################
dataset = iris
#Random Cross Validation
set.seed(2)
split = sample.split(dataset$Species, SplitRatio = 0.7)
summary(split)

#Trainning
trainning_set=subset(dataset,split==TRUE)

#Test
test_set=subset(dataset, split==FALSE)

#Escalado de los datos a otra dimesion
trainning_set[,1:4]=scale(trainning_set[,1:4])
test_set[,1:4]=scale(test_set[,1:4])

###########-----KNN-----###########
KnnTestPrediccion_k1 <- knn(trainning_set[,1:4], test_set[,1:4], trainning_set$Species, k = 3, prob = TRUE )

cmKNN = confusionMatrix(test_set[,5],KnnTestPrediccion_k1)
tablaKNN = t(cmKNN$table)
accuracyKNN = round(cmKNN$overall[1],3)
precisionKNN= round(cmKNN$byClass[,5],3) #con$byClass para todas las filas
recallKNN= round(cmKNN$byClass[,6],3)
f1KNN= round(cmKNN$byClass[,7],3)

TPR_KNN = mean(cmKNN$byClass[,1])
FPR_KNN = 1 - mean(cmKNN$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_KNN, y1 = TPR_KNN, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_KNN, y0 = TPR_KNN, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_KNN=((FPR_KNN*TPR_KNN)/2)+(((1-FPR_KNN)*(1-TPR_KNN))/2)+((1-FPR_KNN)*TPR_KNN)

###########-----Random Forest-----###########
decision_tree <- tree(Species ~ ., data = dataset) # Interpretation

#Funcion para random forest
iris_clasificadorRF <- randomForest(Species ~., 
                                    data = trainning_set, #train data set 
                                    importance = T)

#Prediciendo el test
predicted_table <- predict(iris_clasificadorRF, test_set[,-5])

cmRF = confusionMatrix(test_set[,5],predicted_table)
tablaRF = t(cmRF$table)
accuracyRF = round(cmRF$overall[1],3)
precisionRF= round(cmRF$byClass[,5],3) #con$byClass para todas las filas
recallRF= round(cmRF$byClass[,6],3)
f1RF= round(cmRF$byClass[,7],3)

TPR_RF = mean(cmRF$byClass[,1])
FPR_RF = 1 - mean(cmRF$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RF, y1 = TPR_RF, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RF, y0 = TPR_RF, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RF=((FPR_RF*TPR_RF)/2)+(((1-FPR_RF)*(1-TPR_RF))/2)+((1-FPR_RF)*TPR_RF)

##########-----------Naive Bayes--------############
#Modelo de Naive Bayes
modelo_naiveBayes <- naiveBayes(Species ~ ., data = trainning_set)

predict_modelo_naiveBayes <- predict(modelo_naiveBayes, test_set)

cmNB = confusionMatrix(test_set[,5],predict_modelo_naiveBayes)
tablaNB = t(cmNB$table)
accuracyNB = round(cmNB$overall[1],3)
precisionNB= round(cmNB$byClass[,5],3) #con$byClass para todas las filas
recallNB= round(cmNB$byClass[,6],3)
f1NB= round(cmNB$byClass[,7],3)

TPR_NB = mean(cmNB$byClass[,1])
FPR_NB = 1 - mean(cmNB$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_NB, y1 = TPR_NB, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_NB, y0 = TPR_NB, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_NB=((FPR_NB*TPR_NB)/2)+(((1-FPR_NB)*(1-TPR_NB))/2)+((1-FPR_NB)*TPR_NB)

##########-----------Regresión Logística------------##########
fit <- vglm(Species~., family=multinomial, data=trainning_set)

# make predictions
probabilities <- predict(fit, test_set[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(test_set$Species)[1]
predictions[which(predictions=="2")] <- levels(test_set$Species)[2]
predictions[which(predictions=="3")] <- levels(test_set$Species)[3]
predictions=as.factor(predictions)

cmRL=confusionMatrix(test_set[,5],predictions)
tablaRL = t(cmRL$table)
accuracyRL = round(cmRL$overall[1],3)
precisionRL= round(cmRL$byClass[,5],3) #con$byClass para todas las filas
recallRL= round(cmRL$byClass[,6],3)
f1RL= round(cmRL$byClass[,7],3)

TPR_RL = mean(cmRL$byClass[,1])
FPR_RL = 1 - mean(cmRL$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RL, y1 = TPR_RL, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RL, y0 = TPR_RL, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RL=((FPR_RL*TPR_RL)/2)+(((1-FPR_RL)*(1-TPR_RL))/2)+((1-FPR_RL)*TPR_RL)

##########--------------Red Neuronal -----------------##########
#Entrenamiento de la ANN (red neuronal)
ann = neuralnet(Species~., data = trainning_set, hidden = c(2,3,2))
plot(ann, rep = "best")

ann1 = neuralnet(as.numeric(Species)~., data = trainning_set, hidden = c(2,3,2))
plot(ann1, rep = "best")

#test
output = compute(ann, test_set[,1:4])
output1 = compute(ann1, test_set[,1:4])

#Evaluar
result = data.frame(
  Real = test_set$Species,
  Predicted = levels(iris$Species)[round(output1$net.result)]
)
prediccion=as.factor(result$Predicted)


cmRN = confusionMatrix(test_set[,5],prediccion)
tablaRN = t(cmRN$table)
accuracyRN = round(cmRN$overall[1],3)
precisionRN= round(cmRN$byClass[,5],3) #con$byClass para todas las filas
recallRN= round(cmRN$byClass[,6],3)
f1RN= round(cmRN$byClass[,7],3)

TPR_RN = mean(cmRN$byClass[,1])
FPR_RN = 1 - mean(cmRN$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RN, y1 = TPR_RN, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RN, y0 = TPR_RN, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RN=((FPR_RN*TPR_RN)/2)+(((1-FPR_RN)*(1-TPR_RN))/2)+((1-FPR_RN)*TPR_RN)

##########--------------Máquina de Soporte Vectorial -----------------##########
#Entrenar Clasificador
classifier1=svm(formula=Species~., data=trainning_set, 
                type = 'C-classification', kernel = 'linear')#Se puede probar ocn diferentes tipos de kernel este tiene un rendimiento del 0.903

#predicciones del clasficador
test_pred1 = predict(classifier1,type = 'response', newdata = test_set[-5])

cmMSV = confusionMatrix(test_set[,5],test_pred1)
tablaMSV = t(cmMSV$table)
accuracyMSV = round(cmMSV$overall[1],3)
precisionMSV= round(cmMSV$byClass[,5],3) #con$byClass para todas las filas
recallMSV= round(cmMSV$byClass[,6],3)
f1MSV= round(cmMSV$byClass[,7],3)

TPR_MSV = mean(cmMSV$byClass[,1])
FPR_MSV = 1 - mean(cmMSV$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_MSV, y1 = TPR_MSV, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_MSV, y0 = TPR_MSV, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_MSV=((FPR_MSV*TPR_MSV)/2)+(((1-FPR_MSV)*(1-TPR_MSV))/2)+((1-FPR_MSV)*TPR_MSV)


Valores <- c("Acurracy","Precision","Recall","F1","ROC")

Red_Neuronal <- c(accuracyRN,toString(precisionRN),toString(recallRN),toString(f1RN),RUC_RN)
PromediosRN <- c(accuracyRN,round(mean(precisionRN),3),round(mean(recallRN),3),round(mean(f1RN),3),RUC_RN)

KNN <- c(accuracyKNN,toString(precisionKNN),toString(recallKNN),toString(f1KNN),RUC_KNN)
PromediosKNN <- c(accuracyKNN,round(mean(precisionKNN),3),round(mean(recallKNN),3),round(mean(f1KNN),3),RUC_KNN)

Random_Forest <- c(accuracyRF,toString(precisionRF),toString(recallRF),toString(f1RF),RUC_RF)
PromediosRF <- c(accuracyRF,round(mean(precisionRF),3),round(mean(recallRF),3),round(mean(f1RF),3),RUC_RF)


Naive_Bayes <- c(accuracyNB,toString(precisionNB),toString(recallNB),toString(f1NB),RUC_NB)
PromediosNB <- c(accuracyNB,round(mean(precisionNB),3),round(mean(recallNB),3),round(mean(f1NB),3),RUC_NB)


Regresión_Logística <- c(accuracyRL,toString(precisionRL),toString(recallRL),toString(f1RL),RUC_RL)
PromediosRL <- c(accuracyRL,round(mean(precisionRL),3),round(mean(recallRL),3),round(mean(f1RL),3),RUC_RL)


Máquina_de_Soporte_Vectorial <- c(accuracyMSV,toString(precisionMSV),toString(recallMSV),toString(f1MSV),RUC_MSV)
PromediosMSV<- c(accuracyMSV,round(mean(precisionMSV),3),round(mean(recallMSV),3),round(mean(f1MSV),3),RUC_MSV)


tabla1RN <- data.frame(Valores,Red_Neuronal,PromediosRN)
tabla1KNN <- data.frame(Valores,KNN,PromediosKNN)
tabla1RF <- data.frame(Valores,Random_Forest,PromediosRF)
tabla1NB <- data.frame(Valores,Naive_Bayes,PromediosNB)
tabla1RL <- data.frame(Valores,Regresión_Logística,PromediosRL)
tabla1MSV <- data.frame(Valores,Máquina_de_Soporte_Vectorial,PromediosMSV)

valores_acurracy <- c(accuracyRN,accuracyKNN,accuracyRF,accuracyNB,accuracyRL,accuracyMSV)
valores_precision <- c(round(mean(precisionRN),3),
                       round(mean(precisionKNN),3),
                       round(mean(precisionRF),3),
                       round(mean(precisionNB),3),
                       round(mean(precisionRL),3),
                       round(mean(precisionMSV),3)
)
valores_recall <- c(round(mean(recallRN),3),
                    round(mean(recallKNN),3),
                    round(mean(recallRF),3),
                    round(mean(recallNB),3),
                    round(mean(recallRL),3),
                    round(mean(recallMSV),3)
)
valores_F1 <- c(round(mean(f1RN),3),
                round(mean(f1KNN),3),
                round(mean(f1RF),3),
                round(mean(f1NB),3),
                round(mean(f1RL),3),
                round(mean(f1MSV),3)
)
valores_ROC <- c(round(RUC_RN,3),
                 round(RUC_KNN,3),
                 round(RUC_RF,3),
                 round(RUC_NB,3),
                 round(RUC_RL,3),
                 round(RUC_MSV,3)
)

barras <- data.frame(valores_acurracy,valores_precision,valores_recall,valores_F1,valores_ROC)
rownames(barras) <- c("Red Neuronal","KNN","Random Forest","Naive Bayes","Regresion Logistica", "Máquina Soporte Vectorial")
####################### WHINE DATASET ####################
# Loading the library
library(rattle.data)
# Loading the wine data
data(wine)

datasetWhine = wine

#Random Cross Validation
set.seed(3)
split = sample.split(datasetWhine$Type, SplitRatio = 0.7)

#Trainning
trainning_set_WHINE=subset(datasetWhine,split==TRUE)

#Test
test_set_WHINE=subset(datasetWhine, split==FALSE)


#Escalado de los datos a otra dimesion
trainning_set_WHINE[,2:14]=scale(trainning_set_WHINE[,2:14])
test_set_WHINE[,2:14]=scale(test_set_WHINE[,2:14])

######-----KNN-----######
KnnTestprediccion_WHINE <- knn(trainning_set_WHINE[,2:14], test_set_WHINE[,2:14], trainning_set_WHINE$Type, k = 3, prob = TRUE )

cmKNN_WHINE = confusionMatrix(test_set_WHINE[,1],KnnTestprediccion_WHINE)
accuracyKNN_WHINE = round(cmKNN_WHINE$overall[1],3)
precisionKNN_WHINE= round(cmKNN_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallKNN_WHINE= round(cmKNN_WHINE$byClass[,6],3)
f1KNN_WHINE= round(cmKNN_WHINE$byClass[,7],3)

TPR_KNN_WHINE = mean(cmKNN_WHINE$byClass[,1])
FPR_KNN_WHINE = 1 - mean(cmKNN_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_KNN_WHINE, y1 = TPR_KNN_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_KNN_WHINE, y0 = TPR_KNN_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_KNN_WHINE=((FPR_KNN_WHINE*TPR_KNN_WHINE)/2)+(((1-FPR_KNN_WHINE)*(1-TPR_KNN_WHINE))/2)+((1-FPR_KNN_WHINE)*TPR_KNN_WHINE)

######-----Random Forest-----######
decision_tree_WHINE <- tree(Type ~ ., data = datasetWhine) # Interpretation

#Funcion para random forest
wine_clasificadorRF <- randomForest(Type ~., 
                                    data = trainning_set_WHINE, #train data set 
                                    importance = T)

#Prediciendo el test
predicted_table_WHINE <- predict(wine_clasificadorRF, test_set_WHINE[,-1])

cmRF_WHINE = confusionMatrix(test_set_WHINE[,1],predicted_table_WHINE)
accuracyRF_WHINE = round(cmRF_WHINE$overall[1],3)
precisionRF_WHINE= round(cmRF_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallRF_WHINE= round(cmRF_WHINE$byClass[,6],3)
f1RF_WHINE= round(cmRF_WHINE$byClass[,7],3)

TPR_RF_WHINE = mean(cmRF_WHINE$byClass[,1])
FPR_RF_WHINE = 1 - mean(cmRF_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RF_WHINE, y1 = TPR_RF_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RF_WHINE, y0 = TPR_RF_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RF_WHINE=((FPR_RF_WHINE*TPR_RF_WHINE)/2)+(((1-FPR_RF_WHINE)*(1-TPR_RF_WHINE))/2)+((1-FPR_RF_WHINE)*TPR_RF_WHINE)

#####-----------Naive Bayes--------#######
#Modelo de Naive Bayes
modelo_naiveBayes_WHINE <- naiveBayes(Type ~ ., data = trainning_set_WHINE)

predict_modelo_naiveBayes_WHINE <- predict(modelo_naiveBayes_WHINE, test_set_WHINE)

cmNB_WHINE = confusionMatrix(test_set_WHINE[,1],predict_modelo_naiveBayes_WHINE)
accuracyNB_WHINE = round(cmNB_WHINE$overall[1],3)
precisionNB_WHINE= round(cmNB_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallNB_WHINE= round(cmNB_WHINE$byClass[,6],3)
f1NB_WHINE= round(cmNB_WHINE$byClass[,7],3)

TPR_NB_WHINE = mean(cmNB_WHINE$byClass[,1])
FPR_NB_WHINE = 1 - mean(cmNB_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_NB_WHINE, y1 = TPR_NB_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_NB_WHINE, y0 = TPR_NB_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_NB_WHINE=((FPR_NB_WHINE*TPR_NB_WHINE)/2)+(((1-FPR_NB_WHINE)*(1-TPR_NB_WHINE))/2)+((1-FPR_NB_WHINE)*TPR_NB_WHINE)

#-----------Regresión Logística------------##
fit_WHINE <- vglm(Type~., family=multinomial, data=trainning_set_WHINE)

# make predictions_WHINE
probabilities_WHINE <- predict(fit_WHINE, test_set_WHINE[,2:14], type="response")
predictions_WHINE <- apply(probabilities_WHINE, 1, which.max)
predictions_WHINE[which(predictions_WHINE=="1")] <- levels(test_set_WHINE$Type)[1]
predictions_WHINE[which(predictions_WHINE=="2")] <- levels(test_set_WHINE$Type)[2]
predictions_WHINE[which(predictions_WHINE=="3")] <- levels(test_set_WHINE$Type)[3]
predictions_WHINE=as.factor(predictions_WHINE)

cmRL_WHINE=confusionMatrix(test_set_WHINE[,1],predictions_WHINE)
accuracyRL_WHINE = round(cmRL_WHINE$overall[1],3)
precisionRL_WHINE= round(cmRL_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallRL_WHINE= round(cmRL_WHINE$byClass[,6],3)
f1RL_WHINE= round(cmRL_WHINE$byClass[,7],3)

TPR_RL_WHINE = mean(cmRL_WHINE$byClass[,1])
FPR_RL_WHINE = 1 - mean(cmRL_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RL_WHINE, y1 = TPR_RL_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RL_WHINE, y0 = TPR_RL_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RL_WHINE=((FPR_RL_WHINE*TPR_RL_WHINE)/2)+(((1-FPR_RL_WHINE)*(1-TPR_RL_WHINE))/2)+((1-FPR_RL_WHINE)*TPR_RL_WHINE)

#--------------Red Neuronal -----------------#
#Entrenamiento de la ann_WHINE (red neuronal)
ann_WHINE = neuralnet(Type~., data = trainning_set_WHINE, hidden = c(2,3,2))
plot(ann_WHINE, rep = "best")

ann_WHINE1 = neuralnet(as.numeric(Type)~., data = trainning_set_WHINE, hidden = c(2,3,2))
plot(ann_WHINE1, rep = "best")

#test
output_WHINE = compute(ann_WHINE, test_set_WHINE[,2:14])
output_WHINE1 = compute(ann_WHINE1, test_set_WHINE[,2:14])

#Evaluar
result_WHINE = data.frame(
  Real_WHINE = test_set_WHINE$Type,
  Predicted_WHINE = levels(wine$Type)[round(output_WHINE1$net.result)]
)
prediccion_WHINE=as.factor(result_WHINE$Predicted)


cmRN_WHINE = confusionMatrix(test_set_WHINE[,1],prediccion_WHINE)
accuracyRN_WHINE = round(cmRN_WHINE$overall[1],3)
precisionRN_WHINE= round(cmRN_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallRN_WHINE= round(cmRN_WHINE$byClass[,6],3)
f1RN_WHINE= round(cmRN_WHINE$byClass[,7],3)

TPR_RN_WHINE = mean(cmRN_WHINE$byClass[,1])
FPR_RN_WHINE = 1 - mean(cmRN_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_RN_WHINE, y1 = TPR_RN_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_RN_WHINE, y0 = TPR_RN_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_RN_WHINE=((FPR_RN_WHINE*TPR_RN_WHINE)/2)+(((1-FPR_RN_WHINE)*(1-TPR_RN_WHINE))/2)+((1-FPR_RN_WHINE)*TPR_RN_WHINE)

#--------------Máquina de Soporte Vectorial -----------------#
#Entrenar Clasificador
classifier1_WHINE=svm(formula=Type~., data=trainning_set_WHINE, 
                      type = 'C-classification', kernel = 'linear')#Se puede probar ocn diferentes tipos de kernel este tiene un rendimiento del 0.903

#prediccion_WHINEes del clasficador
test_pred1_WHINE = predict(classifier1_WHINE,type = 'response', newdata = test_set_WHINE[-1])

cmMSV_WHINE = confusionMatrix(test_set_WHINE[,1],test_pred1_WHINE)
accuracyMSV_WHINE = round(cmMSV_WHINE$overall[1],3)
precisionMSV_WHINE= round(cmMSV_WHINE$byClass[,5],3) #con$byClass para todas las filas
recallMSV_WHINE= round(cmMSV_WHINE$byClass[,6],3)
f1MSV_WHINE= round(cmMSV_WHINE$byClass[,7],3)

TPR_MSV_WHINE = mean(cmMSV_WHINE$byClass[,1])
FPR_MSV_WHINE = 1 - mean(cmMSV_WHINE$byClass[,2])

plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))

segments(x0 = 0, y0 = 0, x1 = FPR_MSV_WHINE, y1 = TPR_MSV_WHINE, col = "darkgreen",lwd = 2) 
segments(x0 = FPR_MSV_WHINE, y0 = TPR_MSV_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
grid(nx = NULL, ny = NULL,
     lty = 1,      
     col = "gray", 
     lwd = 1)      


RUC_MSV_WHINE=((FPR_MSV_WHINE*TPR_MSV_WHINE)/2)+(((1-FPR_MSV_WHINE)*(1-TPR_MSV_WHINE))/2)+((1-FPR_MSV_WHINE)*TPR_MSV_WHINE)


Valores <- c("Acurracy","Precision","Recall","F1","ROC")

Red_Neuronal_WHINE <- c(accuracyRN_WHINE,toString(precisionRN_WHINE),toString(recallRN_WHINE),toString(f1RN_WHINE),RUC_RN_WHINE)
PromediosRN_WHINE <- c(accuracyRN_WHINE,round(mean(precisionRN_WHINE),3),round(mean(recallRN_WHINE),3),round(mean(f1RN_WHINE),3),RUC_RN_WHINE)

KNN_WHINE <- c(accuracyKNN_WHINE,toString(precisionKNN_WHINE),toString(recallKNN_WHINE),toString(f1KNN_WHINE),RUC_KNN_WHINE)
PromediosKNN_WHINE <- c(accuracyKNN_WHINE,round(mean(precisionKNN_WHINE),3),round(mean(recallKNN_WHINE),3),round(mean(f1KNN_WHINE),3),RUC_KNN_WHINE)

Random_Forest_WHINE <- c(accuracyRF_WHINE,toString(precisionRF_WHINE),toString(recallRF_WHINE),toString(f1RF_WHINE),RUC_RF_WHINE)
PromediosRF_WHINE <- c(accuracyRF_WHINE,round(mean(precisionRF_WHINE),3),round(mean(recallRF_WHINE),3),round(mean(f1RF_WHINE),3),RUC_RF_WHINE)


Naive_Bayes_WHINE <- c(accuracyNB_WHINE,toString(precisionNB_WHINE),toString(recallNB_WHINE),toString(f1NB_WHINE),RUC_NB_WHINE)
PromediosNB_WHINE <- c(accuracyNB_WHINE,round(mean(precisionNB_WHINE),3),round(mean(recallNB_WHINE),3),round(mean(f1NB_WHINE),3),RUC_NB_WHINE)


Regresión_Logística_WHINE <- c(accuracyRL_WHINE,toString(precisionRL_WHINE),toString(recallRL_WHINE),toString(f1RL_WHINE),RUC_RL_WHINE)
PromediosRL_WHINE <- c(accuracyRL_WHINE,round(mean(precisionRL_WHINE),3),round(mean(recallRL_WHINE),3),round(mean(f1RL_WHINE),3),RUC_RL_WHINE)


Máquina_de_Soporte_Vectorial_WHINE <- c(accuracyMSV,toString(precisionMSV_WHINE),toString(recallMSV_WHINE),toString(f1MSV_WHINE),RUC_MSV_WHINE)
PromediosMSV_WHINE<- c(accuracyMSV_WHINE,round(mean(precisionMSV_WHINE),3),round(mean(recallMSV_WHINE),3),round(mean(f1MSV_WHINE),3),RUC_MSV_WHINE)


tabla1RN_WHINE <- data.frame(Valores,Red_Neuronal_WHINE,PromediosRN_WHINE)
tabla1KNN_WHINE <- data.frame(Valores,KNN_WHINE,PromediosKNN_WHINE)
tabla1RF_WHINE <- data.frame(Valores,Random_Forest_WHINE,PromediosRF_WHINE)
tabla1NB_WHINE <- data.frame(Valores,Naive_Bayes_WHINE,PromediosNB_WHINE)
tabla1RL_WHINE <- data.frame(Valores,Regresión_Logística_WHINE,PromediosRL_WHINE)
tabla1MSV_WHINE <- data.frame(Valores,Máquina_de_Soporte_Vectorial_WHINE,PromediosMSV_WHINE)

valores_acurracy_WHINE <- c(accuracyRN,accuracyKNN,accuracyRF,accuracyNB,accuracyRL,accuracyMSV)
valores_precision_WHINE <- c(round(mean(precisionRN_WHINE),3),
                             round(mean(precisionKNN_WHINE),3),
                             round(mean(precisionRF_WHINE),3),
                             round(mean(precisionNB_WHINE),3),
                             round(mean(precisionRL_WHINE),3),
                             round(mean(precisionMSV_WHINE),3)
)
valores_recall_WHINE <- c(round(mean(recallRN_WHINE),3),
                          round(mean(recallKNN_WHINE),3),
                          round(mean(recallRF_WHINE),3),
                          round(mean(recallNB_WHINE),3),
                          round(mean(recallRL_WHINE),3),
                          round(mean(recallMSV_WHINE),3)
)
valores_F1_WHINE <- c(round(mean(f1RN_WHINE),3),
                      round(mean(f1KNN_WHINE),3),
                      round(mean(f1RF_WHINE),3),
                      round(mean(f1NB_WHINE),3),
                      round(mean(f1RL_WHINE),3),
                      round(mean(f1MSV_WHINE),3)
)
valores_ROC_WHINE <- c(round(RUC_RN_WHINE,3),
                       round(RUC_KNN_WHINE,3),
                       round(RUC_RF_WHINE,3),
                       round(RUC_NB_WHINE,3),
                       round(RUC_RL_WHINE,3),
                       round(RUC_MSV_WHINE,3)
)

barras_WHINE <- data.frame(valores_acurracy_WHINE,valores_precision_WHINE,valores_recall_WHINE,valores_F1_WHINE,valores_ROC_WHINE)
rownames(barras_WHINE) <- c("Red Neuronal","KNN","Random Forest","Naive Bayes","Regresion Logistica", "Máquina Soporte Vectorial")

########################INTERFAZ DEL SERVIDOR########################
shinyServer(function(input, output) {
     
     output$keepAlive <- renderText({
      req(input$count)
      paste("keep alive ", input$count)
    })
  #####DATASET IRIS - PÁGINA 1#####
  algoritmo1Input <- reactive({
    switch(input$select1,
           "knn" = tabla1KNN,
           "rf" = tabla1RF,
           "nb" = tabla1NB,
           "rl" = tabla1RL,
           "rn" = tabla1RN,
           "msv" = tabla1MSV
    )
  })
  
  output$view1 <- DT::renderDataTable(
    DT::datatable(iris, options = list(searching = FALSE, pageLength = 5))
  )
  
  output$view2 <- DT::renderDataTable(
    DT::datatable(algoritmo1Input(), options = list(searching = FALSE))
  )
  
  output$plot1 <- renderPlot({
    if(input$select1 == "knn"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_KNN, y1 = TPR_KNN, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_KNN, y0 = TPR_KNN, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select1 == "rf"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RF, y1 = TPR_RF, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RF, y0 = TPR_RF, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select1 == "nb"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_NB, y1 = TPR_NB, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_NB, y0 = TPR_NB, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select1 == "rl"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RL, y1 = TPR_RL, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RL, y0 = TPR_RL, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select1 == "rn"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RN, y1 = TPR_RN, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RN, y0 = TPR_RN, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select1 == "msv"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_MSV, y1 = TPR_MSV, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_MSV, y0 = TPR_MSV, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
  })
  #####DATASET WHINE - PÁGINA 2#####
  algoritmo2Input <- reactive({
    switch(input$select2,
           "knn" = tabla1KNN_WHINE,
           "rf" = tabla1RF_WHINE,
           "nb" = tabla1NB_WHINE,
           "rl" = tabla1RL_WHINE,
           "rn" = tabla1RN_WHINE,
           "msv" = tabla1MSV_WHINE
    )
  })
  
  output$view3 <- DT::renderDataTable(
    DT::datatable(wine, options = list(searching = FALSE, pageLength = 5))
  )
  
  output$view4 <- DT::renderDataTable(
    DT::datatable(algoritmo2Input(), options = list(searching = FALSE))
  )
  
  output$plot2 <- renderPlot({
    if(input$select2 == "knn"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_KNN_WHINE, y1 = TPR_KNN_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_KNN_WHINE, y0 = TPR_KNN_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select2 == "rf"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RF_WHINE, y1 = TPR_RF_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RF_WHINE, y0 = TPR_RF_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select2 == "nb"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_NB_WHINE, y1 = TPR_NB_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_NB_WHINE, y0 = TPR_NB_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select2 == "rl"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RL_WHINE, y1 = TPR_RL_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RL_WHINE, y0 = TPR_RL_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select2 == "rn"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_RN_WHINE, y1 = TPR_RN_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_RN_WHINE, y0 = TPR_RN_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
    if(input$select2 == "msv"){
      plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
      
      segments(x0 = 0, y0 = 0, x1 = FPR_MSV_WHINE, y1 = TPR_MSV_WHINE, col = "darkgreen",lwd = 2) 
      segments(x0 = FPR_MSV_WHINE, y0 = TPR_MSV_WHINE, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
      grid(nx = NULL, ny = NULL,
           lty = 1,      
           col = "gray", 
           lwd = 1)
    }
  })
  #####BENCHMARK IRIS - PÁGINA 3#####
  output$barrasIRIS <- renderPlot({
    if(input$select3 == "valores_acurracy"){
      barplot(barras[,input$select3], 
              main=input$select3,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select3 == "valores_precision"){
      barplot(barras[,input$select3], 
              main=input$select3,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select3 == "valores_recall"){
      barplot(barras[,input$select3], 
              main=input$select3,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select3 == "valores_F1"){
      barplot(barras[,input$select3], 
              main=input$select3,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select3 == "valores_ROC"){
      barplot(barras[,input$select3], 
              main=input$select3,
              ylab="Valor",
              xlab="Modelo")
    }
  })
  #####BENCHMARK WHINE - PÁGINA 4#####
  output$barrasWHINE <- renderPlot({
    if(input$select4 == "valores_acurracy_WHINE"){
      barplot(barras_WHINE[,input$select4], 
              main=input$select4,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select4 == "valores_precision_WHINE"){
      barplot(barras_WHINE[,input$select4], 
              main=input$select4,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select4 == "valores_recall_WHINE"){
      barplot(barras_WHINE[,input$select4], 
              main=input$select4,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select4 == "valores_F1_WHINE"){
      barplot(barras_WHINE[,input$select4], 
              main=input$select4,
              ylab="Valor",
              xlab="Modelo")
    }
    if(input$select4 == "valores_ROC_WHINE"){
      barplot(barras_WHINE[,input$select4], 
              main=input$select4,
              ylab="Valor",
              xlab="Modelo")
    }
  })
})



