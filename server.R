

####################### IRIS DATASET ####################
dataset = iris






Valores <- c("Acurracy","Precision","Recall","F1","ROC")

Red_Neuronal <- c("0.911","1, 0.867, 0.867","0.938, 0.867, 0.929","0.968, 0.867, 0.897","0.934")
PromediosRN <- c("0.911","0.911","0.911","0.911","0.933")

KNN <- c("0.956","1, 1, 0.867","1, 0.882, 1","1, 0.938, 0.929","0.97")
PromediosKNN <- c("0.956","0.956","0.961","0.956","0.97")

Random_Forest <- c("0.933","1, 0.933, 0.867","1, 0.875, 0.929","1, 0.903, 0.897","0.95076209")
PromediosRF <- c("0.933","0.933","0.935","0.933","0.9507")


Naive_Bayes <- c("0.933", "1, 0.933, 0.867", "1, 0.933, 0.929", "1, 0.903, 0.807","0.951")
PromediosNB <- c("0.933", "0.933", "0.933", "0.933","0.951")


Regresión_Logística <- c("0.933","1, 0.933, 0.867","1, 0.875, 0.929","1, 0.903, 0.897","0.951")
PromediosRL <- c("0.933","0.933","0.935","0.933","0.951")


Máquina_de_Soporte_Vectorial <- c("0.933","1, 0.933, 0.867","1, 0.875, 0.929","1, 0.0903, 0.897","0.951")
PromediosMSV<- c("0.933","0.933","0.935","0.933","0.951")


tabla1RN <- data.frame(Valores,Red_Neuronal,PromediosRN)
tabla1KNN <- data.frame(Valores,KNN,PromediosKNN)
tabla1RF <- data.frame(Valores,Random_Forest,PromediosRF)
tabla1NB <- data.frame(Valores,Naive_Bayes,PromediosNB)
tabla1RL <- data.frame(Valores,Regresión_Logística,PromediosRL)
tabla1MSV <- data.frame(Valores,Máquina_de_Soporte_Vectorial,PromediosMSV)

valores_acurracy <- c(0.911,
                      0.956,
                      0.933,
                      0.933,
                      0.933,
                      0.911
)
valores_precision <- c(0.911,
                       0.956,
                       0.933,
                       0.933,
                       0.933,
                       0.911
)
valores_recall <- c(0.911,
                    0.961,
                    0.935,
                    0.933,
                    0.935,
                    0.935
)
valores_F1 <- c(0.911,
                0.956,
                0.933,
                0.933,
                0.933,
                0.933
)
valores_ROC <- c(0.934,
                 0.97,
                 0.951,
                 0.951,
                 0.951,
                 0.951
)

barras <- data.frame(valores_acurracy,valores_precision,valores_recall,valores_F1,valores_ROC)
rownames(barras) <- c("Red Neuronal","KNN","Random Forest","Naive Bayes","Regresion Logistica", "Máquina Soporte Vectorial")





####################### WHINE DATASET ####################
# Loading the library
#library(rattle.data)
# Loading the wine data
#data(wine)
#datasetWhine = wine



Valores <- c("Acurracy","Precision","Recall","F1","ROC")

Red_Neuronal_WHINE <- c("0.943", "0.944, 0.952, 0.929", "1, 0.909, 0.929", "0.971, 0.93, 0.929","0.959")
PromediosRN_WHINE <- c("0.943", "0.942", "0.946", "0.943","0.957")

KNN_WHINE <- c("0.962","1, 0.905, 1","0.9, 1, 1","0.947, 0.95, 1","0.974")
PromediosKNN_WHINE <- c("0.962","0.968","0.967","0.966","0.974")

Random_Forest_WHINE <- c("0.981","1, 0.952, 1","0.947, 1, 1","0.973, 0.976, 1","0.9861")
PromediosRF_WHINE <- c("0.981","0.984","0.982","0.983","0.9861")


Naive_Bayes_WHINE <- c("0.962", "0.944, 0.952, 1", "1, 0.952, 0.933", "0.971, 0.952, 0.966","0.971")
PromediosNB_WHINE <- c("0.962", "0.965", "0.962", "0.963","0.971")


Regresión_Logística_WHINE <- c("0.962","0.944, 1, 0.929","1, 0.913, 1","0.971, 0.955, 0.963","0.977")
PromediosRL_WHINE <- c("0.962","0.958","0.971","0.963","0.977")


Máquina_de_Soporte_Vectorial_WHINE <- c("0.933","0.889, 1, 0.929","1, 0.875, 1","0.941, 0.933, 0.963","0.9659")
PromediosMSV_WHINE<- c("0.933","0.939","0.958","0.946","0.9659")


tabla1RN_WHINE <- data.frame(Valores,Red_Neuronal_WHINE,PromediosRN_WHINE)
tabla1KNN_WHINE <- data.frame(Valores,KNN_WHINE,PromediosKNN_WHINE)
tabla1RF_WHINE <- data.frame(Valores,Random_Forest_WHINE,PromediosRF_WHINE)
tabla1NB_WHINE <- data.frame(Valores,Naive_Bayes_WHINE,PromediosNB_WHINE)
tabla1RL_WHINE <- data.frame(Valores,Regresión_Logística_WHINE,PromediosRL_WHINE)
tabla1MSV_WHINE <- data.frame(Valores,Máquina_de_Soporte_Vectorial_WHINE,PromediosMSV_WHINE)

valores_acurracy_WHINE <- c(0.943,
                            0.962,
                            0.981,
                            0.962,
                            0.962,
                            0.933
)
valores_precision_WHINE <- c(0.942,
                             0.968,
                             0.984,
                             0.965,
                             0.958,
                             0.939
)
valores_recall_WHINE <- c(0.946,
                          0.967,
                          0.992,
                          0.962,
                          0.971,
                          0.958
)
valores_F1_WHINE <- c(0.943,
                      0.966,
                      0.983,
                      0.963,
                      0.963,
                      0.946
)
valores_ROC_WHINE <- c(0.959,
                       0.974,
                       0.986,
                       0.971,
                       0.977,
                       0.966
)

barras_WHINE <- data.frame(valores_acurracy_WHINE,valores_precision_WHINE,valores_recall_WHINE,valores_F1_WHINE,valores_ROC_WHINE)
rownames(barras_WHINE) <- c("Red Neuronal","KNN","Random Forest","Naive Bayes","Regresion Logistica", "Máquina Soporte Vectorial")

########################INTERFAZ DEL SERVIDOR########################
shinyServer(function(input, output) {
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
            
            segments(x0 = 0, y0 = 0, x1 = 0.02083333, y1 = 0.9607843, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.02083333, y0 = 0.9607843, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select1 == "rf"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.03299963, y1 = 0.9345238, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.03299963, y0 = 0.9345238, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select1 == "nb"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.03299963, y1 = 0.9345238, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.03299963, y0 = 0.9345238, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select1 == "rl"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.03299963, y1 = 0.9345238, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.03299963, y0 = 0.9345238, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select1 == "rn"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.0437276, y1 = 0.9109127, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.0437276, y0 = 0.9109127, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select1 == "msv"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.03299963, y1 = 0.9345238, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.03299963, y0 = 0.9345238, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
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
            
            segments(x0 = 0, y0 = 0, x1 = 0.01960784, y1 = 0.9666667, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.01960784, y0 = 0.9666667, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select2 == "rf"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.01010101, y1 = 0.9824561, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.01010101, y0 = 0.9824561, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select2 == "nb"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.01967593, y1 = 0.9619048, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.01967593, y0 = 0.9619048, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select2 == "rl"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.01759259, y1 = 0.9710145, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.01759259, y0 = 0.9710145, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select2 == "rn"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.02855896, y1 = 0.9458874, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.02855896, y0 = 0.9458874, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
            grid(nx = NULL, ny = NULL,
                 lty = 1,      
                 col = "gray", 
                 lwd = 1)
        }
        if(input$select2 == "msv"){
            plot(0, 0, col = "white", xlab = "", ylab = "", xlim=c(0,1), ylim=c(0,1))
            
            segments(x0 = 0, y0 = 0, x1 = 0.02635135, y1 = 0.9583333, col = "darkgreen",lwd = 2) 
            segments(x0 = 0.02635135, y0 = 0.9583333, x1 =1, y1 = 1, col = "darkgreen", lwd = 2) 
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

