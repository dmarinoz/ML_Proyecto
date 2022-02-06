###################INTERFAZ DEL USUARIO####################
library(shiny)
navbarPage(
  "Machine Learning",
  tabPanel("Dataset de Iris",
           sidebarLayout(
             sidebarPanel(
               selectInput("select1","Algoritmo a aplicar:",
                           choices = list("KNN"="knn","Random Forest"="rf","Naive Bayes"="nb","Regresión Logística"="rl","Red Neuronal"="rn","Máquina de Soporte Vectorial"="msv")
               )
             ),
             # Show a plot of the generated distribution
             mainPanel(
               h1("Iris Dataset"),
               DT::dataTableOutput('view1'),
               h1("Algoritmo"),
               DT::dataTableOutput('view2'),
               h1("Curva ROC"),
               plotOutput("plot1")
             )
           )
           
  ),
  tabPanel("Dataset de Wine",
           sidebarLayout(
             sidebarPanel(
               selectInput("select2","Algoritmo a aplicar:",
                           choices = list("KNN"="knn","Random Forest"="rf","Naive Bayes"="nb","Regresión Logística"="rl","Red Neuronal"="rn","Máquina de Soporte Vectorial"="msv")
               )
             ),
             # Show a plot of the generated distribution
             mainPanel(
               h1("Wine Dataset"),
               DT::dataTableOutput('view3'),
               h1("Algoritmo"),
               DT::dataTableOutput('view4'),
               h1("Curva ROC"),
               plotOutput("plot2")
             )
           )
           
  ),
  tabPanel("Benchmark de Iris",
           sidebarLayout(
             sidebarPanel(
               selectInput("select3","Algoritmo a aplicar:",
                           choices = list("Accuracy"="valores_acurracy","Precision"="valores_precision","Recall"="valores_recall","F1"="valores_F1","ROC"="valores_ROC")
               )
             ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("barrasIRIS")
             )
           )
           
  ),
  tabPanel("Benchmark de Wine",
           sidebarLayout(
             sidebarPanel(
               selectInput("select4","Algoritmo a aplicar:",
                           choices = list("Accuracy"="valores_acurracy_WHINE","Precision"="valores_precision_WHINE","Recall"="valores_recall_WHINE","F1"="valores_F1_WHINE","ROC"="valores_ROC_WHINE")
               )
             ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("barrasWHINE")
             )
           )
  )
)

