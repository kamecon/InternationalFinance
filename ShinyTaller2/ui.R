# Aplicación web que permite visualizar datos de la base de datos de finanzas públicas de 
# Eurostat (gov_10a_main) a través de un gráfico y una tabla, asi como descargar los mismos
# en un documento .pdf
#                   Elaborado por Kamal Romero (karomero@ucm.es)

library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Tipo de Cambio Real (BIS)"),
  sidebarPanel(
    selectizeInput(
      "pais","País",
      choices=c("Canada"="Canada", "Switzerland"="Switzerland", "Australia"="Australia",  "Japan"= "Japan",  "Turkey" = "Turkey", "Russia" ="Russia")
      
    ),
    
    radioButtons('format', 'Formato del reporte', c('PDF'),
                 inline = TRUE),
    downloadButton('downloadReport')
    
),
  mainPanel(
    plotOutput("grafico"),
    plotOutput("grafico2"),
    plotOutput("grafico3")
  )
))

