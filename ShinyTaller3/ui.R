# Aplicación web que permite visualizar datos de la base de datos de finanzas públicas de 
# Eurostat (gov_10a_main) a través de un gráfico y una tabla, asi como descargar los mismos
# en un documento .pdf
#                   Elaborado por Kamal Romero (karomero@ucm.es)

library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Paridad de Poder de Compra Relativa (BIS)"),
  sidebarPanel(
    selectizeInput(
      "pais","País",
      choices=c("Zona Euro"="XM", "Canada"="CA", "Switzerland"="CH", "Australia"="AU",  "Japan"= "JP")
      
    ),
    
    radioButtons('format', 'Formato del reporte', c('PDF'),
                 inline = TRUE),
    downloadButton('downloadReport')
    
),
  mainPanel(
    plotOutput("grafico")
  )
))

