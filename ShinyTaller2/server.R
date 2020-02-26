# Aplicación web que permite visualizar datos de la base de datos del BIS a través de 3 gráficos, asi como descargar los mismos en un documento .pdf
#                   Elaborado por Kamal Romero (kamal.romerosookoo@ceu.es)



#Dato a buscar
datoss <- get_datasets()
rer <- get_bis(datoss$url[datoss$name == "Effective exchange rate indices (monthly)"], quiet = TRUE)
precios <- get_bis(datoss$url[datoss$name == "Consumer prices"], quiet = TRUE)




rer_bis <- function(country){
  
    #Seleccionamos los datos que nos interesan y los ordenamos por país y año
  
   rer1 <- rer %>%
    dplyr::filter(type=="Real" & reference_area %in% c("Spain", country) & eer_basket=="B" & date > "1999-12") %>%
    mutate(date = as.Date(as.yearmon(date))) %>%
    select(date, obs_value, reference_area) %>% 
    dplyr::arrange(reference_area, date)
  
   rer1$reference_area <- as.factor(rer1$reference_area)
  
  return(rer1)
}

price_bis <- function(country){
  
  precios2 <- precios %>%  dplyr::filter(frequency == "Monthly" & reference_area %in% c("Spain",country) & unit_of_measure== "Index, 2010 = 100" & date > "1999-12") %>%
    mutate(date = as.Date(as.yearmon(date))) %>%
    select(date, obs_value, reference_area) %>% 
    dplyr::arrange(reference_area, date) %>% 
    select(date, obs_value, reference_area)
  
  return(precios2)
}

ner_bis <- function(country){
  
  ner <- rer %>%
    dplyr::filter(type=="Nominal" & reference_area %in% c("Spain", country) & eer_basket=="B" & date > "1999-12") %>%
    mutate(date = as.Date(as.yearmon(date))) %>%
    select(date, obs_value, reference_area) %>% 
    group_by(reference_area) %>%
    dplyr::mutate(var_porct = (obs_value/lag(obs_value, n = 12) -1)*100)
  
  
  return(ner)
}


shinyServer(
  function(input, output) {
    
    resultados <- reactive({
      rer_bis(input$pais)
    })
    
    resultados2 <- reactive({
      price_bis(input$pais)
    })
    
    resultados3 <- reactive({
      ner_bis(input$pais)
    })
    
    #Gráfico
    
  
    output$grafico <- renderPlot({
      datos <- resultados()
            
      ploter <- ggplot(datos, aes(x=date, y=obs_value, group=reference_area, colour=reference_area))+
        geom_line(size=1.6)+
        labs(title = "Tipo de cambio real", x="", y="", caption = 'Fuente = BIS')+
        theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=12))+
        theme(legend.position = "bottom")+
        theme(legend.title = element_blank())
      
      
      ploter
    })
    
    output$grafico2 <- renderPlot({
      datos2 <- resultados2()
      
      ploter2 <- ggplot(datos2, aes(x=date, y=obs_value, group=reference_area, colour=reference_area))+
        geom_line(size=1.6)+
        labs(title = "Índice de Precios", x="", y="Índice base 2010", caption = 'Fuente = BIS')+
        theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=12))+
        theme(legend.position = "bottom")+
        theme(legend.title = element_blank())
      
      
      ploter2
    })
    
    
    output$grafico3 <- renderPlot({
      datos3 <- resultados3()
      
      ploter3 <-  ggplot(datos3, aes(x=date, y=var_porct, group=reference_area, colour=reference_area))+
        geom_line(size=1.6)+
        labs(title = "Variación del tipo de cambio nominal", x="", y="%", caption = 'Fuente = BIS')+
        theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=12))+
        theme(legend.position = "bottom")+
        theme(legend.title = element_blank())
      
      
      ploter3
    })
    
    #Reporte en pdf
      
      content = function(file) {
        src <- normalizePath('Prueba_reporte.Rmd')

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'Prueba_reporte.Rmd', overwrite = TRUE)

        #Cargamos los datos
        datos <- resultados()
        datos2 <- resultados2()
        datos3 <- resultados3()

        #library(rmarkdown)
        out <- render('Prueba_reporte.Rmd', switch(
          input$format,
          PDF = pdf_document()
        ))
        file.rename(out, file)
      }
    
    
  }
)