# Aplicación web que permite visualizar datos de la base de datos del BIS, establecer una relación lineal entre ellos y descargar el resultado en un documento .pdf
#                   Elaborado por Kamal Romero (kamal.romerosookoo@ceu.es)



#Dato a buscar
datasets <- get_datasets()
precios <- get_bis(datasets$url[datasets$name == "Consumer prices"], quiet = TRUE)
tdc_usa <- get_bis(datasets$url[datasets$name == "US dollar exchange rates (monthly, quarterly and annual)"], quiet = TRUE)

#Datos de precios

inflaciones <- precios %>%
  dplyr::filter(freq=="A" & unit_of_measure == "Year-on-year changes, in per cent" & date > "1999-12") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  select(ref_area, reference_area, unit_of_measure, date, obs_value) %>% 
  mutate(date = as.numeric(date)) %>% 
  select(ref_area, date, obs_value) %>% 
  rename(inflacion=obs_value)

#Datos de tipo de cambio

tdc_var <- tdc_usa %>% 
  dplyr::filter(freq=="A" & date > "1998" & collection == "A") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  group_by(ref_area) %>%
  mutate(var_porct = (obs_value/lag(obs_value) -1)*100) %>% 
  dplyr::filter(date > "1999")%>% 
  select(ref_area, date, var_porct) %>% 
  mutate(date = as.numeric(date))




ppp_bis <- function(country){
  
    #Seleccionamos los datos que nos interesan y los ordenamos por país y año
  
  datos_lm <- inflaciones %>% 
    dplyr::filter(ref_area %in% c("US", country)) %>%
    spread(key=ref_area, value=inflacion)
  
  colnames(datos_lm)[2:3] <- c("home","us")
  
  datos_lm2 <- datos_lm %>% 
    mutate(diferencial=home - us) %>% 
    dplyr::full_join(tdc_var %>% dplyr::filter(ref_area==country), by="date") %>% 
    select(-ref_area)
  
  return(datos_lm2)
}


shinyServer(
  function(input, output) {
    
    resultados <- reactive({
      ppp_bis(input$pais)
    })
    
    #Gráfico
    
  
    output$grafico <- renderPlot({
      datos <- resultados()
            
    
      ploter <- ggplot(datos, aes(x=diferencial, y=var_porct)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x) +
        labs(title = "Paridad de Poder de Compra Relativa", x= "Diferencial de inflación con USA", y="Variación del tipo de cambio (%)", caption = 'Fuente = BIS') +
        theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=12)) +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right", label.y.npc = 0.15,
                     formula = y ~ x, parse = TRUE, size = 3)
      
      ploter
    })
    
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('reporte', sep = '.', switch(
          input$format, PDF = 'pdf'
        ))
      },
      
    
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

        #library(rmarkdown)
        out <- render('Prueba_reporte.Rmd', switch(
          input$format,
          PDF = pdf_document()
        ))
        file.rename(out, file)
      }
    )
    
  }
)