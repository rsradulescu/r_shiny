#--------------------------
# Visualizacion de voluntariado (docentes y estudiantes) en Universidades Argentinas
# Lic. Rocio Radulescu
# rocior@unpata.edu.ar
# 20200720
#--------------------------

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(lubridate)
library(plotly)
library(RColorBrewer)


#leo y formateo datos de docentes y estudiantes
df_docente <-read_csv('voluntariado_universidad_docente.csv')  %>% 
  mutate(lat= as.numeric(latitude),
         long= as.numeric(longitude),
         call_center= as.numeric(`Call Center`),
         triag_hospital= as.numeric(`Triage hospitalario`),
         test_comunitario= as.numeric(`Testeos comunitarios`),
         test_masivo= as.numeric(`Testeos masivos`),
         asis_cent_aisl= as.numeric(`Asistencia en centros de aislamiento`),
         vacun_antigripal= as.numeric(`Vacunación Antigripal`),
         asis_viajero= as.numeric(`Asistencia a viajeros`),
         llam_geriatrico= as.numeric(`Llamados a geriátricos`),
         asis_hotel= as.numeric(`Asistencia en hoteles de repatriados`),
         ingreso_ciudad=as.numeric(`Cobertura puntos de ingreso a la ciudad`),
         proy_centro_aisl= as.numeric(`Org. e impl. de Proyecto de Centro de aislamiento`),
         salud_mental_gral= as.numeric(`Acomp.en salud Mental Com.en gral.`), 		
         salud_mental_comunidad= as.numeric(`Acomp en SM encomunidad FCM Willay`), 
         triag_protocolo= as.numeric(`Teletriage deriv de casos,activ protocolo covid`), 
         contacto_estrecho=as.numeric(`Seg.Contactos estrechos`), 
         hui_triag_hosp= as.numeric(`Formacion en el HUI p/triage Hosp.`), 
         cetec_pba= as.numeric(`CETEC PBA`), 
         comite_crisis= as.numeric(`Partici.Comite de crisis en CCEU P.Lara`), 
         receta_electronica= as.numeric(`Confeccion recetas electronicass`), 
         terreno_educ_salud= as.numeric(`Salidas a Terreno-Edcacion en Salud`), 
         material_audiovisual= as.numeric(`Confeccion material audiovisual`), 
         asis_lab_no_covid= as.numeric(`Asistencia Laborato No Covid`), 
         capac_equi_interprof= as.numeric(`Capacitacion Equipos Interprofesionales`), 
         plataf_testeo=as.numeric(`Proy.Plataforma de TESTEO`), 
         alimento_comendor=as.numeric(`Recoleccion alimentos p/comedor`),
         alim_saludable= as.numeric(`Elab alimentos saludables`),
         asist_hisopado=as.numeric(`Asistente de hisopado(escribiente)`),
         auditoria_centro_salud= as.numeric(`Auditoria en Centrosde salud`),
         gestion_stres= as.numeric(`Progr. De gestion de stres`),
         int_volunt= as.numeric(`Integracion Voluntarios al escenario curricular`),
         novedades_cient= as.numeric(`Busqueda y editorializacion de novedades cientificas`),
         publi_local= as.numeric(`Publ. local   reflexi. en torno a la pandemia`),
         bioseguridad= as.numeric(`Mesa de Bioseguridad`)
  )

df_estudiante <-read_csv('voluntariado_universidad_estudiante.csv')  %>% 
  mutate(lat=as.numeric(latitude),
         long=as.numeric(longitude),
         call_center= as.numeric(`Call Center`),
         triag_hospital= as.numeric(`Triage hospitalario`),
         test_comunitario= as.numeric(`Testeos comunitarios`),
         test_masivo= as.numeric(`Testeos masivos`),
         asis_cent_aisl= as.numeric(`Asistencia en centros de aislamiento`),
         vacun_antigripal= as.numeric(`Vacunación Antigripal`),
         asis_viajero= as.numeric(`Asistencia a viajeros`),
         llam_geriatrico= as.numeric(`Llamados a geriátricos`),
         asis_hotel= as.numeric(`Asistencia en hoteles de repatriados`),
         ingreso_ciudad=as.numeric(`Cobertura puntos de ingreso a la ciudad`),
         proy_centro_aisl= as.numeric(`Org. e impl. de Proyecto de Centro de aislamiento`),
         salud_mental_gral= as.numeric(`Acomp.en salud Mental Com.en gral.`), 		
         salud_mental_comunidad= as.numeric(`Acomp en SM encomunidad FCM Willay`), 
         triag_protocolo= as.numeric(`Teletriage deriv de casos,activ protocolo covid`), 
         contacto_estrecho=as.numeric(`Seg.Contactos estrechos`), 
         hui_triag_hosp= as.numeric(`Formacion en el HUI p/triage Hosp.`), 
         cetec_pba= as.numeric(`CETEC PBA`), 
         comite_crisis= as.numeric(`Partici.Comite de crisis en CCEU P.Lara`), 
         receta_electronica= as.numeric(`Confeccion recetas electronicass`), 
         terreno_educ_salud= as.numeric(`Salidas a Terreno-Edcacion en Salud`), 
         material_audiovisual= as.numeric(`Confeccion material audiovisual`), 
         asis_lab_no_covid= as.numeric(`Asistencia Laborato No Covid`), 
         capac_equi_interprof= as.numeric(`Capacitacion Equipos Interprofesionales`), 
         plataf_testeo=as.numeric(`Proy.Plataforma de TESTEO`), 
         alimento_comendor=as.numeric(`Recoleccion alimentos p/comedor`),
         alim_saludable= as.numeric(`Elab alimentos saludables`),
         asist_hisopado=as.numeric(`Asistente de hisopado(escribiente)`),
         auditoria_centro_salud= as.numeric(`Auditoria en Centrosde salud`),
         gestion_stres= as.numeric(`Progr. De gestion de stres`),
         int_volunt= as.numeric(`Integracion Voluntarios al escenario curricular`),
         novedades_cient= as.numeric(`Busqueda y editorializacion de novedades cientificas`),
         publi_local= as.numeric(`Publ. local   reflexi. en torno a la pandemia`),
         bioseguridad= as.numeric(`Mesa de Bioseguridad`)
  )

  #creo la tabla merge de ambos df
 df_estudiante <- df_estudiante %>% 
  mutate(origen='Estudiante')
 
 df_docente <- df_docente %>% 
   mutate(origen='Docente')
 
 #df principal
 df_todos <- rbind(df_estudiante, df_docente)
 #agrego sumatoria para trabajar
 df_todos$suma_por_univ = rowSums (df_todos[ , 4:32],na.rm = TRUE) 
 
# UI
ui <- bootstrapPage(

  navbarPage(theme = shinytheme("spacelab"), #otra opcion lumen 
             "Voluntariado Universitario 2020", id="nav",
  
    tabPanel("Mapa", 
      
      leafletOutput("map" ,width="100%",height="500px"),
      absolutePanel( fixed = TRUE,
                     top = 80, left = "auto", right = 20, 
                     bottom = "auto",
                    width = 200, height = "auto",
  
          #primer parametro: el selector de situaciones
          selectInput(inputId = "voluntario", #id
                      label = "Tipo de Voluntariado",#lo que figura
                      choices = c('Todos', 'Docente', 'Estudiante'),
                      selected = 1) #default?
         )
        ),#close tabpanel "mapa"
    tabPanel("Voluntarios por universidad",
          absolutePanel( fixed = TRUE,
                  top = 80, left = "auto", right = 10, 
                  bottom = "auto",
                  width = 200, height = "auto",
        
              selectInput(inputId = "voluntarioPlot", #id
                  label = "Tipo de Voluntariado",#lo que figura
                  choices = c('Todos','Docente', 'Estudiante'),
                  selected = 1), #default?
              hr()
              #downloadButton(outputId = 'download_graf_barra', label = 'Descarga')
          ),

          plotOutput ("graficobarra", width = "95%") #id
      ),#close tabpanel "grafico barra"
    
    # tabPanel("Gráfico de torta",
    #          absolutePanel( fixed = TRUE,
    #                         top = 80, left = "auto", right = 10, 
    #                         bottom = "auto",
    #                         width = 200, height = "auto",
    #                         
    #                         selectInput(inputId = "actividad_voluntariado", #id
    #                                     label = "Actividad de Voluntariado",#lo que figura
    #                                     choices = colnames(df_todos[4:26]),
    #                                     selected = 1), #default?
    #                         hr()
    #          ),
    #          plotOutput ("graficotorta") #id
    # ), # close tabpanel grafico torta
    # 
    tabPanel("Voluntarios por actividad",
             absolutePanel( fixed = TRUE,
                            top = 80, left = "auto", right = 10, 
                            bottom = "auto",
                            width = 200, height = "auto",
                            
                            selectInput(inputId = "input_universidad", #id
                                        label = "Universidad",#lo que figura
                                        choices = df_todos$universidad,
                                        selected = 1), #default?
                            hr()
                            
                           # downloadButton(outputId = 'download_graf_linea', label = 'Descarga')
             ),
             plotOutput ("graficolinea",width = "90%") #id
    ) # close panel grafico de linea
  )#close navpanel
)#close ui

# server
server <- function(input, output,session) {
  
  subset_voluntario = reactive({
    if (input$voluntario!="Todos") { #casos estudiante o docente
      df_todos %>% 
        filter(origen %in% input$voluntario)   
    } else #caso "todos"
      df_todos %>% 
        group_by(universidad,lat,long) %>% #obtengo la suma del total
        summarise(suma_por_univ=sum(suma_por_univ, na.rm = TRUE)) %>% 
        ungroup()
      })
      
  #---- panel mapa
  output$map <- renderLeaflet({
    
    colorEscala <- colorNumeric(
      palette = "RdYlBu",
      domain = subset_voluntario()$suma_por_univ,
      alpha = FALSE,
      reverse = FALSE
    )
    
    leaflet(df_todos) %>%
      addTiles() %>%
      # set view to Argentina
      setView(lng = -50.61, lat = -38.41, zoom = 4)%>%
      addCircles(~subset_voluntario()$long,
                 ~subset_voluntario()$lat ,
                 color = ~colorEscala(subset_voluntario()$suma_por_univ),
                 popup = paste("<b>",subset_voluntario()$universidad,"</b>",
                               "<br/>",
                               "Cantidad de voluntarios:",
                               subset_voluntario()$suma_por_univ,
                               input$voluntario),
                 weight = ~sqrt(as.numeric(subset_voluntario()$suma_por_univ)) * 2
                 
      )%>%
      addLegend("bottomright", 
              title= "Cantidad de voluntarios",
              pal = colorEscala, 
              values = ~subset_voluntario()$suma_por_univ,
              bins = 6,
              opacity = 1)
  })
  
  #---- panel barplots
  subset_voluntario_plot = reactive({
    if (input$voluntarioPlot!="Todos") { #casos estudiante o docente
      df_todos %>% 
        filter(origen %in% input$voluntarioPlot)
    } else #caso "todos"
      df_todos %>% 
      group_by(universidad,lat,long) %>% #obtengo la suma del total
      summarise(suma_por_univ=sum(suma_por_univ, na.rm = TRUE)) %>% 
      ungroup()
    
  })

  output$graficobarra <- renderPlot({
      par(mar=c(9,4,3,3)) #margenes del grafico
    
      graficoplot <- barplot(subset_voluntario_plot()$suma_por_univ,
            names.arg=subset_voluntario_plot()$universidad,
            las=2 , #orientacion de descripcion x
            xlab ='Universidades',
            ylab ='Cantidad de voluntarios',
            main="" ,
            #col= brewer.pal(5, "Set2"),
            col=rgb(0.1,0.4,0.6,0.6),
            ylim=range(pretty(c(0, subset_voluntario_plot()$suma_por_univ+50)))
            )  
    text(graficoplot, #texto sobre barras
         subset_voluntario_plot()$suma_por_univ,
         pos=3,
         paste(subset_voluntario_plot()$suma_por_univ)) 
      }) 
  
  #-boton download
  # output$download_graf_barra <- downloadHandler(
  #   filename=function(){
  #     paste('voluntariado-', Sys.Date(),'.png', sep="")
  #   },
  #   content = function(file){ # teniendo el objeto guardado en variable
  #     ggsave(file, plot = graf_plot(), width = 31, height = 17, 
  #            units = 'cm', device='png')
  #   }
  # )

  #---- panel piechart
  subset_actividad = reactive({ #obtengo solo la info del campo elegido
    colnames(df_todos)[colnames(df_todos) == input$actividad_voluntariado] <- "actividad"
    df_todos <- select(df_todos, universidad, origen,  actividad)
    df_todos %>%
    group_by(universidad, origen, actividad) %>%
    filter(!is.na(actividad)) %>% 
    summarise(valor=sum(actividad, na.rm = TRUE)) %>%
    ungroup()
  })
  
  output$graficotorta <- renderPlot({
    paleta <- brewer.pal(5, "Set2") #colores del pie. otros Paired, Dark2
    
    #obtener los porcentajes de tipo voluntario por actividad
    pct <- round(subset_actividad()$valor/sum(subset_actividad()$valor)*100)
    lbls <- paste(subset_actividad()$origen, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(subset_actividad()$valor,
        labels = lbls,
        border="white", col=paleta)
  })
  
  #---- panel grafico lineas
  subset_universidad = reactive({ #obtengo solo la info del campo elegido
    df_todos %>%
      filter(universidad %in% input$input_universidad) %>%
      gather(actividad, valor, 4:32) %>% #paso los colnames a vector 
      filter(!is.na(valor)) #elimino las barras sin datos
  })
  
  output$graficolinea <- renderPlot({
    ggplot(subset_universidad(),
           aes(x=subset_universidad()$actividad,
               y=subset_universidad()$valor),
               las=2  #orientacion de descripcion x
           )+
      geom_bar(aes(fill=origen),
               stat='identity', 
               position ='dodge')+
          labs(x='Actividades de voluntariado',
           y='Cantidad de voluntarios')+
      geom_text(aes(label=subset_universidad()$valor,
                    fill = subset_universidad()$origen),
                position=position_dodge(width=0.9),
                vjust=-.5)+
     theme(axis.text.x = element_text(angle = 45, hjust=1,size = 10),#para borrar el background
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.title.y = element_text( size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))
  })
}

# App: esta es la parte donde todo corre
shinyApp(ui = ui, server = server)