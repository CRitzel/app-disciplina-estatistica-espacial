source('MapaEmShiny.R')

# FRONT END
ui <- fluidPage(
  titlePanel("Isolamento social em São Paulo - 2022"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Selecionar Data", value = min(dados$data), min = min(dados$data), max = max(dados$data)),
      br(),
      wellPanel(
        style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px;",
        p("O Índice de Isolamento Social reflete a porcentagem da população que permanece em casa durante períodos críticos, como durante uma pandemia. Este índice ajuda a monitorar o comportamento de mobilidade da população em relação à segurança e à saúde pública.")
      ),
      wellPanel(
        style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px;",
        p("Cássio Ritzel, Gabriel Martinewski e Pietra Assmus")
      )
    ),
    
    mainPanel(
      fluidRow(
        leafletOutput("map", height = "650px"),
        br(),
        br(),
        plotlyOutput("city_plot", height = "400px")
      )
    )
  )
)

# BACK END
server <- function(input, output, session) {
  
  #Filtra a data
  filtered_data <- reactive({
    mapa_data %>%
      filter(!is.na(data) & data == as.Date(input$date))
  })
  
  # mapa filtrado pela data
  output$map <- renderLeaflet({
    
    # centroide no mapa pra centralizar a incialização
    poly <- filtered_data()$geometry[1]
    centroid <- st_centroid(poly)
    centroid_coords <- st_coordinates(centroid)
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 8)
  })
  
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = mapa_data_na,
        fillColor = "grey",
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.5,
        popup = ~paste0("Município: ", municipio1, "<br>Sem informação")
      ) %>%
      addPolygons(
        data = filtered_data(),
        fillColor = ~pal(media_de_indice_de_isolamento),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste0("Município: ", municipio1, "<br>",
                        "População: ", populacao_estimada_2020, "<br>",
                        "Data: ", data, "<br>",
                        "Índice de isolamento social: ", media_de_indice_de_isolamento),
        layerId = ~municipio1 
      ) %>%
      addLegend("topright",
                pal = pal,
                values = filtered_data()$media_de_indice_de_isolamento,
                title = "Índice de Isolamento Social",
                opacity = 0.7)
  })
  
  clicked_city <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    clicked_city(input$map_shape_click$id) 
  })
  
  
  output$city_plot <- renderPlotly({
    city <- clicked_city()
    
    
    if (is.null(city)) return(NULL)
    
    # Filter data for the selected city across all dates
    city_data <- mapa_data %>% filter(municipio1 == city)
    
    # Ensure city data is valid before plotting
    if (nrow(city_data) == 0) return(NULL)
    
    selected_date_data <- filtered_data() %>% 
      filter(municipio1 == city) %>% 
      slice(1) 
    
    # Create a plot of isolation index over time for the selected city
    p <- ggplot(city_data, aes(x = as.Date(data), y = media_de_indice_de_isolamento)) +
      geom_line(color = "#FEB24C") +
      geom_point(aes(text = paste("Data:", as.Date(data),
                                  "<br>Índice de Isolamento:", media_de_indice_de_isolamento)),
                 color = "#FEB24C") +
      geom_point(data = selected_date_data, aes(x = as.Date(data), y = media_de_indice_de_isolamento), 
                 color = "red", size = 2) +
      labs(
        title = paste("Índice de Isolamento Social em", city),
        x = "Data",
        y = "Índice de Isolamento Social"
      )
    
    # Convert ggplot to plotly, specifying to use only the 'text' aesthetic for tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

