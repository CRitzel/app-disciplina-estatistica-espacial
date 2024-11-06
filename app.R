source('MapaEmShiny.R')

ui <- fluidPage(
  titlePanel("Isolamento social em São Paulo - 2022"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Selecionar Data", value = min(dados$data), min = min(dados$data), max = max(dados$data)),
      br(),  # Adiciona um espaçamento
      wellPanel(
        style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px;",
        p("O Índice de Isolamento Social reflete a porcentagem da população que permanece em casa durante períodos críticos, como durante uma pandemia. Este índice ajuda a monitorar o comportamento de mobilidade da população em relação à segurança e à saúde pública.")
      ),
      wellPanel(
        style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px;",
        p("Cássio Ritzel, Gabriel Martinewski e Pietra Assmus")
      ),
    ),
    
    mainPanel(fluidRow(
      leafletOutput("map", height = "900px"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data filtered by the selected date
  filtered_data <- reactive({
    mapa_data %>%
      filter(!is.na(data ) & data  == as.Date(input$date))
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    poly <- filtered_data()$geometry[1]
    centroid <- st_centroid(poly)
    centroid_coords <- st_coordinates(centroid)
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 8)  # Centered near Americana, SP
  })
  
  # Update map based on date selection
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = mapa_data_na,
        fillColor = "grey",
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.5,  # Slightly different fill opacity
        popup = ~paste0("Município: ", municipio1  , "<br>Sem informação")
      ) %>%
      addPolygons(data = filtered_data(),
                  fillColor = ~pal(media_de_indice_de_isolamento),            # Use the palette you created
                  #values = mapa_data$Média.de.Índice.De.Isolamento,
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7,
                  popup = ~paste0("Município: ", municipio1  , "<br>",
                                  "População: ", populacao_estimada_2020 , "<br>",
                                  "Data: ", data , "<br>",
                                  "Indíce de isolamento social: ", media_de_indice_de_isolamento)) %>%
    addLegend("bottomright", 
              pal = pal, 
              values = filtered_data()$media_de_indice_de_isolamento,
              title = "Indice de Isolamento Social",
              opacity = 0.7)
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

