directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/SPATIAL/Ocurrencias_aleatorias/"
Data <- readRDS(paste0(directorio, "muestreo_aleat_SA_SC_SD_percent_0.005.RDS")) # RUTA A LOS DATOS

packages <- c(
  "shiny",
  "tidyverse",
  "sf",
  "viridis",
  "jtools",
  "rnaturalearth",
  "rnaturalearthdata")

packages <- unique(packages)
for(package in packages) {
  print(package)
  if( ! package %in% rownames(installed.packages())){install.packages(package)}
  suppressWarnings(library(package, character.only = TRUE) )
}
rm(packages,package)



Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","spatial_O","Año_Mes")
Data$TMAX <- Data$TMAX / 10
Data$TMIN <- Data$TMIN / 10

Data[, c(4:7)] <- round(Data[, c(4:7)], 4)

world_map <- ne_countries(scale = "medium", returnclass = "sf")

spp <- unique(Data$species)

ui <- fluidPage(
  titlePanel("Visualización de Datos de Especies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_species", "Selecciona la especie:", choices = spp),
      uiOutput("date_slider_ui")),
    mainPanel(
      plotOutput("speciesMap"),
      plotOutput("trendPlot"))))


server <- function(input, output, session) {
  species_data <- reactive({
    Data %>%
      filter(species == input$selected_species)
  })
  output$date_slider_ui <- renderUI({
    date_range <- species_data() %>%
      summarize(min_year = min(year), max_year = max(year))
    
    sliderInput("selected_year", "Selecciona el rango de años:",
                min = date_range$min_year, max = date_range$max_year,
                value = c(date_range$min_year, date_range$max_year),
                step = 1, round = TRUE)
  })
  output$speciesMap <- renderPlot({
    req(input$selected_year)
    filtered_data <- species_data() %>%
      filter(year >= input$selected_year[1] & year <= input$selected_year[2])
    
    ggplot() +
      geom_sf(data = world_map) +
      geom_point(data = filtered_data, aes(Long, Lat, col = as.factor(year)), alpha = 0.2) +
      labs(title = input$selected_species, subtitle = paste0("count = ", nrow(filtered_data))) +
      scale_colour_viridis_d(option = "D", alpha = 0.8) +
      coord_sf(xlim = c(-20, 50), ylim = c(35, 75), expand = FALSE) +
      theme(axis.text = element_text(angle = 45, size = 8),
            axis.title = element_blank(),
            legend.title = element_blank())
  })
  
  output$trendPlot <- renderPlot({
    req(input$selected_species)
    
    filtered_data <- species_data()
    
    ggplot() +
      geom_smooth(data = Data, aes(x = Año_Mes, y = Lat, color = "Lat - General"), method = "lm", se = FALSE) +
      geom_smooth(data = filtered_data, aes(x = Año_Mes, y = Lat, color = "Lat - Specie"), method = "lm", se = FALSE) +
      ggtitle(paste0(input$selected_species)) +
      labs(x = "Year", y = "Latitude", color = "Legend", fill = "Legend") +
      scale_color_manual(values = c("Lat - General" = "magenta", 
                                    "Lat - Specie" = "blue"))+
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

