closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("Funciones.R")

library(terra)

# Listar archivos
archivos <- list.files("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/data/", full.names = TRUE)
YEARS_TO_USE <- c(seq(1901, 2015, 10), 2016)

# Inicializar data frame vacío
data <- data.frame()

for (year in YEARS_TO_USE) {
  archivo <- archivos[grep(paste0(year), archivos)]
  a <- terra::rast(archivo)
  a <- terra::mean(a / 10)
  b <- as.data.frame(a, xy = TRUE)
  
  # Renombrar columnas
  colnames(b) <- c("x", "y", as.character(year))
  
  # Combinar datos
  if (nrow(data) == 0) {
    data <- b
  } else {
    data <- merge(data, b, by = c("x", "y"), all = TRUE)
  }
}

# Crear un data frame para los resultados
results <- data.frame(lat = data$y, long = data$x, Pendiente = NA, P_valor = NA, RSE = NA, Media = NA)

# Iterar sobre cada fila
for (i in 1:nrow(data)) {
  valores <- as.numeric(data[i, -c(1, 2)])  # Excluir las primeras columnas que no son valores de la serie
  modelo <- lm(valores ~ YEARS_TO_USE)
  
  results$Pendiente[i] <- round(coef(modelo)[2], 4)
  results$P_valor[i] <- round(summary(modelo)$coefficients[2, 4], 4)
  results$RSE[i] <- sqrt(deviance(modelo) / df.residual(modelo))
  results$Media[i] <- mean(valores)
}

# Tendencia
trend_raster <- as.data.frame(results[, c(2, 1, 3)], xy = TRUE)
colnames(trend_raster) <- c("long", "lat", "trend")
trend_raster <- terra::rast(trend_raster)

writeRaster(r, filename = "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/mean_tmax_spp_virtual.tif",  overwrite = TRUE)

# Media
tmax_mean_raster <- as.data.frame(results[, c(2, 1, 3)], xy = TRUE)
colnames(tmax_mean_raster) <- c("long", "lat", "Tmean")
tmax_mean_raster <- terra::rast(tmax_mean_raster)




# Reshape the data to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = `1901`:`2016`, names_to = "year", values_to = "value")

data_long$year <- as.numeric(as.character(data_long$year))
data_long$value <- as.numeric(as.character(data_long$value))


mask <- sf::read_sf("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/mask.shp")
world_map <- geodata::world(path = tempdir(), resolution = 2)
world_map <- sf::st_as_sf(world_map)



library(showtext) # Para usar fuentes personalizadas

# Cargar la fuente (ejemplo: Montserrat)
font_add_google("Montserrat", "montserrat")
showtext_auto()

# Gráfico mejorado
grafico_mejorado <- ggplot() +
  geom_point(data = data_long, aes(x = year, y = value),alpha = 0.5,  color = "#ADD8E6")+
  geom_smooth(data = data_long, aes(x = year, y = value),
              method = "loess",
              color = "#0072B2", #
              fill = "#ADD8E6", # Un azul más claro para el área de confianza
              alpha = 0.3) +
  labs(y = "Temperature (°C)") +
  theme_minimal(base_family = "calibri") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), angle = 270),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "gray90"), # Fondo blanco con borde gris
    panel.background = element_rect(fill = "white")
  )


ggsave("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/grafico_achatador2.jpg",  plot = grafico_mejorado, width = 10, height = 4, units = "cm")

showtext_auto(FALSE)

# Mostrar el gráfico mejorado
print(grafico_mejorado)

showtext_auto(FALSE) #desactiva showtext

library(ggplot2)
library(patchwork)
library(sf) # Asegúrate de tener instalada la librería sf para geom_sf

# Suponiendo que tienes los dataframes y objetos necesarios:
# data_long, world_map, results, mask

# Gráfico de línea de tendencia
ggplot() +
  geom_smooth(data = data_long, aes(x = year, y = value), method = "lm") +
  labs(y = "ºC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mapa de tendencia
tendencia <- ggplot() +
  geom_sf(data = world_map, fill = "gray80", color = "black") +
  geom_tile(data = results, aes(x = long, y = lat, fill = Pendiente)) +
  geom_sf(data = mask, fill = "transparent", color = "black") +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 70)) +
  theme_light() +
  theme(axis.title = element_blank()) +
  scale_fill_viridis_c()

# Mapa de temperatura media
media <- ggplot() +
  geom_sf(data = world_map, fill = "gray80", color = "black") +
  geom_tile(data = results, aes(x = long, y = lat, fill = Media)) +
  geom_sf(data = mask, fill = "transparent", color = "black") +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 70)) +
  theme_light() +
  theme(axis.title = element_blank()) +
  scale_fill_viridis_c()

# Composición de los gráficos usando patchwork
layout <- "
A C
B C
"

final_plot <- media + tendencia / grafico +
  plot_layout(design = layout)

(media + tendencia) / grafico

# Mostrar el gráfico final
print(final_plot)






























# Ajustar el modelo lineal y mostrar el resumen
lm_model <- lm(kk ~ years, data = data)
summary(lm_model)

# Mostrar el gráfico
print(p)

# Ajustar el modelo lineal y mostrar el resumen
lm_model <- lm(kk ~ years, data = data)
summary(lm_model)


results_raster <- as.data.frame(results[, c(2, 1, 6)], xy = TRUE)
colnames(results_raster) <- c("long", "lat", "mean")

# Convertir a un objeto raster
r <- terra::rast(results_raster)

###########################################################

archivos <- list.files("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/", full.names = T)
YEARS_TO_USE = c(seq(1901,2015,10),2016)

data <- data.frame()

for (i in seq_along(YEARS_TO_USE)) {
  archivo <- archivos[grep(paste0(YEARS_TO_USE[i]), archivos)]
  a <- terra::rast(archivo)
  a <- terra::median(a / 10)
  b <- as.data.frame(a, xy = TRUE)
  
  # Asegurar que las columnas tienen los nombres correctos
  colnames(b) <- c("x", "y", paste0(YEARS_TO_USE[i]))
  
  if (i == 1) {
    # Si es la primera iteración, inicializar `data` con `b`
    data <- b
  } else {
    # Unir los datos por coordenadas x e y
    data <- merge(data, b, by = c("x", "y"), all = TRUE)
  }
}


# Extraer años como variable independiente
years <- c(1901, 1911, 1921, 1931, 1941, 1951, 1961, 1971, 1981, 1991, 2001, 2011, 2016)

# Crear una nueva data frame para resultados
results <- data.frame(lat = data$y, long = data$x, Pendiente = NA, P_valor = NA, RSE = NA, Media = NA)

# Iterar sobre cada fila
for (i in 1:nrow(data)) {
  valores <- as.numeric(data[i, -c(1, 2)])  # Excluir las primeras columnas que no son valores de la serie
  modelo <- lm(valores ~ years)
  
  # Guardar los resultados
  
  results$long <- data[i,1]
  results$lat <- data[i,2]
  results$Pendiente[i] <- round(coef(modelo)[2], 4)
  results$P_valor[i] <- round(summary(modelo)$coefficients[2, 4], 4)
  results$RSE[i] <- sqrt(deviance(modelo) / df.residual(modelo))
  results$Media[i] <- mean(valores)
}


results_raster <- as.data.frame(results[,c(1,2,3)], xy = TRUE)
colnames(results_raster) <- c("long", "lat", "trend")


# Convertir la matriz en un objeto raster
r <- terra::rast(results_raster)

# Definir la extensión y resolución
extent(r) <- c(-180, 180, -90, 90)  # Extensión global
res(r) <- c(36, 18)  # Resolución de 36x18 grados

# Convertir el ráster en un data frame para agregar nombres de columna
results_raster <- as.data.frame(r, xy = TRUE)
colnames(results_raster) <- c("long", "lat", "trend")

# Guardar el ráster en un archivo
writeRaster(r, filename = "raster_output.tif", format = "GTiff", overwrite = TRUE)

# Guardar el data frame en un archivo CSV
write.csv(results_raster, "raster_data.csv", row.names = FALSE)

# Mostrar los primeros registros del data frame
head(results_raster)
terra::writeRaster(r, filename = "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/trends_clima.tif", overwrite = TRUE)
i=1
terra::plot(results_raster)


world_map <- geodata::world(path = tempdir(), resolution = 2)
world_map <- sf::st_as_sf(world_map)


  
ggplot() +
  geom_tile(data = results_raster, aes(x = lat, y = long, fill = trend)) 
  