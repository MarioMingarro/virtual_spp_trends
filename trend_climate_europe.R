closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("Funciones.R")

library(terra)

# Listar archivos
archivos <- list.files("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/", full.names = TRUE)
YEARS_TO_USE <- c(seq(1901, 2015, 10), 2016)

# Inicializar data frame vacío
data <- data.frame()

for (year in YEARS_TO_USE) {
  archivo <- archivos[grep(paste0(year), archivos)]
  a <- terra::rast(archivo)
  a <- terra::median(a / 10)
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

# Crear raster con los resultados
results_raster <- as.data.frame(results[, c(2, 1, 3)], xy = TRUE)
colnames(results_raster) <- c("long", "lat", "trend")

# Convertir a un objeto raster
r <- terra::rast(results_raster)

# Guardar el ráster en un archivo
writeRaster(r, filename = "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/Clima_Europa/mean_tmax_spp_virtual.tif",  overwrite = TRUE)


# Reshape the data to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = `1901`:`2016`, names_to = "year", values_to = "value")

# Graficar los datos con ggplot
ggplot(data_long, aes(x = year, y = value)) +
  geom_point(size = 3) + # Puntos de los datos
  geom_smooth(method = lm) + # Línea de tendencia
  labs(title = "Gráfico de valores a través de los años",
       x = "Año",
       y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar el gráfico
print(p)



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
  