closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("V2/Fun_v2.R")

#   SIG	TREND
#TT	***	+
#TA	***	-
#TC	
#  
#SA	***	+
#SD	***	-
#SC	

# Data ----
directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V4/"
resultados_dir <- paste0(directorio, "resultados/V2/")
if (!dir.exists(resultados_dir)) {
  dir.create(resultados_dir)
}
# Load the required data
Data <- readRDS(paste0(directorio,"lista completa SA_SC_SD.RDS"))
#lista completa SA_SC_SD.RDS

# Preprocess Data
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX / 10
Data$TMIN <- Data$TMIN / 10
Data[, c(4:7)] <- round(Data[, c(4:7)], 4)

x <- "Año_Mes" # Variable independiente
y <- c("Lat") # Variables dependiente



secuencia <- c(0.00005, 0.0001, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.02)



# Crear una tabla vacía para almacenar los resultados finales
final_table <- data.frame(
  Records = numeric(),
  SA_error = numeric(),
  SC_error = numeric(),
  SD_error = numeric()
)

# Iterar sobre la secuencia de muestreos
for (i in seq_along(secuencia)) {
  # Realizar un muestreo de los datos
  sampled_data <- Data %>% slice_sample(prop = secuencia[i], replace = FALSE)
  spp <- unique(sampled_data$species)
  bonferroni <- 0.005 / length(spp)
  # Verificar si hay datos después del muestreo
  if (nrow(sampled_data) == 0) {
    warning(paste("Muestreo vacío para secuencia:", secuencia[i]))
    next
  }
  
  # Configurar clúster de paralelización
  numCores <- detectCores() - 10
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Ejecutar el cálculo de tendencias en paralelo
  tabla_ind <- foreach(
    sp = spp,
    .combine = rbind,
    .packages = c("tidyverse", "jtools")
  ) %dopar% {
    resultado <- spp_trend(sampled_data, sp, y, n_min = 10)
    if (!is.null(resultado) && nrow(resultado) > 0) {
      resultado[, 4] <- round(resultado[, 4], 4)  # Redondear columna 4
    }
    return(resultado)
  }
  
  # Detener el clúster
  stopCluster(cl)
  
  
  
  # Procesar la tabla resultante
  if (!is.null(tabla_ind) && nrow(tabla_ind) > 0) {
    Tabla_sig_mean <- tabla_ind %>%
      dplyr::select(c(Spp, Trend, t, p, Variable, Dif_t, Dif_pvalue)) %>%
      pivot_wider(
        names_from = Variable, 
        values_from = c(Trend, t, p, Dif_t, Dif_pvalue),
        names_sep = "_"
      ) %>%
      mutate(
        Spatial = case_when(
          p_Lat >= bonferroni ~ "SC",
          p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat > 0 ~ "SA",
          p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat < 0 ~ "SD",
          TRUE ~ "SC"
        )
      ) %>%
      # Separar y validar la columna Spp
      separate(Spp, c("A", "Spatial_G", "B"), sep = "_", remove = FALSE, fill = "right") %>%
      mutate(Spatial_G = ifelse(is.na(Spatial_G), "Unknown", Spatial_G))
    
    # Crear tabla de frecuencias, manejando posibles problemas de dimensiones
    a <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)
    
    # Calcular los errores, manejando casos donde la tabla esté incompleta
    SA_error <-  a[1, 2] + a[1, 3]
    SC_error <- a[2, 1] + a[2, 3]
    SD_error <- a[3, 1] + a[3, 2]
    
    # Almacenar los resultados en la tabla final
    final_table <- rbind(final_table, data.frame(
      Records = secuencia[i],
      SA_error = SA_error,
      SC_error = SC_error,
      SD_error = SD_error
    ))
    write_xlsx(Tabla_sig_mean, paste0(resultados_dir,"resultados_aleat_SA_SC_SD_", secuencia[i], ".xlsx"))
  } else {
    warning(paste("No se generaron resultados para secuencia:", secuencia[i]))
  }
}

# Mostrar la tabla final
print(final_table)

final_table_long <- final_table %>%
  pivot_longer(
    cols = c(SA_error, SC_error, SD_error),
    names_to = "Error_Type",
    values_to = "Count"
  )

# Crear el gráfico con ggplot
ggplot(final_table_long, aes(x = Records, y = Count, col = Error_Type)) +
  geom_point() + 
  geom_line()+
  scale_fill_manual(
    values = c("SA_error" = "red", "SC_error" = "blue", "SD_error" = "green"),
    labels = c("SA", "SC", "SD")
  ) +
  labs(
    title = "Distribución de Errores por Records",
    x = "Records",
    y = "Cantidad",
    fill = "Tipo de Error"
  ) +
  theme_minimal()



