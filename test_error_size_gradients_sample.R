closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("V2/Fun_v2.R")

# Data ----
directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V4/"
resultados_dir <- paste0(directorio, "resultados_sesgadas/")
if (!dir.exists(resultados_dir)) {
  dir.create(resultados_dir)
}


# Obtener la lista de archivos que comienzan con "muestreo_lat_bias"
archivos <- list.files(path = "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V4/Ocurrencias_sesgadas", 
                       pattern = "^muestreo_lat_bias", 
                       full.names = TRUE)
archivos <- archivos[8:21]
# Crear una tabla vacía para almacenar los resultados finales
final_table <- data.frame(
  Records = character(),
  SA_error = numeric(),
  SC_error = numeric(),
  SD_error = numeric()
)

# Iterar sobre los archivos
for (archivo in archivos) {
  # Leer el archivo
  Data <- readRDS(archivo)
  Data$Año_Mes <- Data$month * 0.075
  Data$Año_Mes <- Data$year + Data$Año_Mes
  colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
  Data$TMAX <- Data$TMAX / 10
  Data$TMIN <- Data$TMIN / 10
  Data[, c(4:7)] <- round(Data[, c(4:7)], 4)
  
  x <- "Año_Mes" # Variable independiente
  y <- c("Lat") # Variables dependiente
  
  spp <- unique(Data$species)
  bonferroni <- 0.005 / length(spp)
  
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
    resultado <- spp_trend(Data, sp, y, n_min = 10)
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
          p_Lat > bonferroni ~ "SC",
          p_Lat <= bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat > 0 ~ "SA",
          p_Lat <= bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat < 0 ~ "SD",
          TRUE ~ "SC"
        )
      ) %>%
      left_join(
        Data %>%
          group_by(species) %>%
          summarise(Registros = n()),
        by = c("Spp" = "species"))  %>% 
      separate(Spp,c("A", "Spatial_G", "B"), sep = "_", remove = FALSE) %>% 
      subset(select = -c(A,B))
    
    # Crear tabla de frecuencias, manejando posibles problemas de dimensiones
    a <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)
    
    # Calcular los errores, manejando casos donde la tabla esté incompleta
    SA_error <- ifelse("SA" %in% rownames(a), sum(a["SA", c("SC", "SD")]), 0)
    SC_error <- ifelse("SC" %in% rownames(a), sum(a["SC", c("SA", "SD")]), 0)
    SD_error <- ifelse("SD" %in% rownames(a), sum(a["SD", c("SA", "SC")]), 0)
    
    # Almacenar los resultados en la tabla final
    final_table <- rbind(final_table, data.frame(
      Records = basename(archivo),
      SA_error = SA_error,
      SC_error = SC_error,
      SD_error = SD_error
    ))
    # Extraer la parte _1-2-4_
    sesgo <- sub(".*_(\\d+-\\d+-\\d+)_percent.*\\.RDS$", "\\1", basename(archivo))
    
    # Extraer la parte 0.001
    muestra <- sub(".*_percent_(\\d+\\.\\d+).*\\.RDS$", "\\1", basename(archivo))
    
  
    # Guardar Tabla_sig_mean en la carpeta results
      write_xlsx(Tabla_sig_mean, paste0(resultados_dir, "Tabla_sig_mean_", sesgo, "_", muestra, ".xlsx"))
  } else {
    warning(paste("No se generaron resultados para archivo:", archivo))
  }
}

# Mostrar la tabla final
print(final_table)

final_table$Records <- sub(".*_percent_([0-9.e-]+)\\.RDS$", "\\1", final_table$Records)
final_table$Records <- as.numeric(final_table$Records)

# Luego, ordena el dataframe
final_table <- final_table[order(final_table$Records), ]
writexl::write_xlsx(final_table, paste0(resultados_dir, "error_result_", sesgo,".xlsx") )

final_table_long <- final_table %>%
  pivot_longer(
    cols = c(SA_error, SC_error, SD_error),
    names_to = "Error_Type",
    values_to = "Count"
  )

# Crear el gráfico con ggplot



ggplot(final_table_long, aes(x = Records, y = Count, col = Error_Type, group = Error_Type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(
    values = c("SA_error" = "red", "SC_error" = "blue", "SD_error" = "green"),
    labels = c("SA", "SC", "SD")
  ) +
  labs(
    title = "Distribución de Errores por Registros 1-2-4",
    x = "Records",
    y = "Cantidad",
    color = "Tipo de Error"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
