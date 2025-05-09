closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("Funciones.R")

# Data ----
directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/"
resultados_dir <- paste0(directorio, "Resultados_sesgadas/")
if (!dir.exists(resultados_dir)) {
  dir.create(resultados_dir)
}


# Obtener la lista de archivos que comienzan con "muestreo_lat_bias"
archivos <- list.files(path = "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/Ocurrencias_sesgadas", 
                       pattern = "^muestreo_temp_bias", 
                       full.names = TRUE)

archivos <- archivos[8:14]

# Crear una tabla vacía para almacenar los resultados finales
final_table <- data.frame(
  Records = character(),
  TT_error = numeric(),
  TC_error = numeric(),
  TA_error = numeric()
)

# Iterar sobre los archivos
for (archivo in archivos) {
  # Leer el archivo
  Data <- readRDS(archivo)
  Data <- Data[-8]
  Data$Año_Mes <- Data$month * 0.075
  Data$Año_Mes <- Data$year + Data$Año_Mes
  colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
  Data$TMAX <- Data$TMAX / 10
  Data$TMIN <- Data$TMIN / 10
  Data[, c(4:7)] <- round(Data[, c(4:7)], 4)
  Data$thermal_O <- gsub("SC", "TC", Data$thermal_O)
  
  x <- "Año_Mes" # Variable independiente
  y <- c("TMAX","TMIN")  # Variables dependiente
  
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
        names_sep = "_") %>%
      mutate(
        Thermal =
          case_when(
            p_TMIN > bonferroni  ~ "TC",
            p_TMAX > bonferroni  ~ "TC",
            p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN  < 0 ~ "TA",
            p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN  > 0 ~ "TT",
            p_TMAX <= bonferroni & Dif_pvalue_TMAX <= bonferroni & Trend_TMAX  < 0 ~ "TA",
            p_TMAX <= bonferroni & Dif_pvalue_TMAX <= bonferroni & Trend_TMAX  > 0 ~ "TT",
            TRUE ~ "TC")) %>%
      left_join(
        Data %>%
          group_by(species) %>%
          summarise(Registros = n()),
        by = c("Spp" = "species"))  %>% 
      separate(Spp,c("A", "Thermal_G", "B"), sep = "_", remove = FALSE) %>% 
      subset(select = -c(A,B))
    
    # Crear tabla de frecuencias, manejando posibles problemas de dimensiones
    a <- table(Tabla_sig_mean$Thermal_G, Tabla_sig_mean$Thermal)
    
    # Calcular los errores, manejando casos donde la tabla esté incompleta
    TT_error <- ifelse("TT" %in% rownames(a), sum(a["TT", c("TC", "TA")]), 0)
    TC_error <- ifelse("TC" %in% rownames(a), sum(a["TC", c("TT", "TA")]), 0)
    TA_error <- ifelse("TA" %in% rownames(a), sum(a["TA", c("TT", "TC")]), 0)
    
    # Almacenar los resultados en la tabla final
    final_table <- rbind(final_table, data.frame(
      Records = basename(archivo),
      TT_error = TT_error,
      TC_error = TC_error,
      TA_error = TA_error
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
    cols = c(TT_error, TC_error, TA_error),
    names_to = "Error_Type",
    values_to = "Count"
  )

# Crear el gráfico con ggplot



ggplot(final_table_long, aes(x = Records, y = Count, col = Error_Type, group = Error_Type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(
    values = c("TT_error" = "red", "TC_error" = "blue", "TA_error" = "green"),
    labels = c("TT", "TC", "TA")
  ) +
  labs(
    title = "Distribución de Errores",
    x = "Records",
    y = "Cantidad",
    color = "Tipo de Error"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
