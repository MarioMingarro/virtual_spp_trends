closeAllConnections()
rm(list=(ls()[ls()!="Data"]))
gc(reset=TRUE)
source("Funciones.R")

Data <- readRDS("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/lista completa TA_TC_TT.RDS")

Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN", "thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX / 10
Data$TMIN <- Data$TMIN / 10
Data[, c(4:7)] <- round(Data[, c(4:7)], 4)

x <- "Año_Mes" # Variable independiente
y <- c("TMAX","TMIN")  # Variables dependiente


subset_data <- Data %>% sample_frac(0.025)
write_rds(subset_data, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/Ocurrencias_aleatorias/muestreo_aleat_TA_TC_TT_percent_0.025.RDS")


  
  
mutate(
  Thermal =
    case_when(
      # Si los p-valores de p_TMIN o p_TMAX son mayores que el umbral de Bonferroni,
      # significa que no hay suficiente evidencia estadística para detectar un cambio térmico significativo.
      # Por lo tanto, se clasifica como "TC" (Térmicamente Conservador).
      p_TMIN > bonferroni  ~ "TC",
      p_TMAX > bonferroni  ~ "TC",
      
      # Si el p-valor de la tendencia mínima (p_TMIN) es menor que el umbral de Bonferroni
      # Y el p-valor de la diferencia con la general (Dif_pvalue_TMIN) también es significativo
      # Y la tendencia de la temperatura mínima (Trend_TMIN) es NEGATIVA
      # Entonces significa que la temperatura mínima está disminuyendo significativamente,
      # por lo que se clasifica como "TA" (posiblemente enfriamiento).
      p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN < 0 ~ "TA",
      
      # Si el p-valor de la tendencia mínima (p_TMIN) es significativo (≤ bonferroni)
      # Y el p-valor de la diferencia con la general (Dif_pvalue_TMIN) también es significativo
      # Y la tendencia de la temperatura mínima (Trend_TMIN) es POSITIVA
      # Entonces significa que la temperatura mínima está aumentando significativamente,
      # por lo que se clasifica como "TT" (posiblemente calentamiento).
      p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN > 0 ~ "TT",
      
      # Si el p-valor de la tendencia máxima (p_TMAX) es significativo (≤ bonferroni)
      # Y el p-valor de la diferencia con la general (Dif_pvalue_TMAX) también es significativo
      # Y la tendencia de la temperatura máxima (Trend_TMAX) es NEGATIVA
      # Entonces significa que la temperatura máxima está disminuyendo significativamente,
      # por lo que se clasifica como "TA" (posiblemente enfriamiento).
      p_TMAX <= bonferroni & Dif_pvalue_TMAX <= bonferroni & Trend_TMAX < 0 ~ "TA",
      
      # Si el p-valor de la tendencia máxima (p_TMAX) es significativo (≤ bonferroni)
      # Y el p-valor de la diferencia con la general (Dif_pvalue_TMAX) también es significativo
      # Y la tendencia de la temperatura máxima (Trend_TMAX) es POSITIVA
      # Entonces significa que la temperatura máxima está aumentando significativamente,
      # por lo que se clasifica como "TT" (posiblemente calentamiento).
      p_TMAX <= bonferroni & Dif_pvalue_TMAX <= bonferroni & Trend_TMAX > 0 ~ "TT",
      
      # Si ninguna de las condiciones anteriores se cumple, se clasifica como "TC" (sin cambios significativos).
      TRUE ~ "TC"
    )
)


# Errores al 2% todos TC
res_02 <- readxl::read_xlsx("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/Resultados_aleatorias/Tabla_sig_mean_0.001.xlsx")

spp <- unique(res_02$Spp)
bonferroni <- 0.005 / length(spp)
res_02 <- res_02 %>% mutate(
  Thermal =
  case_when(
    p_TMIN > bonferroni  ~ "TC",
    p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN  < 0 ~ "TA",
    p_TMIN <= bonferroni & Dif_pvalue_TMIN <= bonferroni & Trend_TMIN  > 0 ~ "TT",
    TRUE ~ "TC"))

desajustes <- res_02 %>%
  filter(Thermal_G != Thermal) 

spp_SC_49 <- filter(Data, species == "virtualsp_SC_49")
spp_SC_49 <- general_trend(Data = spp_SC_49, y)
spp_SC_2 <- filter(Data, species == "virtualsp_SC_2")
spp_SC_2 <- general_trend(Data = spp_SC_2, y)
spp_SC_225 <- filter(Data, species == "virtualsp_SC_225")
spp_SC_225 <- general_trend(Data = spp_SC_225, y)
spp_SC_199 <- filter(Data, species == "virtualsp_SC_199")
spp_SC_199 <- general_trend(Data = spp_SC_199, y)

unique(Data$species)
spp_SC_101 <- filter(Data, species == "virtualsp_SC_101")
spp_SC_101 <- general_trend(Data = spp_SC_101, y)
spp_SC_150 <- filter(Data, species == "virtualsp_SC_150")
spp_SC_150 <- general_trend(Data = spp_SC_150, y)
spp_SC_156 <- filter(Data, species == "virtualsp_SC_156")
spp_SC_156 <- general_trend(Data = spp_SC_156, y)
