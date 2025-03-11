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




# Errores al 2% todos TC
res_02 <- readxl::read_xlsx("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/THERMAL/Resultados_aleatorias/Tabla_sig_mean_0.02.xlsx")

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
