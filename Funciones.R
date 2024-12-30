packages.to.use <- c("readxl",
                     "tidyverse",
                     "writexl",
                     "tictoc",
                     "jtools",
                     "viridisLite",
                     "rnaturalearth",
                     "rnaturalearthdata",
                     "parallel",
                     "foreach",
                     "doParallel",
                     "vcd")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) & package == "XX" ) { devtools::install_github("GITHUB", dependencies = TRUE) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}
rm(packages.to.use,package)
# Funciones -----

# general_trend ----
general_trend <- function(Data, y) {
  tabla_general <- data.frame(
    "Variable" = character(),
    "Trend" = numeric(),
    "t" = numeric(),
    "p" = numeric(),
    "P95_max" = numeric(),
    "P95_min" = numeric()
  )
  for (i in 1:length(y)) {
    # Bucle para calcular las estadísticas de todas las variables independientes
    tabla <-data.frame(
      "Variable" = NA,
      "Trend" = NA,
      "t" = NA,
      "p" = NA,
      "P95_max" = NA,
      "P95_min" = NA)
    tabla$Variable <- y[i]  # Rellena primera columna con el nombre de la variable
    model_g <-
      lm(formula(paste(y[i],  # Crea formula utilizando la variable del bucle
                       paste(x, collapse = "+"),
                       sep = " ~ ")),
         data = Data)
    tabla$Trend <- model_g$coefficients[[2]] # Tendencia
    tabla$t <- summary(model_g)$coefficients[2, 3] # t del modelo
    tabla$p <- summary(model_g)$coefficients[2, 4] # p del modelo
    tabla$P95_max <- confint(model_g, "Año_Mes", level = .95)[, 2] # Intervalo de confianza max del 95%
    tabla$P95_min <- confint(model_g, "Año_Mes", level = .95)[, 1] # Intervalo de confianza min del 95%
    tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una de las tablas individuales
  }
  return(tabla_general)
}

# spp_trend ----
spp_trend <- function(Data, spp, y, n_min = 50) {
  tabla_ind <- data.frame(
    "Spp" = NA,
    "Variable" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA,
    "Dif_t" = NA,
    "Dif_pvalue" = NA)
  
  tabla_ind <- tabla_ind[-1,]
  
  # Bucle para calcular las tendencias de cada una de las especies
  for (n in 1:length(spp)){
    # Bucle para actuar sobre cada una de las especies
    # Filtra la especie  
    ind <- Data %>%
      dplyr::filter(species == spp[n]) %>%
      mutate(group = "i")
    
    gen <- Data %>%
      mutate(group = "g")
    
    dat <- rbind(gen, ind)
    
    if (nrow(ind) > n_min) {
      # Condicional "SI" para seleccionar aquellas especies con mas de 10 registros
      for (i in 1:length(y)) {
        # Bucle para cada una de las variables independientes
        tryCatch({
          # Implementa código que debe ejecutarse cuando se produce la condición de error
          tabla <-
            data.frame(
              # Crea tabla vacía para después unificar a tabla de resultados
              "Spp" = NA,
              "Variable" = NA,
              "Trend" = NA,
              "t" = NA,
              "p" = NA,
              "P95_max" = NA,
              "P95_min" = NA,
              "Dif_t" = NA,
              "Dif_pvalue" = NA
            )
          
          # Crea de nuevo el modelo general utilizando todos los datos
          model_g <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = gen)
          tabla$Spp <- unique(ind$species)
          tabla$Variable <- y[i]
          
          # Crea el modelo de cada especie para la posterior comparación, De aqui servirán los datos de tendencias
          model_i <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = ind)
          
          # Añade resultados en la tabla
          tabla$Trend <- model_i$coefficients[[2]]
          tabla$t <- summary(model_i)$coefficients[2, 3]
          tabla$p <- summary(model_i)$coefficients[2, 4]
          tabla$P95_max <- confint(model_i, "Año_Mes", level = .95)[, 2]
          tabla$P95_min <- confint(model_i, "Año_Mes", level = .95)[, 1]
          
          # Crea de nuevo el modelo de comparación y ver si existe diferencias acxorde al grupo
          model_int <- lm(formula(paste(y[i], paste(
            x, "*group", collapse = "+"
          ), sep = " ~ ")), data = dat)
          
          
          tabla$Dif_t <- summary(model_int)$coefficients[4, 3]
          tabla$Dif_pvalue <- summary(model_int)$coefficients[4, 4]
          
          tabla_ind <- rbind(tabla_ind, tabla) 
          
        }, error = function(e) {
          # Si la función tryChach da error ejecuta esta parte del codigo
          cat(
            paste0("WARNING: Specie ", ind[1, 1], " variable (", y[i], ") has"),
            # Indica que especie tiene el problema y por qué
            conditionMessage(e),
            "\n"
          )
        })
      }
    } else{
      print(paste0("Data for ", ind[1, 1], " specie are insufficient")) # Si en el condicional no hay suficientes registros
      # para una especie (<50) expresa el mensaje
    }
  }
  return(tabla_ind)
}
