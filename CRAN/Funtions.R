general_trend <- function(Data, responses, conf_level = 0.95) {
  if (!is.data.frame(Data)) stop("Data must be a data frame.")
  if (!all(c("month", "year") %in% names(Data))) stop("Data must contain 'month' and 'year' columns.")
  if (!all(responses %in% names(Data))) stop("All responses must be present in Data.")
  
  general <- data.frame(
    "Response" = character(),
    "Trend" = numeric(),
    "t" = numeric(),
    "pvalue" = numeric(),
    "conf_int_max" = numeric(),
    "conf_int_min" = numeric())
  
  for (i in 1:length(responses)) {
    table <- data.frame(
      "Response" = NA,
      "Trend" = NA,
      "t" = NA,
      "pvalue" = NA,
      "conf_int_max" = NA,
      "conf_int_min" = NA)
    table$Response <- responses[i]
    model_g <-lm(formula(paste(responses[i],
                               paste(predictors, collapse = "+"),
                               sep = " ~ ")),
                 data = Data)
    table$Trend <- model_g$coefficients[[2]] 
    table$t <- summary(model_g)$coefficients[2, 3] 
    table$pvalue <- summary(model_g)$coefficients[2, 4]
    table$conf_int_max <- confint(model_g, "year_month", level = 0.95)[, 2]
    table$conf_int_min <- confint(model_g, "year_month", level = conf_level)[, 1] 
    general <- rbind(general, table)
  }
  return(general)
}


predictors <- "year_month" # Variable independiente
responses <- "TMAX"  # Variables depend


general_trend(Data, responses,  conf_level = 0.95 )
