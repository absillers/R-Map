source("r_shiny_data.R", local = TRUE)


fulldf <- data.frame(
  country = r_shiny_bls_data$OCC_TITLE[1:200],
  gender = r_shiny_bls_data$OCC_GROUP[1:200],
  age = r_shiny_bls_data$AREA_TITLE[1:200],
  value = as.numeric(r_shiny_bls_data$TOT_EMP[1:200]))
  
write.csv(fulldf, 'fulldf.csv')

fulldf <- read.csv("C:/Users/annas/OneDrive/R-Map/fulldf.csv")
