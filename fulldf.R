source("r_shiny_data.R", local = TRUE)


choices <- r_shiny_bls_data %>% 
  filter(OCC_TYPE == '29' & OCC_GROUP == 'detailed') %>% 
  pull(OCC_TITLE) %>% 
  unique()


