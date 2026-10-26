library(readxl)
library(tidyverse)
library(dplyr)
library(sf)
library(data.table)

years <- seq(2016, 2024)
file.list <- paste0("MSA_M", years, '_dl.xlsx')
names <- paste0("MSA_", years)

data_list <- lapply(file.list, read_excel)

#rename columns
colnames_to_keep <- c("AREA_NAME", "AREA_TITLE", 'OCC_CODE', 'OCC_TITLE', 'OCC_GROUP', 'TOT_EMP', 'JOBS_1000', 'LOC QUOTIENT', 'LOC_QUOTIENT', 'H_MEAN', 'A_MEAN', 'H_MEDIAN', 'A_MEDIAN', "YEAR")
lookup <- c(AREA_TITLE = "AREA_NAME", 'LOC_QUOTIENT' = 'LOC QUOTIENT')

#add OCC_GROUP col
add_OCC_GROUP_col <-function(df){
  df$OCC_GROUP <- ifelse(grepl("00-0000", df$OCC_CODE), "total", 
                         ifelse(grepl("-0000", df$OCC_CODE), "major", "detailed"))
  return(df)}

for (i in c(1:9)){
    data_list[[i]]$YEAR <- years[i]
    colnames(data_list[[i]]) <- toupper(colnames(data_list[[i]]))
    data_list[[i]] <- data_list[[i]][,colnames(data_list[[i]]) %in% colnames_to_keep]
    data_list[[i]] <- data_list[[i]] %>% rename(any_of(lookup))
    data_list[[i]] <- add_OCC_GROUP_col(data_list[[i]])
    assign(names[i], as.data.frame(data_list[[i]]), envir = .GlobalEnv)
}

dfs <- list(MSA_2016, MSA_2017, MSA_2018, MSA_2019, MSA_2020, MSA_2021, MSA_2022, MSA_2023, MSA_2024)


###

r_shiny_bls_data <- do.call(rbind, dfs)

#add shapefile data
census_url <- "https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/metro-area-geography-reference/msa_county_reference22.txt"

clean_text <- stringi::stri_enc_toutf8(readLines(census_url, warn = FALSE), validate = TRUE)
clean_text[276] <- gsub("Ca�on City", "Canon City", clean_text[276])

GEOID <- list()
msa_name <- list()
fipstate <- list()
fipscity <- list()
county_name <- list()


###

for (i in c(2:length(clean_text))){
 string <- unlist(strsplit(clean_text[i], ','))
  GEOID <- c(GEOID, string[1])
  msa_name <- c(GEOID, string[2])
  fipstate <- c(GEOID, string[3])
  fipscity <- c(GEOID, string[4])
  county_name <-c(GEOID, string[5])}














my_shapefile <- st_read("C:/Users/annas/OneDrive/Github/plotly_bls_map/cbsa_shape/cb_2025_us_cbsa_500k.shp")





r_shiny_bls_data <- r_shiny_bls_data %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(my_shapefile[c(4,10)], by = 'GEOID')%>%
  filter(!is.na(GEOID)) %>%
  filter(!is.na(Percent)) %>%
  st_as_sf(sf_column_name  = "geometry") %>%
  st_transform(4326)





msa_employment_2025 <- read.csv("C:/Users/annas/OneDrive/Github/plotly_bls_map/plotly_msa_employment.csv")
my_shapefile <- st_read("C:/Users/annas/OneDrive/Github/plotly_bls_map/cbsa_shape/cb_2025_us_cbsa_500k.shp")

occupations <- unique(msa_employment_2025$Occupation)
occupations_names <- gsub("Occupations \\(.*", "", occupations)


msa_employment_2025 <- msa_employment_2025 %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(my_shapefile[c(4,10)], by = 'GEOID')%>%
  filter(!is.na(GEOID)) %>%
  filter(!is.na(Percent)) %>%
  st_as_sf(sf_column_name  = "geometry") %>%
  st_transform(4326)
