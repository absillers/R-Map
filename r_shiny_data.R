library(readxl)
library(tidyverse)
library(dplyr)
library(sf)
library(data.table)
library(fuzzyjoin)

setwd('C:/Users/annas/OneDrive/R-Map')

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

#dfs <- list(MSA_2016, MSA_2017, MSA_2018, MSA_2019, MSA_2020, MSA_2021, MSA_2022, MSA_2023, MSA_2024)
dfs <- list(MSA_2022, MSA_2023)


###

r_shiny_bls_data <- do.call(rbind, dfs)

#add shapefile data
census_url <- "https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/metro-area-geography-reference/msa_county_reference22.txt"

clean_text <- stringi::stri_enc_toutf8(readLines(census_url, warn = FALSE), validate = TRUE)
clean_text[276] <- gsub("Ca�on City", "Canon City", clean_text[276]) #replace character 
clean_text <- clean_text[2:length(clean_text)] #drop the col names

GEOID <- lapply(lapply(clean_text, function(x) unlist(strsplit(x, '\",\"'))[1]), 
                function(x) gsub("\"", "", x))
msa_name <- lapply(lapply(clean_text, function(x) unlist(strsplit(x, '\",\"'))[2]), 
                   function(x) gsub("\"", "", x))
fipstate <- lapply(lapply(clean_text, function(x) unlist(strsplit(x, '\",\"'))[3]), 
                   function(x) gsub("\"", "", x))
fipscity <- lapply(lapply(clean_text, function(x) unlist(strsplit(x, '\",\"'))[4]), 
                   function(x) gsub("\"", "", x))
county_name <- lapply(lapply(clean_text, function(x) unlist(strsplit(x, '\",\"'))[5]), 
                      function(x) gsub("\"", "", x))

###
GEOID <- data.frame(
  GEOID  = unlist(GEOID),
  area = unlist(msa_name),
  state_FIP = unlist(fipstate),
  city_FIP = unlist(fipscity),
  county_name = unlist(county_name)
)

GEOID$AREA_TITLE <- gsub("(.*)(\\, \\w\\w)(.*)", "\\1\\2", GEOID$area)
GEOID <- unique(GEOID[,c(1,6)])

#remove second state from MSA
r_shiny_bls_data$AREA_TITLE <- gsub("(\\s\\D\\D)(-)(\\D\\D.*)", "\\1", r_shiny_bls_data$AREA_TITLE)
r_shiny_bls_data <- left_join(r_shiny_bls_data, GEOID, by = "AREA_TITLE") #find the GEOIDs

##still 81 left (including some PR)
GEOID$no_match <- ifelse(GEOID$GEOID %in% r_shiny_bls_data$GEOID, 1, 0)

GEOID$first_city <- sub("(^[^-]+)(-|,).*", "\\1", GEOID$AREA_TITLE)
GEOID$first_city <- ifelse(GEOID$no_match == 1, NA, GEOID$first_city)
GEOID$GEOID_alt <- GEOID$GEOID

r_shiny_bls_data$first_city <- sub("(^[^-]+)(-|,).*", "\\1", r_shiny_bls_data$AREA_TITLE)

r_shiny_bls_data <- left_join(r_shiny_bls_data, GEOID[c(4,5)], by = "first_city") #find the GEOIDs

r_shiny_bls_data$GEOID <- ifelse(is.na(r_shiny_bls_data$GEOID), r_shiny_bls_data$GEOID_alt, r_shiny_bls_data$GEOID)

r_shiny_bls_data$GEOID[r_shiny_bls_data$AREA_TITLE == "Portsmouth, NH"] <- NA
r_shiny_bls_data$GEOID[r_shiny_bls_data$AREA_TITLE == "Danbury, CT"] <- 14860
r_shiny_bls_data$GEOID[r_shiny_bls_data$AREA_TITLE == "The Villages, FL"] <- 48680
r_shiny_bls_data$GEOID[r_shiny_bls_data$AREA_TITLE == "Prescott, AZ"] <- 39150
r_shiny_bls_data$GEOID_alt <- NULL
#find GEOID

my_shapefile <- st_read("cb_2025_us_cbsa_500k.shp")


#find the polygons
r_shiny_bls_data <- r_shiny_bls_data %>%
  left_join(my_shapefile[c(4,10)], by = 'GEOID') %>%
  filter(!is.na(GEOID)) %>%
  st_as_sf(sf_column_name  = "geometry") %>%
  st_transform(4326)

r_shiny_bls_data$OCC_TYPE <- substr(r_shiny_bls_data$OCC_CODE, 1, 2)

r_shiny_bls_data$OCC_TITLE <- gsub(" Occupations", "", r_shiny_bls_data$OCC_TITLE)

r_shiny_bls_data <- r_shiny_bls_data %>%
  filter(YEAR == 2023)

saveRDS(r_shiny_bls_data, file = "r_shiny_bls_data.rds", compress = 'xz')

#rsconnect::writeManifest()
