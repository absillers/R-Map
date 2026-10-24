library(readxl)
library(tidyverse)
library(dplyr)

years <- seq(2016, 2024)
file.list <- paste0("MSA_M", years, '_dl.xlsx')
names <- paste0("MSA_", years)

data_list <- lapply(file.list, read_excel)

colnames_to_keep <- c("AREA_NAME", "AREA_TITLE", 'OCC_CODE', 'OCC_TITLE', 'OCC_GROUP', 'TOT_EMP', 'JOBS_1000', 'LOC QUOTIENT', 'LOC_QUOTIENT', 'H_MEAN', 'A_MEAN', 'H_MEDIAN', 'A_MEDIAN', "YEAR")
lookup <- c(AREA_TITLE = "AREA_NAME", 'LOC_QUOTIENT' = 'LOC QUOTIENT')

#REPLACE LOC_QUOTIENT WITH LOC  QUOTIENT

for (i in c(1:9)){
    data_list[[i]]$YEAR <- years[i]
    colnames(data_list[[i]]) <- toupper(colnames(data_list[[i]]))
    data_list[[i]] <- data_list[[i]][,colnames(data_list[[i]]) %in% colnames_to_keep]
    data_list[[i]] <- data_list[[i]] %>% rename(any_of(lookup))
    assign(names[i], as.data.frame(data_list[[i]]), envir = .GlobalEnv)
}

dfs <- c(MSA_2016, MSA_2017, MSA_2018, MSA_2019, MSA_2020, MSA_2021, MSA_2022, MSA_2023, MSA_2024)

add_OCC_GROUP_col <-function(df){
  ifelse(grepl("00-0000", df$OCC_CODE), "total", 
  ifelse(grepl("-0000", df$OCC_CODE), "major", "detailed"))}

for (i in dfs){
  print(length(colnames(i)))
  if (length(colnames(i)) == 11){
      i$OCC_GROUP <- add_OCC_GROUP_col(MSA_2024)}
  else{
    next
  }}



