# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment
setwd(".")
library("tidyverse")
file_names <- list.files("./data")

generate_tibble <- function(filename, date_format) {
  temp_tibble <- read_csv(
    paste0("./data/", filename),
    na = c('calm',"Calm", " ", ""),
    skip = 7,
    col_types = cols(
      'Date' = col_date(format = date_format),
      'Evaporation (mm)' = col_double(),
      'Sunshine (hours)' = col_double(),
      '9am wind speed (km/h)' = col_double()
    )
  )
}
main_df <- generate_tibble(file_names[1],"%d/%m/%Y")

for (i in file_names[2:19]) {
  temp <- generate_tibble(i, "%d/%m/%Y")
  main_df <- rbind(main_df, temp)
  print(paste("FINISHED PARSING:", i))
}

for (i in file_names[20:length(file_names)]) {
  temp <- generate_tibble(i, "%m/%d/%Y")
  names(temp) <- names(main_df)
  print(names(main_df))
  main_df <- rbind(main_df, temp)
  print(paste("FINISHED PARSING:", i))
}

problems(main_df)

str(file_names)