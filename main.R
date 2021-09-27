# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment
setwd(".")
library("tidyverse")
file_names <- list.files("./data")

generate_tibble <- function(filename) {
  temp_tibble <- read_csv(
    paste0("./data/", filename),
    skip = 7,
    col_types = cols(
      Date = col_date(format = "%d/%m/%Y")
    )
  )
}
main_df <- generate_tibble(file_names[1])

for (i in file_names[2:length(file_names)]) {
  temp <- generate_tibble(i)
  main_df <- rbind(main_df, temp)
  print(paste("FINISHED PARSING:", i))
}

problems(main_df)

str(file_names)