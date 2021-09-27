# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment
setwd(".")
file_names <- list.files("./data")
temp_file <- read.csv("./data/201808.csv", skip = 7)
str(file_names)