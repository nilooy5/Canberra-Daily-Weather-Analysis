# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment
setwd(".")
library("tidyverse")
file_names <- list.files("./data_updated")

generate_tibble <- function(filename, date_format) {
  temp_tibble <- read_csv(
    paste0("./data_updated/", filename),
    na = c('calm',"Calm", " ", ""), # part B-1&2 answer
    skip = 7,
    col_types = cols(
      'Date' = col_date(format = date_format), # par B-5 answer
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
  main_df <- rbind(main_df, temp)
  print(paste("FINISHED PARSING:", i))
}

###############
#   PART B    #
###############

# 1
# DONE while importing table

# 2
# DONE while importing table

# 3: delete 90% NA columns
calculate_NA_ratio <- function(data, variable) {
  temp_var <- (length(which(is.na(data[variable]))) / length(data[[variable]]))
  return (temp_var)
}
x <- c()

for (item in names(main_df)) {
  ratio <- calculate_NA_ratio(main_df, item)
  # print(paste(item, "(", length(main_df[[item]]), ")", ":", length(which(is.na(main_df[item]))), ratio))
  if (ratio >= 0.9) {
    x <- append(x,item)
  }
}

print(x)
main_df <- main_df[,!(names(main_df) %in% x)]
names(main_df)

# 4: delete spaces from col names
main_df_col_names <- names(main_df)
names(main_df) <- gsub(" ", "_", main_df_col_names)

# 5
# done while importing table

# 6
main_df <- main_df %>%
  separate(col = Date, into = c ("Year", "Month", "Day"), "-")

# 7
main_df$Year <- parse_integer(main_df$Year)
main_df$Month <- parse_integer(main_df$Month)

main_df$Year <- as_factor(main_df$Year)
main_df$Month <- as_factor(main_df$Month)

# 8


###############
#   PART C    #
###############

###############
#   PART D    #
###############