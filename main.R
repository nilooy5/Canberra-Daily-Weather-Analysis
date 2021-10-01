# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment
# setwd(".")
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
na_column_names <- c()

for (item in names(main_df)) {
  ratio <- calculate_NA_ratio(main_df, item)

  if (ratio >= 0.9) {
    na_column_names <- append(na_column_names,item)
  }
}

print(na_column_names)
main_df <- main_df[,!(names(main_df) %in% na_column_names)]
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

set_na_to_median <- function(data_frame, column_name) {

  median_value <- median(data_frame[[column_name]], na.rm = TRUE)
  print(paste(column_name, ":", median_value))

  data_frame[which(is.na(data_frame[column_name])),column_name] <- median_value
}

col_type_vector <- sapply(main_df, typeof)
for (item in names(col_type_vector)[3:length(names(col_type_vector))]) {
  if (col_type_vector[item] == "integer" | col_type_vector[item] == "double") {
    set_na_to_median(main_df, item)
  }
}

# for test
main_df[c(575), "Speed_of_maximum_wind_gust_(km/h)"]
# set_na_to_median("Speed_of_maximum_wind_gust_(km/h)")
median_speed <- median(main_df[["Speed_of_maximum_wind_gust_(km/h)"]], na.rm = TRUE)
na_speed <- which(is.na(main_df["Speed_of_maximum_wind_gust_(km/h)"]))
main_df[na_speed, "Speed_of_maximum_wind_gust_(km/h)"] <- median_speed
which(is.na(main_df["Speed_of_maximum_wind_gust_(km/h)"]))


###############
#   PART C    #
###############

# 1
print_highlights <- function(col_name) {
  print(paste(col_name, "(min):", min(main_df[[col_name]])))
  print(paste(col_name, "(median) :", median(main_df[[col_name]])))
  print(paste(col_name, "(mean) :", mean(main_df[[col_name]])))
  print(paste(col_name, "(max) :", max(main_df[[col_name]])))
}

temperature_wind_vector <- c("Minimum_temperature",
                             "Maximum_temperature",
                             "9am_Temperature",
                             "3pm_Temperature",
                             "Speed_of_maximum_wind_gust_(km/h)")

for (item in temperature_wind_vector) {
  print_highlights(item)
}

# 2: Average of "Minimum_temperature" per month and year
# by month
by_year_month <- group_by(main_df, Year, Month)
summarise(by_year_month, average = mean(Minimum_temperature))

main_df %>%
  group_by(Year, Month) %>%
  summarise(mean = mean(Minimum_temperature))

#by year
by_year <- group_by(main_df, Year)
summarise(by_year, average = mean(Minimum_temperature))

# 3: Average of "Maximum_temperature" per month and year
summarise(by_year_month, average = mean(Maximum_temperature))
summarise(by_year, average = mean(Maximum_temperature))

# 4: Average of Speed_of_maximum_wind_gust_(km/h) per Direction_of_maximum_wind_gust
new_tibble <- main_df
new_tibble_col_names <- names(new_tibble)
names(new_tibble) <- gsub("[()/%]", "", new_tibble_col_names)

by_direction <- group_by(new_tibble, Direction_of_maximum_wind_gust)
summarise(by_direction, average = mean(Speed_of_maximum_wind_gust_kmh, na.rm = TRUE))

# 5: Which month has the highest rainfall quantity? and which year?
# month
monthly_rainfall <- new_tibble %>%
  group_by(Month, Year) %>%
  summarise(total_rainfall_by_month = sum(Rainfall_mm))

print(paste0("month of a year with most rainfall:"))
head(arrange(monthly_rainfall, desc(total_rainfall_by_month)),1)

# year
yearly_rainfall <- new_tibble %>%
  group_by(Year) %>%
  summarise(total_rainfall_by_year = sum(Rainfall_mm))

print("year with most rainfall:")
head(arrange(yearly_rainfall, desc(total_rainfall_by_year)),1)

# 6: Dry month? year?
# FOR MONTH
if (length(which(monthly_rainfall$total_rainfall_by_month == 0)) == 0) {
  print("There were no months with no rainfall")
} else {
  which(monthly_rainfall$total_rainfall_by_month == 0)
}

print(paste0("month of a month with least rainfall:"))
tail(arrange(monthly_rainfall, desc(total_rainfall_by_month)), 1)
# FOR YEAR
if (length(which(yearly_rainfall$total_rainfall_by_year == 0)) == 0) {
  print("There were no years with no rainfall")
} else {
  which(yearly_rainfall$total_rainfall_by_year == 0)
}
print(paste0("year with least rainfall:"))
tail(arrange(yearly_rainfall, desc(total_rainfall_by_year)), 1)

# 7 humidity
humidity_tibble <- transmute(main_df, Year, Month, average = (`3pm_relative_humidity_(%)`+`9am_relative_humidity_(%)`)/2)
humidity_tibble <- humidity_tibble[which(humidity_tibble$Year == 2019),]
humidity_tibble <- humidity_tibble %>%
  group_by(Year, Month) %>%
  summarise(mean(average))
print(paste0("month with highest humidity in 2019:"))
head(arrange(humidity_tibble, desc(`mean(average)`)),1)
###############
#   PART D    #
###############