#' ---
#' title: Assignment 1
#' author: Fazal Mahmud Niloy (u3228358)
#' output: pdf_document
#' ---

# Start-------------------------------------------
# Unit & ID:    Introduction to Data Science 11516
# Name & ID:    Fazal Mahmud Niloy 3228358
# Description:  Intro to Data Science Assignment

setwd("~/IDS_assignment_1")
tinytex::install_tinytex()
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
print(names(main_df))

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

# median_value <- median(main_df[["9am_wind_speed_(km/h)"]], na.rm = TRUE)
# na_indices <- which(is.na(main_df[["9am_wind_speed_(km/h)"]]))
# main_df[na_indices, "9am_wind_speed_(km/h)"] <- median_value
# # checking
# which(is.na(main_df[["9am_wind_speed_(km/h)"]]))
###########################################################################################
# set_na_to_median <- function(data_frame, column_name) {
#   median_value <- median(data_frame[[column_name]], na.rm = TRUE)
#   na_indices <- which(is.na(data_frame[column_name]))
#   data_frame[na_indices, column_name] <- median_value
# }
#
# col_type_vector <- sapply(main_df, typeof)
# for (item in names(col_type_vector)[3:length(names(col_type_vector))]) {
#   if (col_type_vector[item] == "integer" | col_type_vector[item] == "double"| col_type_vector[item] == "double") {
#     set_na_to_median(main_df, item)
#   }
# }
###########################################################################################

main_df[is.na(main_df$`Minimum_temperature`),]$`Minimum_temperature` <- median(main_df$`Minimum_temperature`, na.rm = TRUE)
main_df[is.na(main_df$`Maximum_temperature`),]$`Maximum_temperature` <- median(main_df$`Maximum_temperature`, na.rm = TRUE)
main_df[is.na(main_df$`Rainfall_(mm)`),]$`Rainfall_(mm)` <- median(main_df$`Rainfall_(mm)`, na.rm = TRUE)
main_df[is.na(main_df$`Speed_of_maximum_wind_gust_(km/h)`),]$`Speed_of_maximum_wind_gust_(km/h)` <- median(main_df$`Speed_of_maximum_wind_gust_(km/h)`, na.rm = TRUE)

main_df[is.na(main_df$`9am_Temperature`),]$`9am_Temperature` <- median(main_df$`9am_Temperature`, na.rm = TRUE)
main_df[is.na(main_df$`9am_relative_humidity_(%)`),]$`9am_relative_humidity_(%)` <- median(main_df$`9am_relative_humidity_(%)`, na.rm = TRUE)
main_df[is.na(main_df$`9am_cloud_amount_(oktas)`),]$`9am_cloud_amount_(oktas)` <- median(main_df$`9am_cloud_amount_(oktas)`, na.rm = TRUE)
main_df[is.na(main_df$`9am_wind_speed_(km/h)`),]$`9am_wind_speed_(km/h)` <- median(main_df$`9am_wind_speed_(km/h)`, na.rm = TRUE)
main_df[is.na(main_df$`9am_MSL_pressure_(hPa)`),]$`9am_MSL_pressure_(hPa)` <- median(main_df$`9am_MSL_pressure_(hPa)`, na.rm = TRUE)

main_df[is.na(main_df$`3pm_Temperature`),]$`3pm_Temperature` <- median(main_df$`3pm_Temperature`, na.rm = TRUE)
main_df[is.na(main_df$`3pm_relative_humidity_(%)`),]$`3pm_relative_humidity_(%)` <- median(main_df$`3pm_relative_humidity_(%)`, na.rm = TRUE)
main_df[is.na(main_df$`3pm_cloud_amount_(oktas)`),]$`3pm_cloud_amount_(oktas)` <- median(main_df$`3pm_cloud_amount_(oktas)`, na.rm = TRUE)
main_df[is.na(main_df$`3pm_wind_speed_(km/h)`),]$`3pm_wind_speed_(km/h)` <- median(main_df$`3pm_wind_speed_(km/h)`, na.rm = TRUE)
main_df[is.na(main_df$`3pm_MSL_pressure_(hPa)`),]$`3pm_MSL_pressure_(hPa)` <- median(main_df$`3pm_MSL_pressure_(hPa)`, na.rm = TRUE)


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
summarise(by_year_month, average_minimum_temperature_monthly = mean(Minimum_temperature))

main_df %>%
  group_by(Year, Month) %>%
  summarise(average_minimum_temperature_monthly = mean(Minimum_temperature))

#by year
by_year <- group_by(main_df, Year)
summarise(by_year, average_minimum_temperature_yearly = mean(Minimum_temperature))

# 3: Average of "Maximum_temperature" per month and year
summarise(by_year_month, average_maximum_temperature_by_month = mean(Maximum_temperature))
summarise(by_year, average_maximum_temperature_by_year = mean(Maximum_temperature))

# 4: Average of Speed_of_maximum_wind_gust_(km/h) per Direction_of_maximum_wind_gust
by_direction <- group_by(main_df, `Direction_of_maximum_wind_gust`)
summarise(by_direction, average_speed_of_max_wind_gust = mean(`Speed_of_maximum_wind_gust_(km/h)`, na.rm = TRUE))

# 5: Which month has the highest rainfall quantity? and which year?
# month
monthly_rainfall <- main_df %>%
  group_by(Month, Year) %>%
  summarise(total_rainfall_by_month = sum(`Rainfall_(mm)`))

print(paste0("month of a year with most rainfall:"))
head(arrange(monthly_rainfall, desc(total_rainfall_by_month)),1)

# year
yearly_rainfall <- main_df %>%
  group_by(Year) %>%
  summarise(total_rainfall_by_year = sum(`Rainfall_(mm)`))

print("year with most rainfall:")
head(arrange(yearly_rainfall, desc(total_rainfall_by_year)),1)

# 6: Dry month? year?
# FOR MONTH
if (length(which(monthly_rainfall$total_rainfall_by_month == 0)) == 0) {
  print("There were no months with no rainfall")
} else {
  which(monthly_rainfall$total_rainfall_by_month == 0)
}

print(paste0("month of a year with least rainfall:"))
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

# 8 temperature_wind_humidity

temperature_wind_humidity_tibble <- transmute(main_df[which(main_df$Year == 2019),],
                                           Year,
                                           Month,
                                           average_temperature = (`Minimum_temperature` + `Maximum_temperature`)/2,
                                           average_humidity = (`3pm_relative_humidity_(%)` + `9am_relative_humidity_(%)`)/2,
                                           average_windspeed = (`Speed_of_maximum_wind_gust_(km/h)` + `9am_wind_speed_(km/h)` + `3pm_wind_speed_(km/h)`)/3)
temperature_wind_humidity_tibble

# temperature avg 2019 all months
temperature_avg_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(temperature_avg_2019 = mean(average_temperature))
head(arrange(temperature_avg_tibble_2019, desc(temperature_avg_2019)), 1)
tail(arrange(temperature_avg_tibble_2019, desc(temperature_avg_2019)), 1)
temperature_avg_tibble_2019

# temperature avg 2019 QUARTERLY
temperature_avg_2019_quarterly <-
  data.frame(quarter = 1: 4,
             average_temperature = c(
                mean(as_vector(temperature_avg_tibble_2019[1:3, 3])),
                mean(as_vector(temperature_avg_tibble_2019[4:6, 3])),
                mean(as_vector(temperature_avg_tibble_2019[7:9, 3])),
                mean(as_vector(temperature_avg_tibble_2019[10:12, 3]))
             )
  )
temperature_avg_2019_quarterly <- as_tibble(temperature_avg_2019_quarterly)
temperature_avg_2019_quarterly

# temperature min all months 2019
temperature_min_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(temperature_min_2019 = min(average_temperature))
head(arrange(temperature_min_tibble_2019, desc(temperature_min_2019)), 1)
tail(arrange(temperature_min_tibble_2019, desc(temperature_min_2019)), 1)

# temperature min 2019 QUARTERLY
temperature_min_2019_quarterly <-
  data.frame(quarter = 1: 4,
             min_temperature = c(
                min(as_vector(temperature_min_tibble_2019[1:3, 3])),
                min(as_vector(temperature_min_tibble_2019[4:6, 3])),
                min(as_vector(temperature_min_tibble_2019[7:9, 3])),
                min(as_vector(temperature_min_tibble_2019[10:12, 3]))
             )
  )
temperature_min_2019_quarterly <- as_tibble(temperature_min_2019_quarterly)
temperature_min_2019_quarterly

# temperature max all months
temperature_max_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(temperature_max_2019 = max(average_temperature))
temperature_max_tibble_2019
head(arrange(temperature_max_tibble_2019, desc(temperature_max_2019)), 1)
tail(arrange(temperature_max_tibble_2019, desc(temperature_max_2019)), 1)

# temperature max 2019 QUARTERLY
temperature_max_2019_quarterly <-
  data.frame(quarter = 1: 4,
             max_temperature = c(
                max(as_vector(temperature_max_tibble_2019[1:3, 3])),
                max(as_vector(temperature_max_tibble_2019[4:6, 3])),
                max(as_vector(temperature_max_tibble_2019[7:9, 3])),
                max(as_vector(temperature_max_tibble_2019[10:12, 3]))
             )
  )
temperature_max_2019_quarterly <- as_tibble(temperature_max_2019_quarterly)
temperature_max_2019_quarterly

# humidity average 2019
humidity_avg_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(humidity_avg_2019 = mean(average_humidity))
humidity_avg_tibble_2019
head(arrange(humidity_avg_tibble_2019, desc(humidity_avg_2019)), 1)
tail(arrange(humidity_avg_tibble_2019, desc(humidity_avg_2019)), 1)

# Humidity avarage 2019 QUARTERLY
humidity_avg_2019_quarterly <-
  data.frame(quarter = 1: 4,
             average_humidity = c(
                mean(as_vector(humidity_avg_tibble_2019[1:3, 3])),
                mean(as_vector(humidity_avg_tibble_2019[4:6, 3])),
                mean(as_vector(humidity_avg_tibble_2019[7:9, 3])),
                mean(as_vector(humidity_avg_tibble_2019[10:12, 3]))
             )
  )
humidity_avg_2019_quarterly <- as_tibble(humidity_avg_2019_quarterly)
humidity_avg_2019_quarterly

# humidity min 2019
humidity_min_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(humidity_min_2019 = min(average_humidity))
humidity_min_tibble_2019
head(arrange(humidity_min_tibble_2019, desc(humidity_min_2019)), 1)
tail(arrange(humidity_min_tibble_2019, desc(humidity_min_2019)), 1)

# Humidity min 2019 QUARTERLY
humidity_min_2019_quarterly <-
  data.frame(quarter = 1: 4,
             min_humidity = c(
                min(as_vector(humidity_min_tibble_2019[1:3, 3])),
                min(as_vector(humidity_min_tibble_2019[4:6, 3])),
                min(as_vector(humidity_min_tibble_2019[7:9, 3])),
                min(as_vector(humidity_min_tibble_2019[10:12, 3]))
             )
  )
humidity_min_2019_quarterly <- as_tibble(humidity_min_2019_quarterly)
humidity_min_2019_quarterly

# humidity max 2019
humidity_max_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(humidity_max_2019 = max(average_humidity))
humidity_max_tibble_2019
head(arrange(humidity_max_tibble_2019, desc(humidity_max_2019)), 1)
tail(arrange(humidity_max_tibble_2019, desc(humidity_max_2019)), 1)

# Humidity max 2019 QUARTERLY
humidity_max_2019_quarterly <-
  data.frame(quarter = 1: 4,
             max_humidity = c(
                max(as_vector(humidity_max_tibble_2019[1:3, 3])),
                max(as_vector(humidity_max_tibble_2019[4:6, 3])),
                max(as_vector(humidity_max_tibble_2019[7:9, 3])),
                max(as_vector(humidity_max_tibble_2019[10:12, 3]))
             )
  )
humidity_max_2019_quarterly <- as_tibble(humidity_max_2019_quarterly)
humidity_max_2019_quarterly

# windspeed avg 2019
windspeed_avg_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(windspeed_avg_2019 = mean(average_windspeed))
windspeed_avg_tibble_2019
head(arrange(windspeed_avg_tibble_2019, desc(windspeed_avg_2019)), 1)
tail(arrange(windspeed_avg_tibble_2019, desc(windspeed_avg_2019)), 1)

# Windspeed avg 2019 QUARTERLY
windspeed_avg_2019_quarterly <-
  data.frame(quarter = 1: 4,
             average_windspeed = c(
                mean(as_vector(windspeed_avg_tibble_2019[1:3, 3])),
                mean(as_vector(windspeed_avg_tibble_2019[4:6, 3])),
                mean(as_vector(windspeed_avg_tibble_2019[7:9, 3])),
                mean(as_vector(windspeed_avg_tibble_2019[10:12, 3]))
             )
  )
windspeed_avg_2019_quarterly <- as_tibble(windspeed_avg_2019_quarterly)
windspeed_avg_2019_quarterly

# windspeed min 2019
windspeed_min_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(windspeed_min_2019 = min(average_windspeed))
windspeed_min_tibble_2019
head(arrange(windspeed_min_tibble_2019, desc(windspeed_min_2019)), 1)
tail(arrange(windspeed_min_tibble_2019, desc(windspeed_min_2019)), 1)

# Windspeed min 2019 QUARTERLY
windspeed_min_2019_quarterly <-
  data.frame(quarter = 1: 4,
             min_windspeed = c(
                min(as_vector(windspeed_min_tibble_2019[1:3, 3])),
                min(as_vector(windspeed_min_tibble_2019[4:6, 3])),
                min(as_vector(windspeed_min_tibble_2019[7:9, 3])),
                min(as_vector(windspeed_min_tibble_2019[10:12, 3]))
             )
  )
windspeed_min_2019_quarterly <- as_tibble(windspeed_min_2019_quarterly)
windspeed_min_2019_quarterly

# windspeed max 2019
windspeed_max_tibble_2019 <- temperature_wind_humidity_tibble %>%
  group_by(Month, Year) %>%
  summarise(windspeed_max_2019 = max(average_windspeed))
windspeed_max_tibble_2019
head(arrange(windspeed_max_tibble_2019, desc(windspeed_max_2019)), 1)
tail(arrange(windspeed_max_tibble_2019, desc(windspeed_max_2019)), 1)

# Windspeed min 2019 QUARTERLY
windspeed_max_2019_quarterly <-
  data.frame(quarter = 1: 4,
             max_windspeed = c(
                max(as_vector(windspeed_max_tibble_2019[1:3, 3])),
                max(as_vector(windspeed_max_tibble_2019[4:6, 3])),
                max(as_vector(windspeed_max_tibble_2019[7:9, 3])),
                max(as_vector(windspeed_max_tibble_2019[10:12, 3]))
             )
  )
windspeed_max_2019_quarterly <- as_tibble(windspeed_max_2019_quarterly)
windspeed_max_2019_quarterly


# 8 temperature_wind_humidity

# TEMPERATURE average for all months
ggplot(
  data = temperature_avg_tibble_2019,
  aes(
    x = Month,
    y = temperature_avg_2019
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = temperature_avg_2019
    ),
    vjust = 1.6,
    color = "black",
    size = 2.5
  ) +
  ggtitle("average temperature for all months in 2019") +
  theme_minimal()
# TEMPERATURE average for all quarters
ggplot(
  data = temperature_avg_2019_quarterly,
  aes(
    x = quarter,
    y = average_temperature
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = average_temperature
    ),
    vjust = 1.6,
    color = "white",
    size = 2.5
  ) +
  ggtitle("average temperature for all quarters in 2019") +
  theme_minimal()

# HUMIDITY average for all months
ggplot(
  data = humidity_avg_tibble_2019,
  aes(
    x = Month,
    y = humidity_avg_2019
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = humidity_avg_2019
    ),
    vjust = -.5,
    color = "black",
    size = 2.5
  ) +
  ggtitle("average humidity for all months in 2019") +
  theme_minimal()
# HUMIDITY average all quarters
ggplot(
  data = humidity_avg_2019_quarterly,
  aes(x = quarter, y = average_humidity)
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = average_humidity
    ),
    vjust = 1.6,
    color = "white",
    size = 2.5
  ) +
  ggtitle("average humidity for all quarters in 2019") +
  theme_minimal()

# WINDSPEED average for all months
ggplot(
  data = windspeed_avg_tibble_2019,
  aes(
    x = Month,
    y = windspeed_avg_2019
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = windspeed_avg_2019
    ),
    vjust = -.5,
    color = "black",
    size = 2.5
  ) +
  ggtitle("average windspeed for all months in 2019") +
  theme_minimal()

# WINDSPEED min for all months
ggplot(
  data = windspeed_min_tibble_2019,
  aes(
    x = Month,
    y = windspeed_min_2019
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = windspeed_min_2019
    ),
    vjust = -.5,
    color = "black",
    size = 2.5
  ) +
  ggtitle("min windspeed for all months in 2019") +
  theme_minimal()

# WINDSPEED max for all months
ggplot(
  data = windspeed_max_tibble_2019,
  aes(
    x = Month,
    y = windspeed_max_2019
  )
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = windspeed_max_2019
    ),
    vjust = -.5,
    color = "black",
    size = 2.5
  ) +
  ggtitle("max windspeed for all months in 2019") +
  theme_minimal()

# WINDSPEED average all quarters
ggplot(
  data = windspeed_avg_2019_quarterly,
  aes(x = quarter, y = average_windspeed)
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = average_windspeed
    ),
    vjust = 1.6,
    color = "white",
    size = 2.5
  ) +
  ggtitle("average windspeed for all quarters in 2019") +
  theme_minimal()

# WINDSPEED min all quarters
ggplot(
  data = windspeed_min_2019_quarterly,
  aes(x = quarter, y = min_windspeed)
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = min_windspeed
    ),
    vjust = 1.6,
    color = "white",
    size = 2.5
  ) +
  ggtitle("min windspeed for all quarters in 2019") +
  theme_minimal()

# WINDSPEED max all quarters
ggplot(
  data = windspeed_max_2019_quarterly,
  aes(x = quarter, y = max_windspeed)
) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = max_windspeed
    ),
    vjust = 1.6,
    color = "white",
    size = 2.5
  ) +
  ggtitle("max windspeed for all quarters in 2019") +
  theme_minimal()
