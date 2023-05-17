# Eric Voisine

setwd() # must set current working directory, alternatively, change file path in line 6 to complete path
library(dplyr)
flight_data <- read.csv("flightdata.csv")

# Question 1 (a)
avg_dist <- mean(flight_data$distance)

above_avg <- flight_data[flight_data$distance > avg_dist, ]

above_avg_sorted <- arrange(above_avg, desc(distance))
head(above_avg_sorted, 10)

# Question 1 (b)

flight_no_depna <- flight_data[!is.na(flight_data$depdelay), ]
sorted_flight_no_depna <- arrange(flight_no_depna, desc(depdelay))
head(sorted_flight_no_depna, 10)

# Question 1 (c)

flight_data$date <- as.Date(flight_data$date, "%Y-%m-%d")

flight_date_subset <- flight_data[flight_data$date >= "2016-04-06" & flight_data$date <= "2016-04-12", ]
head(flight_date_subset, 10)


# Question 1 (d)

grouped_carries <- group_by(flight_data, carrier)
cancel_summary <- arrange(summarize(grouped_carries, 
                            cancellations=(sum(cancelled, na.rm=FALSE) / n()), .groups="keep"), 
                          desc(cancellations))
cancel_summary


# Question 1 (e)

median_del_val <- median(flight_data$depdelay[flight_data$depdelay > 0], na.rm=TRUE)
flight_data$delay_type <- as.factor(if_else(flight_data$depdelay > median_del_val, "HighDelay",
                                    if_else(flight_data$depdelay > 0, "LowDelay", 
                                    if_else(flight_data$depdelay <=0 | is.na(flight_data$depdelay) == TRUE, "NoDelay", "NA"))))
head(flight_data, 10)
