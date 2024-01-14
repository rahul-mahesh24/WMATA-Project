#WMATA Analysis - Lines#
#the focus of this part of the project is analyzing the lines of the WMATA and comparing them to one another

#loaded required packages for data exploration
library(tidyverse)
library(readxl)

#loaded in data for WMATA Rail Lines and assigned to lines_data
#I collected data from the official WMATA website and their 2023 Reports

lines_data <- read_excel("Data/lines_data.xlsx")
View(lines_data)

#produced summary of data set on each of the variables being tested

summary(lines_data)

#provide overview of data and the type it is stored in

str(lines_data)

#ensuring that the data is not missing values

colSums(is.na(lines_data))

# Filter data for rail lines with an expected run rate over the median

filtered_lines <- lines_data %>%
  mutate(expected_ran = as.numeric(expected_ran)) %>%  # Made sure expected_ran was a numeric by converting it
  filter(expected_ran > 99.30) %>%
  arrange(desc(expected_ran))

# Print the filtered data for rail lines who had an expected run rate above the median of 99.3%

print(filtered_lines)

#renaming 'Line Color' to become line_color to make it consistent with the other variables

lines_data <- lines_data %>%
  rename(line_color = `Line Color`)

#reprinting the entire table with the fixed variable name

print(lines_data)

#creating a basic bar chart of monthly trips ran by each line without labels

barplot(lines_data$month_trips)

#cleaning color data to fix issues with capitalization and colors (like silver) not existing in the library

line_colors <- c("Red" = "red", "Orange" = "orange", "Yellow" = "darkgoldenrod",
                 "Green" = "green", "Blue" = "blue", "Silver" = "gray")

#I swapped out "yellow" for "darkgoldenrod" to increase readability on the charts where the Yellow Line was represented

#creating a bar chart comparing average monthly ridership between lines

lines_data %>%
  ggplot(aes(x = reorder(line_color, -month_trips, sum), y = month_trips, fill = line_color)) + #reorder in descending values
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = line_colors) + #bars line up with colors of the rail line
  labs(x = "Line Color", y = "Average Monthly Trips", title = "Average Monthly Trips by Line", subtitle = "Source: Washington Metropolitan Area Transit Authority")

#creating a correlation matrix among all variables
# Weak (0 to .3 and 0 to -.3), Medium (.3 to .7 and -.3 to -.7), Significant (.7 to 1 and -.7 to -1)

correlations <- cor(lines_data[c("expected_ran", "on_time", "distance_cov", "headway", "month_trips", "stations_cov", "pct_trips")]) #across all variables
correlations #print correlations in console

#finding the correlations that are Significant (.7 to 1 and -.7 to -1)

# Setting a threshold for significance

threshold <- 0.7

# Identifying correlations that had a score above .7 but were not the same variable

significant_pairs <- abs(correlations) > threshold & correlations != 1
significant_pairs #reprint matrix with TRUE/FALSE on meeting threshold

# Extracting row and column names of significant correlations by finding TRUE

pairs_above_threshold <- which(significant_pairs, arr.ind = TRUE)
rownames(correlations)[pairs_above_threshold[, 1]]
colnames(correlations)[pairs_above_threshold[, 2]]

#Re-running correlations on the significant pairs

cor_stations_month <- correlations["stations_cov", "month_trips"]
cor_stations_pct <- correlations["stations_cov", "pct_trips"]
cor_stations_distance <- correlations["stations_cov", "distance_cov"]
cor_month_pct <- correlations["month_trips", "pct_trips"]
cor_distance_headway <- correlations["distance_cov", "headway"]
cor_pct_headway <- correlations["pct_trips", "headway"]

#printing the most significant correlations 

cor_stations_month #correlation between stations covered and trips per month
cor_stations_pct #correlation between stations covered and and share of total WMATA Rail trips
cor_stations_distance #correlation between stations covered and distance covered
cor_month_pct #correlation between trips per month and share of total WMATA Rail trips (obvious)
cor_pct_headway #correlation between share of total WMATA Rail trips and higher headway times (bad)
cor_distance_headway #correlation between distance covered and higher headway times (bad)

#the first two variables effectively show the same relationship
#most interested in investigating these last two
#also will want to investigate cor_stations_month and cor_stations_distance 

#Putting all the correlation data into a data frame

cor_data <- as.data.frame(correlations)
cor_data <- rownames_to_column(cor_data, var = "Var1") %>%
    pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value")
  
# Plotting a heat map of all variables for better visualization of correlations

ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(-1, 1)) + #positive is red, negative is blue, median is no correlation
  labs(title = "WMATA Correlation Heatmap", x = "", y = "", subtitle = "Source: Washington Metropolitan Area Transit Authority") + #included blank x and y labs to remove unneeded x/y labels
  theme_minimal() + #removed border
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #adjusted height of x-axis text to avoid overlap on heat map

#Creating variables to represent WMATA Rail averages or totals to compare against or use later

wmata_expected_ran_average <- mean(lines_data$expected_ran)
wmata_on_time_average <- mean(lines_data$on_time)
wmata_monthly_trips_average <- mean(lines_data$month_trips)
wmata_distance_cov_average <- mean(lines_data$distance_cov)
wmata_stations_cov_average <- mean(lines_data$stations_cov)
wmata_stations_covered <- 98 #stations overlap so could not use sum
wmata_distance_covered <- 129 #rail lines overlap so could not use sum
wmata_headwayavg <- mean(lines_data$headway)

#verifying variables received accurate data/values

wmata_expected_ran_average
wmata_on_time_average
wmata_monthly_trips_average
wmata_distance_cov_average
wmata_stations_cov_average
wmata_stations_covered
wmata_distance_covered
wmata_headwayavg

###### correlation testing cor_pct_headway ######

cor_testPH <- cor.test(lines_data$pct_trips, lines_data$headway)
cor_testPH
#indicated statistical significance because p-val was under .05

###### correlation testing on cor_distance_headway ######

cor_testDH <- cor.test(lines_data$distance_cov, lines_data$headway)
cor_testDH
#does not indicate statistical significance because p-val is above .05

# Creating a scatter plot of distance covered vs. headway times (just for visualization, given lack of statistical significance)
ggplot(lines_data, aes(x = distance_cov, y = headway)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 0.5, nudge_y = 0.5, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Distance Covered", y = "Headway Times", title = "Scatter Plot of Distance Covered against Headways", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#looking at this graph, the relationship looks very weak

# Creating a scatter plot of pct_trips vs. headway times (does have significance)
ggplot(lines_data, aes(x = pct_trips, y = headway)) +
  geom_point() +
  geom_text(aes(label = line_color), nudge_x = -0.2, nudge_y = 0.5, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Share of Total WMATA Trips", y = "Headway Times", title = "Scatter Plot of Pct_Trips against Headways", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#graph shows an negative correlation between Percent of Total WMATA Trips a line makes up (pct_trips) and headway times

#identify and testing values that were of interest but fell below the original threshold of .7

# Setting a threshold for significance

threshold2 <- 0.5

# Identifying correlations that had a score above .5 and below .7 but were not the same variable

moderate_pairs <- abs(correlations) > threshold2 & abs(correlations) <= threshold & correlations != 1

moderate_pairs #reprint matrix with TRUE/FALSE on meeting threshold

# Extract row and column names of moderate correlations by finding TRUE

pairs_above_threshold2 <- which(moderate_pairs, arr.ind = TRUE)
rownames(correlations)[pairs_above_threshold2[, 1]]
colnames(correlations)[pairs_above_threshold2[, 2]]

#I manually paired them up after this, as it was fairly quick and efficient

#New correlations from the pairs that I just created

cor_dist_montrips <- correlations["distance_cov", "month_trips"] # distance_cov - month_trips
cor_dist_ontime <- correlations["distance_cov", "on_time"] # distance_cov - on_time
cor_exran_montrips <- correlations["expected_ran", "month_trips"] # expected_ran - month_trips
cor_head_distcov <- correlations["headway", "distance_cov"] # headway - distance_cov
cor_head_exran <- correlations["headway", "expected_ran"] # headway - expected_ran
cor_pct_distcov <- correlations["pct_trips", "distance_cov"] # pct_trips - distance_cov
cor_pct_exran <- correlations["pct_trips", "expected_ran"] # pct_trips - expected_ran
cor_pct_stacov <- correlations["pct_trips", "stations_cov"] # pct_trips - stations_cov
cor_stacov_montrips <- correlations["stations_cov", "month_trips"] # stations_cov - month_trips
cor_stacov_ontime <- correlations["stations_cov", "on_time"] # stations_cov - on_time

#Printing Correlations to Verify if between .5 and .7 or -.5 and -.7

cor_dist_montrips #1
cor_dist_ontime #2
cor_exran_montrips #3
cor_head_distcov #4
cor_head_exran #5
cor_pct_distcov #6
cor_pct_exran #7
cor_pct_stacov #8
cor_stacov_montrips #9
cor_stacov_ontime #10

#correlation testing to check for statistical significance (p-value below .05)

cor_test1 <- cor.test(lines_data$distance_cov, lines_data$month_trips) #non-significant
cor_test1
cor_test2 <- cor.test(lines_data$distance_cov, lines_data$on_time) #non-significant
cor_test2
cor_test3 <- cor.test(lines_data$expected_ran, lines_data$month_trips) #non-significant
cor_test3
cor_test4 <- cor.test(lines_data$headway, lines_data$distance_cov) #non-significant
cor_test4
cor_test5 <- cor.test(lines_data$headway, lines_data$expected_ran) #non-significant
cor_test5
cor_test6 <- cor.test(lines_data$pct_trips, lines_data$distance_cov) #non-significant
cor_test6
cor_test7 <- cor.test(lines_data$pct_trips, lines_data$expected_ran) #non-significant
cor_test7
cor_test8 <- cor.test(lines_data$pct_trips, lines_data$stations_cov) #non-significant
cor_test8
cor_test9 <- cor.test(lines_data$stations_cov, lines_data$month_trips) #non-significant
cor_test9
cor_test10 <- cor.test(lines_data$stations_cov, lines_data$on_time) #non-significant
cor_test10
#None of the above pairings yielded a p-value of less than .05, meaning the null-hypothesis could not be rejected

#Returning to test two earlier variables that had a correlation value of more than .7

cor_stations_month #correlation between stations covered and trips per month (A)
cor_stations_distance #correlation between stations covered and distance covered (B)

#correlation testing

cor_testA <- cor.test(lines_data$stations_cov, lines_data$month_trips) 
cor_testB <- cor.test(lines_data$stations_cov, lines_data$distance_cov)

#printing correlations

cor_testA #failed (p-val more than .05)
cor_testB #passed (p-val less than .05)

#I will plot both of these relationships. Even though cor_stations_month failed the test, I am still interested in seeing the relationship graphically

# Creating a scatter plot (Stations Covered vs Monthly Trips)

ggplot(lines_data, aes(x = stations_cov, y = month_trips)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 1.1, nudge_y = 0.5, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Stations Covered", y = "Monthly Trips", title = "Scatter Plot of Stations Covered against Monthly Trips", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#looking at this graph, the lack of a relationship is clear, deviation from trend line provide good insights into over/under performers

# Creating a scatter plot (Stations Covered, Distance Covered)

ggplot(lines_data, aes(x = stations_cov, y = distance_cov)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 1, nudge_y = 0.5, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Stations Covered", y = "Distance Covered", title = "Scatter Plot of Stations Covered against Distance Covered", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#relationship between these variables is visibly clear

#testing relationships variables of interest (even of non-statistical significance)

#distance_cov, on_time

ggplot(lines_data, aes(x = distance_cov, y = on_time)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 1.4, nudge_y = 0, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Distance Covered", y = "On Time Percentage", title = "Scatter Plot of Distance Covered Against On Time Percentage", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#looking at this graph, the lack of a relationship is mostly clear but the overall shape is of interest. Good for showing outliers

#graphing expected_ran/headway and expected_ran/pct_trips (they didn't display statistical significance but want to model for visualization)

ggplot(lines_data, aes(x = expected_ran, y = headway)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 0, nudge_y = 0.5, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Percent of Expected Trains that Ran", y = "Headway Times", title = "Scatter Plot of Expected Trains that Ran and Headway Times", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#this graph shows no correlation

ggplot(lines_data, aes(x = distance_cov, y = on_time)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = 1, nudge_y = .1, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Distance Covered", y = "On Time Percentage", title = "Scatter Plot of Distance Covered Against On Time Percentage", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#no significant correlation but we can see which one's are over or under performers based on distance

ggplot(lines_data, aes(x = expected_ran, y = on_time)) +
  geom_point() +  
  geom_text(aes(label = line_color), nudge_x = .2, nudge_y = 0, check_overlap = TRUE, size = 3, color = line_colors) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding a linear regression line
  labs(x = "Expected Trips Ran", y = "On Time Percentage", title = "Scatter Plot of Expected Trips Ran Against On Time Percentage", subtitle = "Source: Washington Metropolitan Area Transit Authority")
#this graph shows no correlation

#comparing values against previously calculated means

# Bar plot comparing each line to the mean monthly trips
ggplot(lines_data, aes(x = reorder(line_color, -month_trips, sum), y = month_trips, fill = line_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = wmata_monthly_trips_average, linetype = "dashed", color = "black", size = 1) + #runs across to show the mean
  labs(x = "Line Color", y = "Average Monthly Trips", title = "Average Monthly Trips by Line Compared to Mean",
       subtitle = "Source: Washington Metropolitan Area Transit Authority") +
  scale_fill_manual(values = line_colors) +
  theme_minimal()

# Bar plot comparing each line to the mean Expected Ran Percentage
ggplot(lines_data, aes(x = reorder(line_color, -expected_ran, sum), y = expected_ran, fill = line_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = wmata_expected_ran_average, linetype = "dashed", color = "black", size = 1) + #runs across to show the mean
  labs(x = "Line Color", y = "Expected Run Percentage", title = "Expected Run Percentage by Line Compared to Mean",
       subtitle = "Source: Washington Metropolitan Area Transit Authority") +
  scale_fill_manual(values = line_colors) +
  theme_minimal() + 
  coord_cartesian(ylim = c(90, 100)) #makes it easier to see the difference

# Bar plot comparing each line to the mean On Time Percentage
ggplot(lines_data, aes(x = reorder(line_color, -on_time, sum), y = on_time, fill = line_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = wmata_on_time_average, linetype = "dashed", color = "black", size = 1) + #runs across to show the mean
  labs(x = "Line Color", y = "On-Time Percentage", title = "On-Time Percentage by Line Compared to Mean",
       subtitle = "Source: Washington Metropolitan Area Transit Authority") +
  scale_fill_manual(values = line_colors) +
  theme_minimal() +
  coord_cartesian(ylim = c(80, 95)) #makes it easier to see the difference

# Bar plot comparing each line to the mean Headway
ggplot(lines_data, aes(x = reorder(line_color, -headway, sum), y = headway, fill = line_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = wmata_headwayavg, linetype = "dashed", color = "black", size = 1) + #runs across to show the mean
  labs(x = "Line Color", y = "Average Headway", title = "Average Headway by Line Compared to Mean",
       subtitle = "Source: Washington Metropolitan Area Transit Authority") +
  scale_fill_manual(values = line_colors) +
  theme_minimal() +
  coord_cartesian(ylim = c(0,20)) #makes it easier to see the difference

# Created a pie chart for pct_trips with labels
ggplot(lines_data, aes(x = "", y = pct_trips, fill = line_color)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert the bar plot to a polar coordinate system
  labs(title = "Percentage of Total WMATA Trips by Line",
       subtitle = "Source: Washington Metropolitan Area Transit Authority",
       fill = "Line Color") +
  scale_fill_manual(values = line_colors) + #colors sections
  theme_minimal() +
  theme(axis.text = element_blank(),  # Removed axis text/labels for better presentation
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  geom_text(aes(label = paste0(pct_trips, "%")), 
            position = position_stack(vjust = 0.5),
            color = ifelse(lines_data$line_color == "Blue", "white", "black")) #black text on blue wasn't readable so I made it white

#Simple bar chart to show how many stations each line covers, a metric to show accessibility and reach
ggplot(lines_data, aes(x = reorder(line_color, -stations_cov, sum), y = stations_cov, fill = line_color)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Line Color", y = "Stations Covered", title = "Stations Covered by Line",
       subtitle = "Source: Washington Metropolitan Area Transit Authority") +
  scale_fill_manual(values = line_colors) +
  theme_minimal()

