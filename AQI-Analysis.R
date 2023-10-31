# Reading the data
df <- read.csv("data/city_hour.csv")

head(df)

summary(df)

# Print datatype of date column before conversion
class(df$Date)


# Converting the Datetime column from character to Date format
df[["Datetime"]] <- as.POSIXct(df[["Datetime"]])

class(df$Date)



# Using DPlyr to group the data by city and calculate the mean, min and max of AQI
install.packages("dplyr")
library(dplyr)

df %>%
  group_by(City) %>%
  summarise(Mean = round(mean( AQI, na.rm = TRUE), 2),
            Min = min( AQI, na.rm = TRUE),
            Max = max( AQI, na.rm = TRUE))



# Keep only Mumbai city data and remove the city column
df <- df[df$City == "Mumbai", ]

df <- df[, -1]


# Print the head of the dataframe
head(df)



# Create a list of pollutants
pollutants <- c("NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene", "AQI")


# Create an empty Dataframe
pollutant_df <- data.frame()


# Loop through the pollutants list
for (Pollutant in pollutants) {
  # Calculate the average, maximum and minimum of the pollutant
  Average <- round(mean(df[[Pollutant]], na.rm = TRUE), 2)
  Max <- max(df[[Pollutant]], na.rm = TRUE)
  Min <- min(df[[Pollutant]], na.rm = TRUE)


  # Create a Dataframe with the pollutant name and the calculated values
  pollutant_df <- rbind(pollutant_df, data.frame(Pollutant, Average, Max, Min))
}

pollutant_df



# Import libraries for plotting
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("reshape")

library(ggplot2)
library(ggthemes)
library(reshape)


# Plotting AQI vs Datetime
ggplot(df, aes(x = Datetime, y = AQI)) +
  geom_smooth(na.rm = T) +
  theme_economist() +
  xlab("") +
  ggtitle("AQI in Mumbai") +
  scale_x_datetime(date_labels = "%b %y") +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Bar Graph for AQI Bucket
positions <- c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor")
colors <- c("green", "orange", "red", "yellow", "purple")

ggplot(subset(df, AQI_Bucket != ""), aes(x = AQI_Bucket)) +
  geom_bar(fill = colors) +
  scale_x_discrete(limits = positions) +
  theme_economist() +
  ggtitle("AQI Bucket") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Histogram for AQI
ggplot(df, aes(x = AQI)) +
  geom_histogram(bins = 100, na.rm = T) +
  theme_economist() +
  ggtitle("AQI Histogram") +
  xlab("AQI") +
  ylab("")


# Making a correlation matrix
pollutants <- c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "O3", "Benzene", "AQI")
corr <- cor(df[, pollutants], use = "complete.obs")

corr


# Using ggplot to plot the correlation matrix
ggplot(melt(corr), aes(X1, X2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  ggtitle("Heatmap") +
  xlab("") +
  ylab("")



# Make a plot of all the pollutants
ggplot(df, aes(x = Datetime)) +
  geom_smooth(aes(y = PM2.5, color = "PM2.5"), na.rm = T) +
  geom_smooth(aes(y = PM10, color = "PM10"), na.rm = T) +
  geom_smooth(aes(y = NO, color = "NO"), na.rm = T) +
  geom_smooth(aes(y = NO2, color = "NO2"), na.rm = T) +
  geom_smooth(aes(y = NH3, color = "NH3"), na.rm = T) +
  geom_smooth(aes(y = O3, color = "O3"), na.rm = T) +
  geom_smooth(aes(y = AQI, color = "AQI"), na.rm = T) +
  theme_economist() +
  ggtitle("Pollutants in Mumbai") +
  xlab("") +
  ylab("") +
  scale_color_manual("", values = c("PM2.5" = "red", "PM10" = "blue", "NO" = "green", "NO2" = "orange", "NH3" = "gold", "O3" = "brown", "AQI" = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
