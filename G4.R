# Importing the libraries
library(readr)
library(dplyr)
library(ggplot2)

# Set the file path
filepath <- file.path(getwd(), "G4_howell.csv")

# Load the dataset
df <- read_csv(filepath)

# Showing the first 5 values
head(df)

# Showing the last 5 values
tail(df)

# Checking for the shape of the values
dim(df)

# Getting some stats
str(df)
summary(df)

# How many males and females
table(df$sex)

# Checking for Null values
sum(is.na(df))

# First, remove the "kg" part from the "weight" column to make it numeric
df$weight <- sub(" kg", "", df$weight)

# Convert the "weight" column to numeric
df$weight <- as.numeric(df$weight)

# Calculate the mean of the column
mean_value <- mean(df$weight, na.rm = TRUE)

# Impute the missing values with the mean
df$weight[is.na(df$weight)] <- mean_value
df$weight <- round(df$weight, 2)

# Overweight column calculation
df$Overweight <- df$weight / (df$height ** 2) * 10000

# Viz Part

# Pie chart for the Sex column
sex_counts <- table(df$sex)

# Create a pie chart
plt <- ggplot(df, aes(x = factor(1), fill = sex)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, title = "Distribution of Sex") +
  theme_void()

print(plt)

# Age According to the Overweight
most_overweight_person <- df[which.max(df$Overweight), ]

age <- most_overweight_person$age
overweight <- most_overweight_person$Overweight
cat("The most overweight person is", age, "years old with an overweight value of", overweight, ".")

# Sort the DataFrame by "Overweight" and "Age" columns
sorted_df <- df %>% arrange(Overweight, age)

# Get the sorted age and overweight values
ages <- sorted_df$age
overweights <- sorted_df$Overweight

# Plot the scatter plot
plt <- ggplot(sorted_df, aes(x = age, y = Overweight)) +
  geom_point() +
  labs(x = "Age", y = "Overweight", title = "Overweight vs Age") +
  theme_bw()

print(plt)

# Create the bar chart
plt <- ggplot(df, aes(x = weight, y = height)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(x = "Weight", y = "Height", title = "Comparison of Height and Weight") +
  theme_bw()

print(plt)

# Filter the data for males and females
male_data <- df %>% filter(sex == "M")
female_data <- df %>% filter(sex == "F")

# Find the highest height for males and females
max_height_male <- max(male_data$height)
max_height_female <- max(female_data$height)

# Create a bar chart to display the highest heights
plt <- ggplot() +
  geom_bar(data = data.frame(Sex = "M", Height = max_height_male), aes(x = Sex, y = Height), stat = "identity", fill = "blue") +
  geom_bar(data = data.frame(Sex = "F", Height = max_height_female), aes(x = Sex, y = Height), stat = "identity", fill = "pink") +
  geom_text(data = data.frame(Sex = c("M", "F"), Height = c(max_height_male, max_height_female)), aes(x = Sex, y = Height, label = Height), vjust = -0.5, color = "black") +
  labs(x = "Sex", y = "Height", title = "Highest Height for Males and Females") +
  theme_bw()

print(plt)
