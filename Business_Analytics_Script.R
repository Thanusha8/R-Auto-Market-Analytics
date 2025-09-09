
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(car)       # For Levene's test
library(multcomp)  # For post-hoc tests

my_data <- read.csv("C:/Thanusha/ICBT/sem4/business analytics/auto_info.csv")

View(my_data)

head(my_data)

str(my_data)



#Task 03

# central tendency analysis 

get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

# Calculate for Engine Size
mean_engine <- mean(my_data$engine_size, na.rm = TRUE)
median_engine <- median(my_data$engine_size, na.rm = TRUE)
mode_engine <- get_mode(my_data$engine_size)

# Calculate for Horsepower
mean_horsepower <- mean(my_data$horsepower, na.rm = TRUE)
median_horsepower <- median(my_data$horsepower, na.rm = TRUE)
mode_horsepower <- get_mode(my_data$horsepower)

# Calculate for Curb Weight
mean_weight <- mean(my_data$curb_weight, na.rm = TRUE)
median_weight <- median(my_data$curb_weight, na.rm = TRUE)
mode_weight <- get_mode(my_data$curb_weight)

# Calculate for Price
mean_price <- mean(my_data$price, na.rm = TRUE)
median_price <- median(my_data$price, na.rm = TRUE)
mode_price <- get_mode(my_data$price)


cat("Engine Size:\n Mean =", mean_engine, "\n Median =", median_engine, "\n Mode =", mode_engine, "\n\n")
cat("Horsepower:\n Mean =", mean_horsepower, "\n Median =", median_horsepower, "\n Mode =", mode_horsepower, "\n\n")
cat("Curb Weight:\n Mean =", mean_weight, "\n Median =", median_weight, "\n Mode =", mode_weight, "\n\n")
cat("Price:\n Mean =", mean_price, "\n Median =", median_price, "\n Mode =", mode_price, "\n\n")


# Bell curves

# 1. Engine Size
ggplot(my_data, aes(x = engine_size)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, 
                 fill = "lightblue", 
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_engine, color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_engine, color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Engine Size Distribution",
       x = "Engine Size (L)", 
       y = "Density") +
  theme_minimal()





# 2. Horsepower
ggplot(my_data, aes(x = horsepower)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, 
                 fill = "lightgreen", 
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_horsepower, color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_horsepower, color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Horsepower Distribution",
       x = "Horsepower", 
       y = "Density") +
  theme_minimal()


# 3. Curb Weight
ggplot(my_data, aes(x = curb_weight)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, 
                 fill = "lightpink", 
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_weight, color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_weight, color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Curb Weight Distribution",
       x = "Weight (lbs)", 
       y = "Density") +
  theme_minimal()


# 4. Price
ggplot(my_data, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, 
                 fill = "yellow", 
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_price, color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_price, color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Price Distribution",
       x = "Price (USD)", 
       y = "Density") +
  theme_minimal()




# Task 04


# Convert vehicle_type to factor
my_data$vehicle_type <- as.factor(my_data$vehicle_type)

# Descriptive statistics by vehicle type
price_stats <- my_data %>%
  group_by(vehicle_type) %>%
  summarise(
    count = n(),
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price),
    min_price = min(price),
    max_price = max(price)
  )
print(price_stats)

# Check ANOVA assumptions

# 1. Normality within each group (using Shapiro-Wilk test)
normality_check <- by(my_data$price, my_data$vehicle_type, shapiro.test)
print(normality_check)

# 2. Homogeneity of variances (Levene's Test)
leveneTest(price ~ vehicle_type, data = my_data)

# Visualization - Boxplot of prices by vehicle type
ggplot(my_data, aes(x = vehicle_type, y = price, fill = vehicle_type)) +
  geom_boxplot() +
  labs(title = "Vehicle Price Distribution by Type",
       x = "Vehicle Type",
       y = "Price (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform One-Way ANOVA
anova_result <- aov(price ~ vehicle_type, data = my_data)
summary(anova_result)

# If ANOVA is significant, perform post-hoc Tukey HSD test
if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  # Visualize pairwise comparisons
  plot(tukey_result, las = 1)
}

# Non-parametric alternative (Kruskal-Wallis test)
kruskal.test(price ~ vehicle_type, data = my_data)




# Task 05: Statistical Analysis of Vehicle Price Relationships


str(my_data)

# Summary statistics
summary(my_data[, c("price", "engine_size", "horsepower", "curb_weight", "age")])


#Normality Test (Shapiro-Wilk)

shapiro.test(my_data$price)             # Price distribution
shapiro.test(my_data$engine_size)       # Engine size
shapiro.test(my_data$horsepower)        # Horsepower
shapiro.test(my_data$curb_weight)       # Curb weight
shapiro.test(my_data$age)               # Age



# Correlation Analysis
cor.test(my_data$price, my_data$engine_size, method = "spearman")
cor.test(my_data$price, my_data$horsepower, method = "spearman")
cor.test(my_data$price, my_data$curb_weight, method = "spearman")
cor.test(my_data$price, my_data$age, method = "spearman")


#Linear Regression
model <- lm(price ~ engine_size + horsepower + curb_weight + age, data = my_data)
summary(model)



#Graphical Analysis

# Engine Size vs. Price
ggplot(my_data, aes(x = engine_size, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Engine Size vs. Price", x = "Engine Size (L)", y = "Price ($)")

# Horsepower vs. Price
ggplot(my_data, aes(x = horsepower, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Horsepower vs. Price", x = "Horsepower (hp)", y = "Price ($)")

# Curb Weight vs. Price
ggplot(my_data, aes(x = curb_weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Curb Weight vs. Price", x = "Weight (lbs)", y = "Price ($)")

# Age vs. Price
ggplot(my_data, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Age vs. Price", x = "Age (years)", y = "Price ($)")
