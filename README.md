# R-Auto-Market-Analytics
Statistical analysis of automobile features influencing price for strategic decision-making.

A comprehensive business analytics project in R, performing statistical analysis on automobile data to support strategic decision-making for a Ministry of Transport.

## Project Overview

This project was conducted under the scenario of being a senior business analyst for the Sri Lankan Ministry of Transport. The goal is to analyze a dataset of automobiles (`auto_info.csv`) to understand how various features (engine size, horsepower, weight, age, and type) influence vehicle price. The insights are intended to inform strategic policies related to vehicle selection suitable for the country's road network and geography.

## Analysis Includes

*   **Descriptive Statistics & Central Tendency:** Mean, median, and mode for key numerical features (Price, Engine Size, Horsepower, Curb Weight).
*   **Data Visualization:** Bell curves (histograms with density plots) to visualize the distribution of these features.
*   **Hypothesis Testing (ANOVA):** Determined if vehicle price fluctuates significantly based on vehicle type (Sedan, SUV, Truck, etc.), including post-hoc Tukey HSD tests.
*   **Correlation & Regression Analysis:** Investigated the statistical significance of relationships between vehicle price and features like engine size, horsepower, curb weight, and age using Spearman correlation and multiple linear regression.

## Key Findings

*   Price distribution varies significantly across different vehicle types (e.g., Trucks vs. Sedans).
*   Strong positive correlations were found between price and features like engine size and horsepower.
*   A negative correlation was identified between the age of a vehicle and its price.
*   The regression model helps estimate price based on a combination of these features.

## Installation & Requirements

To run this analysis, you need R and the following packages installed. You can install them using the following command in R:

```r
install.packages(c("ggplot2", "dplyr", "car", "multcomp"))
