library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(tidyverse)

data = read.csv("insurance.csv")

summary(data)

#calculates mean bmi by gender
sex_bmi = aggregate(data[, 3], list(data$sex), mean)
sex_bmi

#calculates smoker vs. non-smoker bmi
smoker_bmi = aggregate(data[, 3], list(data$smoker), mean)
smoker_bmi

#calculates mean bmi by region
region_bmi = aggregate(data[, 3], list(data$region), mean)
region_bmi

#histogram of ages with mean line
ggplot(data, aes(x=age))+
  geom_histogram( bins = 8, color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(age)),
             color="red", linetype="dashed", size=1) +
  labs(title = "Distribution of Ages",
       x = "Age",
       y = "Count")


chi = c("0","1","2","3","4","5")

chi_count = c(574,324,240,157,25,18)

#creates a new dataframe with two columns, chi & chi_count
df = data.frame(chi, chi_count)

#bar chart of the count of children in the dataset
ggplot(data=df, aes(x=chi, y=chi_count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=chi_count), vjust=-0.3, size=3.5)+
  theme_minimal() +
  labs(title = "Count of Children by Customer Household",
       x = "Number of Children in Household",
       y = "Count")

#code to create a correlation matrix from the data

pairs.panels(data[,1:7], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses

#mean charges by age
age_charges = aggregate(data[, 7], list(data$age), mean)
age_charges

#plot a line chart of age_charges
ggplot(data=age_charges, aes(x=Group.1, y=x, group=1)) +
  geom_line(color="red")+
  geom_point() +
  labs(title = "Mean Charges by Age",
       x = "Age (years)",
       y = "Charges ($)")
