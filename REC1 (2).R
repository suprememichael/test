#Recitation 1 2019 solutions


rm(list = ls())
library(magrittr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Q2. ##############################################################################
q2_data = c(113,	118,	121,	123,	126	,128,	130,	135,	136,	137,138,	139,	140,	140,	142	,142	,142,	142	,143,	155,157	,   157,	   158,	   159,	   164)
hist(q2_data, breaks = seq(from = 110, to = 165, by = 5), xlab = "time", xlim = c(110, 170), main = "Histogram")
####################################################################################

# Q3. ##############################################################################
q3_data = c(165,	145,	115,	110,	150,	145,	38,	140,	122,	155)
summary(q3_data)
boxplot(q3_data, main = "Box plot of heart rate", ylab = "Heart rate", ylim = c(0, 200))
#####################################################################################

# Q4. Text book problem. Please mention the page, number , but also type the text Rosner (8th edition), page 38: problems 2.23-2.25 (fev.dat attached).###############

# 2.23 For each variable (other than ID), obtain appropriate descriptive statistics (both numeric and graphic). ##################

#0. input data
data = read_xls("FEV.DAT.xls") %>%
  mutate(Sex = recode(Sex, "0" = "girl", "1" = "boy"),
         Smoke = recode(Smoke, "0" = "non_smoker", "1" = "smoker"))
head(data)
data$Sex = as.factor(data$Sex)
data$Smoke = as.factor(data$Smoke)
data$Id = as.factor(data$Id)

#1. descriptive statistics for continuous variables (age, fev, and hgt)

#numeric description:
continuous_data_list = list(data$Age, data$FEV, data$Hgt)
min = sapply(continuous_data_list, min)
max = sapply(continuous_data_list, max)
mean = sapply(continuous_data_list, mean)
median = sapply(continuous_data_list, median)
sd = sapply(continuous_data_list, sd)
IQR = sapply(continuous_data_list, IQR)
continuous_description = rbind(min, max, mean, median, sd, IQR)
colnames(continuous_description) = c("age", "fev", "hgt")
write.csv(continuous_description, "continuous description.csv")


#graphic description:
#histogram
par(mfrow = c(3,1))
hist(data$Age, ylim = c(0, 200), xlim = c(0,20), xlab = "Age (years)", main = "Age")
hist(data$FEV, ylim = c(0, 200), xlim = c(0,6), , xlab = "FEV (liters)", main = "FEV")
hist(data$Hgt, ylim = c(0, 200), xlab = "Height (inches)", main = "Height")
#boxplot
par(mfrow = c(1,3))
boxplot(data$Age, ylab = "Age (years)", main = "Age")
boxplot(data$FEV, ylab = "FEV (liters)", main = "FEV")
boxplot(data$Hgt, ylab = "Height (inches)", main = "Height")

#2. descriptive statistics for categorical variables (sex and smoke)

#numeric description:
# Sex count
Sex_frequency = table(data$Sex)
# Sex proportion
Sex_proportion = prop.table(table(data$Sex))

Sex_summary = rbind(Sex_frequency, Sex_proportion)
write.csv(Sex_summary, "sex summary.csv")

# Smoke count
Smoke_frequency = table(data$Smoke)
# Smoke proportion
Smoke_proportion = prop.table(table(data$Smoke))

Smoke_summary = rbind(Smoke_frequency, Smoke_proportion)
write.csv(Smoke_summary, "smoke summary.csv")

# two-way table count
xtabs(~Sex + Smoke, data)
# two-way table proportion
prop.table(xtabs(~Sex + Smoke, data))

#graphic description:
par(mfrow = c(1,2))
barplot(table(data$Sex), main = "Sex", ylab = "Frequency", ylim = c(0, 600))
barplot(table(data$Smoke), main = "Smoke", ylab = "Frequency", ylim = c(0, 600))


# 2.24 Use both numeric and graphic measures to assess the relationship of FEV to age, height, and smoking status. (Do this separately for boys and girls.). ################

# 1. For girls
girl = subset(data, Sex == "girl")

# Scatter plots showing the relationship of FEV to age and height
par(mfrow = c(1,2))
plot(girl$Age, girl$FEV, xlab = "Age (years)", ylab = "FEV (liters)", ylim = c(0, 6), main = "Relationship of FEV to age for girls")
plot(girl$Hgt, girl$FEV, xlab = "Height (inches)", ylab = "FEV (liters)", ylim = c(0, 6), main = "Relationship of FEV to height for girls")

# Boxplots and numeric statistics showing the relationship of FEV to smoking status
#boxplot
par(mfrow = c(1,1))
boxplot(FEV ~ Smoke, girl, ylab = "FEV (liters)", xlab = "Smoking status", main = "FEV of different smoking status for girls", ylim = c(0, 6))
#numeric
girl_smoker = summary(subset(girl, Smoke == "smoker")$FEV)
girl_nonsmoker = summary(subset(girl, Smoke == "non_smoker")$FEV)
write.csv(rbind(girl_smoker, girl_nonsmoker), "girl fev x smoke summary.csv")

# 2. For boys
boy = subset(data, Sex == "boy")

# Scatter plots showing the relationship of FEV to age and height
par(mfrow = c(1,2))
plot(boy$Age, boy$FEV, xlab = "Age (years)", ylab = "FEV (liters)", ylim = c(0, 6), main = "Relationship of FEV to age for boys")
plot(boy$Hgt, boy$FEV, xlab = "Height (inches)", ylab = "FEV (liters)", ylim = c(0, 6), main = "Relationship of FEV to height for boys")

# Boxplots and numerical statistics showing the relationship of FEV to smoking status
#boxplot
par(mfrow = c(1,1))
boxplot(FEV ~ Smoke, boy, ylab = "FEV (liters)", xlab = "Smoking status", main = "FEV of different smoking status for boys", ylim = c(0, 6))
#numerical statistics
boy_smoker = summary(subset(boy, Smoke == "smoker")$FEV)
boy_nonsmoker = summary(subset(boy, Smoke == "non_smoker")$FEV)
write.csv(rbind(boy_smoker, boy_nonsmoker), "boy fev x smoke summary.csv")

# 2.25 Compare the pattern of growth of FEV by age for boys and girls. Are there any similarities? Any differences? #####

# 1. Make a scatterplot to compare the pattern of FEV growth:
data %>% 
  ggplot(aes(x = Age, y = FEV, color = Sex)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Relationship of FEV to age", x = "Age (years)", y = "FEV (liters)") +
  theme_bw()

# 2. Or, we can follow the hint in the textbook to compare the averages of FEV in each age group.
# Add a new variable: age_group
data = data %>%
  mutate(age_group = factor(ifelse((Age >= 3) & (Age <= 4), "3-4", ifelse((Age >= 5) & (Age <= 9), "5-9", ifelse((Age >= 10) & (Age <= 14), "10-14", ifelse((Age >= 15) & (Age <= 19), "15-19", "other" )))), c("3-4", "5-9", "10-14", "15-19")))
#numerical statistics of each age group
summary_by_agegroup = data %>%
  group_by(Sex, age_group) %>%
  summarise(mean = mean(FEV), sd = sd(FEV)) %>%
  gather(variable, value, mean:sd) %>%
  unite(temp, Sex, variable) %>%
  spread(temp, value)
write.csv(summary_by_agegroup, "summary by age group.csv")
#box plot  
data %>%
  ggplot(aes(x = age_group, y = FEV, color = Sex)) +
  geom_boxplot() +
  labs(title = "Relationship of FEV to age group",  x = "Age (years)", y = "FEV (liters)") +
  theme_bw()



















