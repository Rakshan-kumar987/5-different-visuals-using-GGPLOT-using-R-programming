#:  Importing the data using url as csv file and by using chain operations cleaning the unnecessary data.
url <- "https://github.com/SavioSal/datasets/raw/master/Bank%20Churn_Modelling.csv"
data_1 <- read.csv(url)
data_1
library(dplyr)
library(ggplot2)
##Removing rownumber and customer id columns and then creating converting into factor data the columns for gender, is_active_member, has_credit_card, exited and tenure 
data_2<-data_1 %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>% #remove unwanted column 
  mutate(Geography = as.factor(Geography),
         Gender = as.factor(Gender),
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember),
         Exited = as.factor(Exited),
         Tenure = as.factor(Tenure),
         NumOfProducts = as.factor(NumOfProducts))

#1 Plotting the churn numbers with respect to age
ggplot(data_2, aes(Exited, fill = Exited)) +
  geom_bar() +
  theme(legend.position = 'none')
#2 . The number of active members country-wise
ggplot(data_2, aes(Geography, fill = IsActiveMember)) +
  geom_bar() +
  theme(legend.position = 'none')
#3 . Box plot of the customers to identify how many have exited based on age
library(scales)
ggplot(data_2, aes(x = Age, fill = Exited)) +
  geom_boxplot(binwidth = 5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = comma)
#4 . Plotting the churn ratio of the active members
library(tidyr)
library(tidyselect)
library(tidyverse)
data_2 %>%
  dplyr::select(-Exited) %>% 
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')
# Plotting tenure versus churn ratio
library(ggcorrplot)
data_3 <- names(which(sapply(data_2, is.numeric)))
corr <- cor(data_2[,data_3], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE)



##2 qustion
##. What is the average credit score of females and males in France?
data_2 %>% select(CreditScore, Gender, Geography) %>% filter(Geography == "France") %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(Gender_Average = mean(CreditScore))


#### What is the average credit score of people in the age brackets 20-30,31-40,41-50?
data_2 %>% select(CreditScore, Age) %>% mutate(agegroup = case_when(Age >= 41  & Age <= 50 ~ '3', Age >= 31  & Age <= 40 ~ '2', Age >= 20  & Age <= 30 ~ '1')) %>%
  filter(agegroup == "1" | agegroup == '2' | agegroup == '3') %>%
  dplyr::group_by(agegroup) %>%
  dplyr::summarise(Age_Average = mean(CreditScore))


#### What is the correlation between credit score and estimated salary?  
data_2 %>% select(CreditScore, EstimatedSalary) %>% cor()
####D. Develop a statistical model to explain and establish a mathematical relationship between credit score (dependent) and gender, age, estimate salary.
# Create the relationship model.
model <- lm(CreditScore ~Gender+Age+EstimatedSalary, data = data_2)

# Show the model.
print(model)
summary(model)

