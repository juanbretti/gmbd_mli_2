library(tidyverse)
library(readxl)
library(skimr)
library(caret)
library(MASS)

# Id	House identification
# District	District
# Address	Address
# Number	Number
# Area	Area
# Rent	Rental price in €
# Bedrooms	Number of bedrooms
# Sq.Mt	Squared meters
# Floor	Floor number
# Outer	Is Outer? (0=No, 1=Yes)
# Elevator	Does it have elevator? (0=No, 1=Yes)
# Penthouse	Is it a penthouse? (0=No, 1=Yes)
# Cottage	Is it a cottage? (0=No, 1=Yes)
# Duplex	Is it a duplex? (0=No, 1=Yes)
# Semidetached	Is it semidetached? (0=No, 1=Yes)
data <- read_excel(path = file.path('data', 'Houses for rent in madrid_assignment 2020.xlsx'), sheet = 'Houses_for_rent_madrid_assignme')
skim(data)

data1 <- data %>% 
  mutate(
    Outer = factor(Outer, levels = c(0, 1), labels = c('No', 'Yes')),
    Elevator = factor(Elevator, levels = c(0, 1), labels = c('No', 'Yes')),
    Penthouse = factor(Penthouse, levels = c(0, 1), labels = c('No', 'Yes')),
    Cottage = factor(Cottage, levels = c(0, 1), labels = c('No', 'Yes')),
    Duplex = factor(Duplex, levels = c(0, 1), labels = c('No', 'Yes')),
    Semidetached = factor(Semidetached, levels = c(0, 1), labels = c('No', 'Yes'))
  ) %>% 
  dplyr::select(-Number, -Address, -Id, -Area, -District) %>%
  # filter_at(vars(Area, Outer, Elevator, Bedrooms, Floor), all_vars(!is.na(.)))
  filter_at(vars(Outer, Elevator, Bedrooms, Floor), all_vars(!is.na(.)))
skim(data1)

data2 <- data1 %>% 
  mutate(
    Rent = factor(ifelse(Rent > 2000, 0, 1), levels = c(0, 1), labels = c('Cheap', 'Expensive'))
  )
skim(data2)

# Fit full model
full.model <- glm(Rent ~ ., data = data2, family = binomial(link = "logit"))
# coef(full.model)
summary(full.model)
# Step model

step.model <- full.model %>% 
  stepAIC(trace = FALSE)
# coef(step.model)
summary(step.model)

## Table ----

logit_table <- function(x, level = 0.95) {
  table <- cbind(
    summary(x)$coefficients,
    exp(coefficients(x)),
    exp(confint.default(x, level = level)))
  # colnames(table)[1] <- "Variable"
  colnames(table)[5] <- "Exp(Beta)"
  return(table)
}

logit_table(step.model)

## Confusion matrix ----

# Define threshold
pdata <- predict(step.model, newdata = data2, type = "response")
pdata <- as.numeric(pdata>0.5)
pdata <- factor(pdata, levels = c(0, 1), labels = c('Cheap', 'Expensive'))

# Confusion matrix
caret::confusionMatrix(data = pdata, reference = data2$Rent)
# Economic table
table(data = pdata, reference = data2$Rent) * matrix(c(1,0,1,0), ncol = 2, nrow = 2)

