library(tidyverse)
library(readxl)
library(skimr)
library(caret)
library(MASS)
library(mltest)

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
    Type = factor(ifelse(grepl('^Piso\\sen', Address), 'Piso',
                  ifelse(grepl('^Ático\\sen', Address), 'Ático',
                  ifelse(grepl('^Dúplex\\sen', Address), 'Dúplex',
                  ifelse(grepl('^Estudio\\sen', Address), 'Estudio',
                  ifelse(grepl('^Chalet\\spareado\\sen', Address), 'Chalet pareado',
                  ifelse(grepl('^Chalet\\sadosado\\sen', Address), 'Chalet adosado',
                  ifelse(grepl('^Caserón\\sen', Address), 'Caserón',
                  ifelse(grepl('^Casa\\so\\schalet\\sindependiente\\sen', Address), 'Casa o chalet independiente',
                  ifelse(grepl('^Chalet\\sen', Address), 'Chalet', 'Other')))))))))),
    Outer = factor(Outer, levels = c(0, 1), labels = c('No', 'Yes')),
    Elevator = factor(Elevator, levels = c(0, 1), labels = c('No', 'Yes')),
    Penthouse = factor(Penthouse, levels = c(0, 1), labels = c('No', 'Yes')),
    Cottage = factor(Cottage, levels = c(0, 1), labels = c('No', 'Yes')),
    Duplex = factor(Duplex, levels = c(0, 1), labels = c('No', 'Yes')),
    Semidetached = factor(Semidetached, levels = c(0, 1), labels = c('No', 'Yes'))
  ) %>% 
  dplyr::select(-Id, -Address, -Number, -Area) %>% #District
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

## Looking for the optimal value of threshold ----
# https://stats.stackexchange.com/questions/110969/using-the-caret-package-is-it-possible-to-obtain-confusion-matrices-for-specific
# https://community.rstudio.com/t/how-to-choose-best-threshold-value-automatically/12910
library(pROC)
probsTrain <- predict(step.model, newdata = data2)
rocCurve <- roc(response = data2$Rent, predictor = probsTrain, levels = levels(data2$Rent))
plot(rocCurve, print.thres = "best", main = 'Houses for rent in madrid')
# max(sensitivities + specificities)

# Optimal from ROC curve
# pROC::coords(rocCurve, "best", input = "threshold", transpose = FALSE)
ROC_best <- coords(rocCurve, "best", ret = "all", transpose = FALSE)
print(ROC_best)
# All the points of the curve
coords(rocCurve, seq(0,1, by = 0.1), ret = 'all', transpose = FALSE)

## Imbalance ----
# F0.5 calculated as: 1.25*(recall*precision/(0.25*precision+recall))
# https://stats.stackexchange.com/questions/49226/how-to-interpret-f-measure-values
# https://machinelearningmastery.com/tour-of-evaluation-metrics-for-imbalanced-classification/
# https://stats.stackexchange.com/a/207371/80897

optimal_value <- function(x, measure = 'F0.5') {
  pdata <- predict(step.model, newdata = data2, type = "response")
  pdata <- as.numeric(pdata>=x)
  pdata <- factor(pdata, levels = c(0, 1), labels = c('Cheap', 'Expensive'))
  out <- ml_test(pdata, data2$Rent, output.as.table = TRUE)['Cheap', measure]
  return(out)
}

optimal_f1 <- optimize(optimal_value, interval=c(0, 1), maximum=TRUE, measure = 'F1')
optimal_f05 <- optimize(optimal_value, interval=c(0, 1), maximum=TRUE, measure = 'F0.5')
optimal_f2 <- optimize(optimal_value, interval=c(0, 1), maximum=TRUE, measure = 'F2')

## Alternatives for optimal ----

ROC_best$threshold
optimal_f1$maximum
optimal_f05$maximum
optimal_f2$maximum

## Confusion matrix ----

# Define threshold
pdata <- predict(step.model, newdata = data2, type = "response")
pdata <- as.numeric(pdata>=optimal_f05$maximum)
pdata <- factor(pdata, levels = c(0, 1), labels = c('Cheap', 'Expensive'))

# Confusion matrix
caret::confusionMatrix(data = pdata, reference = data2$Rent, positive = 'Cheap')
# Economic table
table(data = pdata, reference = data2$Rent) * matrix(c(1,0,1,0), ncol = 2, nrow = 2)
matrix(c(1,0,1,0), ncol = 2, nrow = 2)
