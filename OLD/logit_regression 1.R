library(tidyverse)
library(readxl)
library(skimr)
library(caret)
library(MASS)

data <- read_excel(file.path('data', 'Houses for rent in madrid_assignment 2020.xlsx'), 1)
skim(data)

# https://honingds.com/blog/confusion-matrix-in-r/
# https://www.datacamp.com/community/tutorials/logistic-regression-R
# https://stats.stackexchange.com/questions/169391/beta-distribution-glm-with-categorical-independents-and-proportional-response

data <- read_excel(file.path('data', 'Risk.xlsx'), 1)
model <- glm(risk ~ howpaid + loans + marit1 + marit2 + numcards + numkids + storecar, data = data, family = binomial(link = "logit"))

# https://stats.stackexchange.com/questions/304833/how-to-calculate-odds-ratio-and-95-confidence-interval-for-logistic-regression
# exp(cbind("Odds ratio" = coef(mod, complete = TRUE), confint.default(mod, level = 0.95)))
# summary(mod)$coefficients
# confint.default(mod, level = 0.95)

logit_table <- function(x, level = 0.95) {
  table <- cbind(
      summary(x)$coefficients,
      exp(coefficients(x)),
      exp(confint.default(x, level = level)))
  # colnames(table)[1] <- "Variable"
  colnames(table)[5] <- "Exp(Beta)"
  return(table)
}

logit_table(model, 0.9)

pdata <- predict(model, newdata = data, type = "response")
caret::confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(data$risk))

table(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(data$risk)) * matrix(c(1,0,1,0), ncol = 2, nrow = 2)
table(data = as.factor(as.numeric(pdata>0.5)), reference = as.factor(data$risk)) * table(c(1,0), c(1,0))

## Using Caret ----
# https://topepo.github.io/caret/feature-selection-overview.html#external-validation
# https://topepo.github.io/caret/available-models.html

train.control <- trainControl(method = "cv", number = 10)

data$risk <- factor(data$risk)
step.model <- train(risk ~ .,
                    data = data,
                    method = "glmStepAIC",
                    # tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

## Using Caret ----

data <- read_excel(file.path('data', 'Risk.xlsx'), 1)
data$risk <- factor(data$risk, levels = c('1', '0'), labels = c('Yes', 'No'))
data$id <- NULL

trControl <- trainControl(method = "repeatedcv",
                          repeats = 3,
                          classProbs = TRUE,
                          number = 10, 
                          savePredictions = TRUE,
                          summaryFunction = twoClassSummary)

caret_model <- train(risk ~ ., 
                     data=data, 
                     method="glmStepAIC", 
                     family = "binomial",
                     # family = "binomial(link = 'logit')",
                     # direction ="backward",
                     trControl=trControl)

summary(caret_model)

## Normal way using  ----

# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

# library(MASS)
# # Fit the model
# model <- glm(risk ~ howpaid + loans + marit1 + marit2 + numcards + numkids + storecar, data = data, family = binomial(link = "logit")) %>%
#   stepAIC(trace = 5)
# # Summarize the final selected model
# summary(model)
# # Make predictions
# probabilities <- model %>% predict(test.data, type = "response")
# predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# # Model accuracy
# mean(predicted.classes==test.data$diabetes)


# mutate(
#   marit1 = ifelse(marit1 == 1, 'Married', 0),
#   marit2 = ifelse(marit2 == 1, 'Divorced', 0),
#   marit3 = ifelse(marit3 == 1, 'Single', 0)
# ) %>% 
#   pivot_longer(all_of(c('marit1', 'marit2', 'marit3')))
# 


# Read data
data <- read_excel(file.path('data', 'Risk.xlsx'), sheet = 'Risk_binomial') %>% 
  mutate(
    gender = factor(gender, levels = c('1', '0'), labels = c('Male', 'Female')),
    mortgage = factor(mortgage, levels = c('1', '0'), labels = c('Yes', 'No')),
    howpaid = factor(howpaid, levels = c('1', '0'), labels = c('Weekly', 'Monthly')),
    risk = factor(risk, levels = c('1', '0'), labels = c('GoodRisk', 'BadLoss'))
  ) %>% 
  rename(
    married = marit1, #Married
    divorced = marit2, #Divorced, separated, widowed
    single = marit3 #Single
  )

## Our case ----

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

step.model <- full.model %>% stepAIC(trace = FALSE)
# coef(step.model)
summary(step.model)

logit_table(step.model, 0.9)
