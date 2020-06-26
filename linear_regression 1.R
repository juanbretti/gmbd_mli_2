library(tidyverse)
library(readxl)
library(skimr)
library(MASS)

## Read data ----
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
  dplyr::select(-Id, -Address, -Number, -Area, -District) %>%
  # dplyr::select(-Id, -Address, -Number, -Area, -District) %>%
  # filter_at(vars(Area, Outer, Elevator, Bedrooms, Floor), all_vars(!is.na(.)))
  filter_at(vars(Outer, Elevator, Bedrooms, Floor), all_vars(!is.na(.)))
skim(data1)

## Model ----

# Fit full model
# full.model <- glm(Rent ~ ., data = data1)
full.model <- lm(Rent ~ ., data = data1)
summary(full.model)
# Step model
step.model <- full.model %>% 
  stepAIC(trace = FALSE)
# coef(step.model)
summary(step.model)

## Find good opportunities in the market looking for flats that may be under their theoretical estimated price ----
data2 <- data1
data2$prediction <- predict(step.model, data1)
data2 %>% 
  filter(Rent<prediction) %>%
  arrange(desc(prediction/Rent-1))

# Scaled
# Prepare data
data2 <- data1 %>% 
  mutate_if(is.numeric, scale)
step.model_scaled <- lm(Rent ~ ., data = data2)
summary(step.model_scaled)






## Case in the PPT ----
data <- read_excel(path = file.path('data', 'wage.xlsx'), sheet = 1)

# Maintaining units
full.model <- lm(WAGE ~ AFROAMERICAN + AGE + EDUC + EXPER + HOURS + MARRIED + SIBS + BRTHORD, data = data)
summary(full.model)
# Scaled
data2 <- data %>% 
  mutate_if(is.numeric, scale)
full.model <- lm(WAGE ~ AFROAMERICAN + AGE + EDUC + EXPER + HOURS + MARRIED + SIBS + BRTHORD, data = data2)
summary(full.model)
summary(full.model)$coefficients[, 'Estimate']


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


## Geolocalization ----

# https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

data4 <- data %>% 
  mutate(pos = gsub(".*\\sen\\s","", Address),
         pos = paste(pos, ifelse(is.na(Number), 1, Number), 'Madrid', 'Spain', sep = ', '))

d <- lapply(data4$pos[1:3], nominatim_osm) %>% 
  bind_rows()