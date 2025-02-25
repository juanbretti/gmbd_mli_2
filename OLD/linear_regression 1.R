library(tidyverse)
library(readxl)
library(skimr)
library(MASS)
library(leaflet)

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
    Area = gsub('\\s-\\s', '-', Area),
    Area = gsub('^en\\s', '', Area),
    Area = gsub('chalet independiente en Nueva España', 'Nueva España', Area),
    Outer = factor(Outer, levels = c(0, 1), labels = c('No', 'Yes')),
    Elevator = factor(Elevator, levels = c(0, 1), labels = c('No', 'Yes')),
    Penthouse = factor(Penthouse, levels = c(0, 1), labels = c('No', 'Yes')), # Ático
    Cottage = factor(Cottage, levels = c(0, 1), labels = c('No', 'Yes')), # Casa o chalet independiente, Caserón, Chalet, Chalet adosado, Chalet pareado
    Duplex = factor(Duplex, levels = c(0, 1), labels = c('No', 'Yes')), # Dúplex
    Semidetached = factor(Semidetached, levels = c(0, 1), labels = c('No', 'Yes')) #Chalet, Chalet adosado
  )
skim(data1)

# data1 %>% 
#   filter(Semidetached == 'Yes') %>% 
#   group_by(Type) %>% 
#   summarise(n=n())

## Geolocalization ----

# https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 

# Function to geolocate
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
  return(data.frame(address = address, lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

# Full address
data2 <- data1 %>% 
  mutate(address_complete = gsub(".*\\sen\\s","", Address),
         address_complete = paste(address_complete, ifelse(is.na(Number), 1, Number), Area,'Madrid', 'Spain', sep = ', ')) %>% 
  group_by(address_complete) %>% 
  summarize(n=n())

# Look for the geolocalization
# system.time({
#   data_geopos_address <- lapply(data2$address_complete, nominatim_osm) %>%
#     bind_rows()
# })
# saveRDS(object = data_geopos_address, file = file.path('storage', 'data_geopos_address.RData'))
data_geopos_address <- readRDS(file = file.path('storage', 'data_geopos_address.RData'))

# Only for the areas of addresses with no data
data3 <- data1 %>% 
  mutate(address_complete = gsub(".*\\sen\\s","", Address),
         address_complete = paste(address_complete, ifelse(is.na(Number), 1, Number), Area,'Madrid', 'Spain', sep = ', ')) %>% 
  left_join(data_geopos_address, by = c("address_complete" = "address"))

# Prepare
data4 <- data3 %>% 
  filter(is.na(lat)) %>% 
  group_by(Area) %>% 
  summarize(n=n()) %>% 
  mutate(area_complete = paste(Area,'Madrid', 'Spain', sep = ', '))

# Look for the geolocalization
# system.time({
#   data_geopos_area <- lapply(data4$area_complete, nominatim_osm) %>%
#     bind_rows()
# })
# saveRDS(object = data_geopos_area, file = file.path('storage', 'data_geopos_area.RData'))
data_geopos_area <- readRDS(file = file.path('storage', 'data_geopos_area.RData'))

data4 <- data3 %>% 
  mutate(area_complete = paste(Area,'Madrid', 'Spain', sep = ', ')) %>% 
  left_join(data_geopos_area, by = c("area_complete" = "address")) %>% 
  mutate(Longitude = ifelse(is.na(lon.x), lon.y, lon.x),
         Latitude = ifelse(is.na(lat.x), lat.y, lat.x)) %>% 
  dplyr::select(-any_of(c('lon.x', 'lat.x', 'lon.y', 'lat.y', 'address_complete', 'area_complete')))

skim(data4)

## Map ----
leaflet(data = data4) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, 
             popup = ~as.character(Id), 
             label = ~as.character(paste(gsub(".*\\sen\\s","", Address), ifelse(is.na(Number), 1, Number), Area,'Madrid', 'Spain', sep = ', ')))

## Select data ----

data5 <- data4 %>% 
  mutate_at(vars(Rent, Sq.Mt), log10) %>%
  # mutate_if(is.numeric, scale) %>% 
  dplyr::select(-Id, -Address, -Number, -Area, -District) %>%
  dplyr::select(-Penthouse, -Cottage, -Duplex, -Semidetached) %>% 
  filter_at(vars(Floor), all_vars(. <= quantile(., 0.9999, na.rm = TRUE))) %>%
  filter_at(vars(Outer, Elevator, Bedrooms, Floor), all_vars(!is.na(.)))
  
skim(data5)
## Model ----

# Fit full model
# model_full <- glm(Rent ~ ., data = data1)
# https://stats.stackexchange.com/questions/181113/is-there-any-difference-between-lm-and-glm-for-the-gaussian-family-of-glm
# full.model1 <- lm(Rent ~ ., data = data1)
model_full <- glm(Rent ~ ., data = data5, family = gaussian(link = "identity"))
summary(model_full)

# Step model
model_step <- model_full %>% 
  stepAIC(trace = FALSE)
summary(model_step)

## Good opportunities ----
# Find good opportunities in the market looking for flats that may be under their theoretical estimated price

data6 <- data5
data6$prediction <- predict(model_step, data5)
data6 %>% 
  # filter(Rent<prediction) %>%
  arrange(desc(prediction/Rent-1))

## Summary ----

# Scaled
data7 <- data5 %>% 
  mutate_if(is.numeric, scale)
model_step_scaled <- glm(formula(model_step), data = data7, family = gaussian(link = "identity"))
summary(model_step_scaled)

# First version of the summary
summary_unit <- summary(model_step)$coefficients %>% 
  data.frame() %>% 
  rownames_to_column()
summary_scaled <- summary(model_step_scaled)$coefficients %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename_all(function(x) paste(x, 'scaled', sep = '_'))
bind_cols(summary_unit,
          dplyr::select(summary_scaled, Estimate_scaled, Std..Error_scaled, t.value_scaled))

# Second version of the summary
regression_table <- function(x, x_scaled, level = 0.95) {
  table <- cbind(
    summary(x)$coefficients,
    summary(x_scaled)$coefficients[, 'Estimate'],
    confint.default(x, level = level))
  colnames(table)[5] <- "Estimate Std."
  return(table)
}

regression_table(model_step, model_step_scaled)

## Case in the PPT ----
data <- read_excel(path = file.path('data', 'wage.xlsx'), sheet = 1)

# Maintaining units
model_full <- lm(WAGE ~ AFROAMERICAN + AGE + EDUC + EXPER + HOURS + MARRIED + SIBS + BRTHORD, data = data)
summary(model_full)
# Scaled
data2 <- data %>% 
  mutate_if(is.numeric, scale)
model_full <- lm(WAGE ~ AFROAMERICAN + AGE + EDUC + EXPER + HOURS + MARRIED + SIBS + BRTHORD, data = data2)
summary(model_full)
summary(model_full)$coefficients[, 'Estimate']