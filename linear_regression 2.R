library(tidyverse)
library(readxl)
library(skimr)
library(MASS)
library(leaflet)
library(PerformanceAnalytics)
library(gmodels)

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
                  ifelse(grepl('^Caserón\\sen', Address), 'Chalet',
                  ifelse(grepl('^Casa\\so\\schalet\\sindependiente\\sen', Address), 'Chalet',
                  ifelse(grepl('^Chalet\\sen', Address), 'Chalet', 'Other')))))))))),
    # Fix Area
    Area = gsub('\\s-\\s', '-', Area),
    Area = gsub('^en\\s', '', Area),
    Area = gsub('chalet independiente en Nueva España', 'Nueva España', Area),
    # Chalet
    Floor = ifelse(is.na(Floor) & Type == 'Chalet', 0, Floor),
    Outer = ifelse(is.na(Outer) & Type == 'Chalet', 0, Outer),
    Elevator = ifelse(is.na(Elevator) & Type == 'Chalet', 0, Elevator),
    # Estudio
    Bedrooms = ifelse(is.na(Bedrooms) & Type == 'Estudio', 0, Bedrooms),
    # Chalet pareado
    Floor = ifelse(is.na(Floor) & Type == 'Chalet pareado', 0, Floor),
    Outer = ifelse(is.na(Outer) & Type == 'Chalet pareado', 0, Outer),
    # Chalet adosado
    Floor = ifelse(Type == 'Chalet adosado', 0, Floor), # Data error
    Outer = ifelse(is.na(Outer) & Type == 'Chalet adosado', 0, Outer),
    # Factoring
    Outer = factor(Outer, levels = c(0, 1), labels = c('No', 'Yes')),
    Elevator = factor(Elevator, levels = c(0, 1), labels = c('No', 'Yes')),
    # Type, but will be removed
    Penthouse = factor(Penthouse, levels = c(0, 1), labels = c('No', 'Yes')), # Ático
    Cottage = factor(Cottage, levels = c(0, 1), labels = c('No', 'Yes')), # Casa o chalet independiente, Caserón, Chalet, Chalet adosado, Chalet pareado
    Duplex = factor(Duplex, levels = c(0, 1), labels = c('No', 'Yes')), # Dúplex
    Semidetached = factor(Semidetached, levels = c(0, 1), labels = c('No', 'Yes')) #Chalet, Chalet adosado
  )
skim(data1)

# data1 %>% 
#   filter(Type == 'Chalet adosado') %>% 
#   dplyr::select(-Id, -District, -Address, -Number, -Area, -Type) %>% 
#   skim(.)

# data1 %>%
#   filter(is.na(Floor)) %>%
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
  mutate(AddressComplete = gsub(".*\\sen\\s","", Address),
         AddressComplete = paste(AddressComplete, ifelse(is.na(Number), 1, Number), Area, 'Madrid', 'Spain', sep = ', '),
         AreaComplete = paste(Area, 'Madrid', 'Spain', sep = ', '))

# Group full address
data3 <- data2 %>% 
  group_by(AddressComplete) %>% 
  summarize(n=n())

# Look for the geolocalization
# system.time({
#   data_geopos_address <- lapply(data3$AddressComplete, nominatim_osm) %>%
#     bind_rows()
# })
# saveRDS(object = data_geopos_address, file = file.path('storage', 'data_geopos_address.RData'))
data_geopos_address <- readRDS(file = file.path('storage', 'data_geopos_address.RData'))

# Group area
data3 <- data2 %>% 
  filter(!AddressComplete %in% data_geopos_address$address) %>% 
  group_by(AreaComplete) %>% 
  summarize(n=n())

# Look for the geolocalization
# system.time({
#   data_geopos_area <- lapply(data3$AreaComplete, nominatim_osm) %>%
#     bind_rows()
# })
# saveRDS(object = data_geopos_area, file = file.path('storage', 'data_geopos_area.RData'))
data_geopos_area <- readRDS(file = file.path('storage', 'data_geopos_area.RData'))

data3 <- data2 %>% 
  left_join(data_geopos_address, by = c("AddressComplete" = "address")) %>% 
  left_join(data_geopos_area, by = c("AreaComplete" = "address")) %>% 
  mutate(Longitude = ifelse(is.na(lon.x), lon.y, lon.x),
         Latitude = ifelse(is.na(lat.x), lat.y, lat.x)) %>% 
  dplyr::select(-any_of(c('lon.x', 'lat.x', 'lon.y', 'lat.y', 'AddressComplete', 'AreaComplete')))

skim(data3)

## Map ----
leaflet(data = data3) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, 
             popup = ~as.character(Id), 
             label = ~as.character(paste(gsub(".*\\sen\\s","", Address), ifelse(is.na(Number), 1, Number), Area,'Madrid', 'Spain', sep = ', ')))

## Select data ----

data4 <- data3 %>% 
  # mutate_at(vars(Rent, Sq.Mt), log10) %>%
  # rename_at(vars(Rent, Sq.Mt), function(x) paste('Log10(', x, ')', sep = '')) %>% 
  # mutate_if(is.numeric, scale) %>% 
  dplyr::select(-Id, -Address, -Number, -Area, -District) %>% # Replaced by lat and lon.
  dplyr::select(-Penthouse, -Cottage, -Duplex, -Semidetached) %>% # 'Type' is more comprehensive
  filter_at(vars(Outer, Elevator, Bedrooms, Floor, Latitude, Longitude), all_vars(!is.na(.))) # Remove nulls

skim(data4)

## Correlation ----

# C:\Users\juanb\OneDrive\GMBD\2020-01-10 - TERM 1\STATISTICAL PROGRAMMING - R (MBD-EN-BL2020J-1_32R369_316435)\Group assignment\GitHub\gmbd_r\!Delivery\EDA-1.R

data4 %>% 
  # dplyr::select(Bedrooms, Sq.Mt, Floor, Longitude, Latitude) %>% 
  dplyr::select_if(is.numeric) %>% 
  chart.Correlation(histogram=TRUE)

## Frequency tables ----
# https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/

CrossTable(data4$Type, data4$Elevator, expected = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data4$Type, data4$Outer, expected = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data4$Type, data4$Bedrooms, expected = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data4$Type, data4$Floor, expected = FALSE, prop.t = FALSE, prop.chisq = FALSE)

## Sankey ----

# https://www.displayr.com/how-to-create-sankey-diagrams-from-tables-using-r/
# install.packages("devtools")
# library(devtools)
# install_github("Displayr/flipPlots")
library(flipPlots)

data1 %>% 
  dplyr::select(Type, Outer, Elevator, Bedrooms, Floor) %>% 
  SankeyDiagram(link.color = "Source") 

data1 %>% 
  dplyr::select(District, Type) %>% 
  SankeyDiagram(link.color = "Target", max.categories = 300) 

## Densities ----

# data4 %>% 
#   ggplot(aes(x = Sq.Mt, color = Type)) +
#   # geom_histogram(aes(y=..density..)) +
#   geom_density(alpha=.2) +
#   labs(x = 'Log10(Sq.Mt)', y = 'Density')
# 
# data4 %>%
#   ggplot(aes(x = Bedrooms, color = Type)) +
#   # geom_histogram(aes(y=..density..)) +
#   geom_density(alpha=.2) +
#   labs(x = 'Bedrooms', y = 'Density')

library(ggridges)
# http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
theme_set(theme_ridges())

data4 %>% 
  ggplot(aes(x = Sq.Mt, y = Type)) +
  # geom_histogram(aes(y=..density..)) +
  geom_density_ridges(aes(fill = Type), alpha=.2) +
  labs(x = 'Sq.Mt', y = 'Density') +
  theme(legend.position = "none")

data4 %>%
  ggplot(aes(x = Bedrooms, y = Type)) +
  # geom_histogram(aes(y=..density..)) +
  geom_density_ridges(aes(fill = Type), alpha=.2) +
  labs(x = 'Bedrooms', y = 'Density') +
  theme(legend.position = "none")

data4 %>%
  ggplot(aes(x = Floor, y = Type)) +
  # geom_histogram(aes(y=..density..)) +
  geom_density_ridges(aes(fill = Type), alpha=.2) +
  labs(x = 'Floor', y = 'Density') +
  theme(legend.position = "none")

data4 %>%
  ggplot(aes(x = Sq.Mt)) +
  stat_ecdf(aes(color = Type, linetype = Type), geom = "step", size = 1) +
  labs(y = "f(Sq.Mt)")

data4 %>%
  ggplot(aes(x = Bedrooms)) +
  stat_ecdf(aes(color = Type, linetype = Type), geom = "step", size = 1) +
  labs(y = "f(Bedrooms)")

## Impact of removing one feature at a time ----

bind_cols(
  FeatureRemoved = c('None', colnames(data4)[2:length(colnames(data4))]),
bind_rows(
  coef(glm(Rent ~ ., data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Bedrooms, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Sq.Mt, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Floor, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Outer, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Elevator, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Type, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Longitude, data = data4, family = gaussian(link = "identity"))),
  coef(glm(Rent ~ . - Latitude, data = data4, family = gaussian(link = "identity"))))
)

## Summary per factor ----
data4 %>% 
  group_by(Outer) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(),
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  group_by(Elevator) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(),
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  group_by(Type) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(),
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  group_by(Bedrooms) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(),
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  group_by(Floor) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(),
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  mutate(Sq.MtRanges = cut(data4$Sq.Mt, breaks = 6)) %>% 
  group_by(Sq.MtRanges) %>% 
  summarise(Mean=mean(Rent), SD=sd(Rent), N=n(), 
            p0.25 = quantile(Rent, probs = 0.25), 
            p0.50 = quantile(Rent, probs = 0.50), 
            p0.75 = quantile(Rent, probs = 0.75),
            .groups = 'drop')

data4 %>% 
  mutate(Sq.MtRanges = cut(data4$Sq.Mt, breaks = 6)) %>% 
  ggplot(aes(x = Rent/Sq.Mt, y = Sq.MtRanges)) +
  # geom_histogram(aes(y=..density..)) +
  geom_density_ridges(aes(fill = Sq.MtRanges), alpha=.2) +
  labs(x = 'Rent/Sq.Mt', y = 'Sq.Mt range') +
  theme(legend.position = "none")

data4 %>% 
  mutate(Sq.MtRanges = cut(data4$Sq.Mt, breaks = 6)) %>% 
  ggplot(aes(x = Rent, y = Sq.MtRanges)) +
  # geom_histogram(aes(y=..density..)) +
  geom_density_ridges(aes(fill = Sq.MtRanges), alpha=.2) +
  labs(x = 'Rent', y = 'Sq.Mt range') +
  theme(legend.position = "none")

## Regression model ----

# Fit full model
# model_full <- glm(Rent ~ ., data = data1)
# https://stats.stackexchange.com/questions/181113/is-there-any-difference-between-lm-and-glm-for-the-gaussian-family-of-glm
# full.model1 <- lm(Rent ~ ., data = data1)
model_full <- glm(Rent ~ ., data = data4, family = gaussian(link = "identity"))
summary(model_full)

# Step model
model_step <- model_full %>% 
  stepAIC(trace = TRUE)
summary(model_step)

## Good opportunities ----
# Find good opportunities in the market looking for flats that may be under their theoretical estimated price

data6 <- data4
data6$prediction <- predict(model_step, data6)
data6 %>% 
  # filter(Rent<prediction) %>%
  arrange(desc(prediction/Rent-1))

## Model summary table ----

# Scaled
data7 <- data4 %>% 
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
