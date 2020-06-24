library(tidyverse)
library(readxl)
library(skimr)

data <- read_excel(file.path('data', 'Houses for rent in madrid_assignment 2020.xlsx'), 1)
skim(data)

