library(tidyverse)
library(dplyr)
library(readr)

get_url <- "https://raw.githubusercontent.com/meiqing39/607-Project-3/refs/heads/main/data_cleaned_2021.csv"
load_url <- read_csv(get_url)
colnames(load_url)
