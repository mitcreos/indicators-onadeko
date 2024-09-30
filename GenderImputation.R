# Open Gender Imputation
library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
ImportedData = readRDS("./Data(rds files)/Contrib_dataV7.rds")

View(ImportedData)

test = ImportedData %>% unnest(data)

names = test[,c('Given Name', 'Surname')] %>% rename(given = 'Given Name')

View(names)

# Surnames = names[,'Surname']
G_names = names[, 'given'] %>% mutate(given = word(given, 1))

# View(Surnames)
View(G_names)

Given_imputation = opengender::add_gender_predictions(G_names, dicts = c("wgen2"))

View(Given_imputation)

gender_mean(Given_imputation$og_pr_F)
