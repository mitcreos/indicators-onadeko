library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)

ImputedData = readRDS("./Data(rds files)/ContribWithImputation1.rds")
ImputedData2 = readRDS("./Data(rds files)/ReviewersWithImputation1.rds")


#Try to match roles with gender

View(ImputedData)

test2 = ImputedData %>% unnest(Role) %>% group_by(Role)

View(test2)
test2   %>% group_by(Role)
