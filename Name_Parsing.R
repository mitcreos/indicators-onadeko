# Open Gender Imputation
library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)
ImportedData = readRDS("./Data(rds files)/Contrib_dataV7.rds")
ImportedData2 = readRDS("./Data(rds files)/Reviewer_names.rds")


test_name = " Ricardo Daniel Moreno Mauro"

name_parsing <- function(name){
  honorifics = c('Dr', 'Phd', 'MD')
  #maybe run a function that goes through the honorifics and takes them out
  name %>%  str_remove("\\s*\\([^\\)]+\\)") %>% str_squish()
}



parse_name <- function(name){
  # parsed_name = name %>% stringr::str_squish() %>% salutation
  x = name%>% parse_names
  print(x)
  # if (!is.na(x$first_name)){
  #   return (x$first_name)
  # }else if (!is.na(x$middle_name )){
  #   return (x$middle_name)
  # }else{
  #   return (NULL)
  # }
}

parse_name(test_name)

