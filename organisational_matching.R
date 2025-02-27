# Open Gender Imputation
library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)


InstitutionData = readRDS("./Data(rds files)/PlosData/Contrib_dataV8(DOIandinstitution).rds") %>% unnest(data) %>% ungroup()

smaller = InstitutionData[1:5,]

test = opengender::add_dict_matches(smaller, col_map = c(text = 'Institution'), dicts = c("iso3166"))

test2 = opengender::add_dict_matches(InstitutionData, col_map = c(text = 'Institution'), dicts = c("iso3166"))




# Function to parse given name as needed

test_name = "D.R. Ricardo Moreno mauro(professor of medicine)"

split_and_rejoin_imputation <- function(institutions, col_map, dicts){
  test = opengender::add_dict_matches(institutions[1,],col_map = col_map, dicts = dicts)
  size = nrow(institutions)
  end = (floor(size/10000)+1)
  InstImputation = uncount(tibble(test), size)
  print(end)
  #For loop to sequentially run the imputation over the file without running out of memory(hopefully)
  for (i in 1:end){
    print(i)
    if (i == end){
      Given_imputation = opengender::add_dict_matches(institutions[((end-1)*10000 + 1):size,], col_map = col_map, dicts = dicts)
      InstImputation[((end-1)*10000 + 1):size,] = Given_imputation
    }else{
      Given_imputation = opengender::add_dict_matches(institutions[((i-1)*10000 + 1):(i* 10000),], col_map = col_map, dicts = dicts)
      InstImputation[((i-1)*10000 + 1):(i* 10000),] = Given_imputation
    }
  }

  return (InstImputation)
}

# parse_name(test_name)

col_map = c(text = 'Institution')
dicts = c("iso3166, ror")

InstImputation = split_and_rejoin_imputation(InstitutionData, col_map = c(text = 'Institution'), dicts = c("iso3166") )



# ContribsEditedTibble = ContribsEdited %>% mutate(given = y)
# test = opengender::add_dict_matches(ContribsEditedTibble[1,], dicts = dicts)
# ContribImputation = split_and_rejoin_imputation(ContribsEditedTibble)
# ContribWithImputation = Contribs %>% bind_cols(ContribImputation)



# saveRDS(InstImputation, file = "./Data(rds files)/InstImputation1.rds")


