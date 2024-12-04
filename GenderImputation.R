# Open Gender Imputation
library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
ImportedData = readRDS("./Data(rds files)/Contrib_dataV7.rds")
ImportedData2 = readRDS("./Data(rds files)/Reviewer_names.rds")

# Function to parse given name as needed
name_parsing <- function(name){

}

split_and_rejoin_imputation <- function(){

}

# View(ImportedData)

Contribs = ImportedData %>% unnest(data)

ContribsEditedTibble = Contribs[,"Given Name"] %>% rename(given = 'Given Name')%>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))

test = opengender::add_gender_predictions(ContribsEditedTibble[1,], dicts = c("wgen2"))

View(test)

bigtest = opengender::add_gender_predictions(ContribsEditedTibble[1:100000,], dicts = c("wgen2"))
#Reviewer Information

Reviewers = ImportedData2 %>% unnest(Reviewers)

ReviewersEditedTibble = Reviewers[,"Reviewers"] %>% rename(given = 'Reviewers')%>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))
View(Reviewers)
View(ReviewersEditedTibble)


Given_imputation = opengender::add_gender_predictions(ReviewersEditedTibble[1:10000,], dicts = c("wgen2"))
saveRDS(Given_imputation, file = "./Data(rds files)/ReviewersImputationPt1.rds")

Given_imputation = opengender::add_gender_predictions(ReviewersEditedTibble[10001:20000,], dicts = c("wgen2"))
saveRDS(Given_imputation, file = "./Data(rds files)/ReviewersImputationPt2.rds")

Given_imputation = opengender::add_gender_predictions(ReviewersEditedTibble[20001:30000,], dicts = c("wgen2"))
saveRDS(Given_imputation, file = "./Data(rds files)/ReviewersImputationPt3.rds")

Given_imputation = opengender::add_gender_predictions(ReviewersEditedTibble[30001:31176,], dicts = c("wgen2"))
saveRDS(Given_imputation, file = "./Data(rds files)/ReviewersImputationPt4.rds")
# x = rep(0,31176)

a = readRDS("./Data(rds files)/ReviewersImputationPt1.rds")
b = readRDS("./Data(rds files)/ReviewersImputationPt2.rds")
c = readRDS("./Data(rds files)/ReviewersImputationPt3.rds")
d = readRDS("./Data(rds files)/ReviewersImputationPt4.rds")
x = bind_rows(a,b,c,d)
View(x)

ReviewersWithImputation = Reviewers %>% bind_cols(x)
View(ReviewersWithImputation)
saveRDS(ReviewersWithImputation, file = "./Data(rds files)/ReviewersImputation1.rds")





#Contrib Information

ContribImputation = uncount(tibble(test), 2824282)
View(ContribImputation)
# for cleaner code: (floor(nrow(ContribImputation)/1000)+1) = 283

#For loop to sequentially run the imputation over the file without running out of memory(hopefully)
for (i in 1:283){
  print(i)
  if (i == 283){
    Given_imputation = opengender::add_gender_predictions(ContribsEditedTibble[2820001:2824282,], dicts = c("wgen2"))
    ContribImputation[2820001:2824282,] = Given_imputation
  }else{
    Given_imputation = opengender::add_gender_predictions(ContribsEditedTibble[((i-1)*10000 + 1):(i* 10000),], dicts = c("wgen2"))
    ContribImputation[((i-1)*10000 + 1):(i* 10000),] = Given_imputation
  }
}

saveRDS(ContribImputation, file = "./Data(rds files)/ContribImputation1.rds")

ContribWithImputation = Contribs %>% bind_cols(ContribImputation)

saveRDS(ContribWithImputation, file = "./Data(rds files)/ContribWithImputation1.rds")


View(ContribWithImputation[20000,])

# View(Given_imputation)

#Funding Imputation

Funding = readRDS("./Data(rds files)/Funding_Info.rds") %>% unnest(info)
FundingEditedTibble = tibble(given = Funding$info$given_name)

FundingEditedTibble2 = FundingEditedTibble %>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))

View(FundingEditedTibble2)

# gender_mean(Given_imputation$og_pr_F)


