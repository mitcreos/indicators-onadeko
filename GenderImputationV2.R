# Open Gender Imputation
library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)


ImportedData = readRDS("./Data(rds files)/PlosData/Contrib_dataV8.rds")
ImportedData2 = readRDS("./Data(rds files)/PlosData/Reviewer_names.rds")
ImportedData3 = readRDS("./Data(rds files)/PlosData/Funding_Info.rds")
Honorifics = read_lines("list-of-salutations-titles-honorifics.txt")
Honorifics



# Function to parse given name as needed

test_name = "D.R. Ricardo Moreno mauro(professor of medicine)"


parse_name <- function(name){
  # parsed_name = name %>% stringr::str_squish() %>% salutation
  returned_name = ''
  #Using Honorific DataBase
  Honorifics = read_lines("list-of-salutations-titles-honorifics.txt")
  #removing parentesis and anything within them from an input
  cleaned_text <- gsub("\\s*\\([^\\)]+\\)", "", name)
  # print(cleaned_text)
  #Converting the input to title case
  cleaned_text <- str_to_title(cleaned_text, locale = "en")
  # print(cleaned_text)
  #Spliting the string at periods and spaces
  segmented_word = unlist(strsplit(cleaned_text, c(" ")))
  segmented_word = unlist(strsplit(segmented_word, c("[.]")))
  # print(segmented_word)
  # print('af')
  assumed_first = trimws(segmented_word[1])
  # print(assumed_first)

  tryCatch({
    if ((nchar(assumed_first) != 1) && !(assumed_first %in% Honorifics)){
      return (assumed_first)
    }else{
      for (i in 2:length(segmented_word)){
        word = trimws(segmented_word[i])
        if (i == length(segmented_word)){
          return ('Not Processed')
        }else if ((nchar(word) != 1) || (i == (length(segmented_word) - 1))){
          return (word)
        }else{
          next
        }
      }
    }
  },error = function(msg){
      return(NA)}
  )
}

split_and_rejoin_imputation <- function(given_names){
  test = opengender::add_gender_predictions(given_names[1,], dicts = c("wgen2"))
  size = nrow(given_names)
  end = (floor(size/10000)+1)
  ContribImputation = uncount(tibble(test), size)
  print(end)
  #For loop to sequentially run the imputation over the file without running out of memory(hopefully)
  for (i in 1:end){
    print(i)
    if (i == end){
      Given_imputation = opengender::add_gender_predictions(given_names[((end-1)*10000 + 1):size,], dicts = c("wgen2"))
      ContribImputation[((end-1)*10000 + 1):size,] = Given_imputation
    }else{
      Given_imputation = opengender::add_gender_predictions(given_names[((i-1)*10000 + 1):(i* 10000),], dicts = c("wgen2"))
      ContribImputation[((i-1)*10000 + 1):(i* 10000),] = Given_imputation
    }
  }

  return (ContribImputation)
}

# parse_name(test_name)


Reviewers = ImportedData2 %>% unnest(Reviewers)
ReviewersEditedTibble = Reviewers[,"Reviewers"] %>% mutate(given = Reviewers %>% sapply(parse_name))
View(ReviewersEditedTibble)
test = opengender::add_gender_predictions(ReviewersEditedTibble[1,], dicts = c("wgen2"))
View(test)
ReviewersImputation = split_and_rejoin_imputation(ReviewersEditedTibble)
View(ReviewersImputation)
ReviewersWithImputation = Reviewers %>% bind_cols(ReviewersImputation)


Contribs = ImportedData %>% unnest(data)
ContribsEdited = Contribs[,"Given Name"] %>% rename(Given_Name = "Given Name")
y = ContribsEdited[1:100000,]$Given_Name
library(parallel)


result <- mclapply(y, parse_name, mc.cores = 4)

ContribsEditedTibble = ContribsEdited %>% mutate(given = y)
test = opengender::add_gender_predictions(ContribsEditedTibble[1,], dicts = c("wgen2"))
ContribImputation = split_and_rejoin_imputation(ContribsEditedTibble)
ContribWithImputation = Contribs %>% bind_cols(ContribImputation)



saveRDS(ContribWithImputation, file = "./Data(rds files)/ContribWithImputation3.rds")
# Contribs = ImportedData %>% unnest(data)
#
# ContribsEditedTibble = Contribs[,"Given Name"] %>% rename(given = 'Given Name')%>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))
#


# View(ImportedData)





Funding = readRDS("./Data(rds files)/Funding_Info.rds") %>% unnest(info)
FundingEditedTibble = tibble(given = Funding$info$given_name)
FundingEditedTibble2 = FundingEditedTibble %>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))

View(FundingEditedTibble2)

FundingImputation = split_and_rejoin_imputation(FundingEditedTibble2)

FundingWithImputation = Funding %>% bind_cols(FundingImputation)


saveRDS(FundingWithImputation, file = "./Data(rds files)/FundingWithImputation1.rds")



