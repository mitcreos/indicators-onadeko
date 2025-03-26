# Ethnic Imputation
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
  #removing parentesis and anything within them from an

  cleaned_text <-
    name %>%
    trimws()%>%
    stringr::str_remove_all("\\(.+?\\)|[.]" ) %>%
    stringr::str_to_title(locale = "en") %>%
    stringr::str_split(" ")

   temp.df <-
     tibble::tibble (
       t1 = as.character(lapply(cleaned_text,function(x)x[[1]])),
       t2 = as.character(lapply(cleaned_text,function(x)ifelse(length(x)>1,x[[2]],NA)))
   )

  temp.df %>%
     dplyr::transmute(result = ifelse(t1 %in% Honorifics, t2, t1)) %>%
     pull(result)

}

Contribs = ImportedData %>% unnest(data)
ContribsEdited = Contribs[,"Given Name"] %>% rename(Given_Name = "Given Name")
y = ContribsEdited[3,]

result <- y %>% mutate(given = parse_name(Given_Name))

test = opengender::add_category_predictions(result, dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))

split_and_rejoin_imputation <- function(given_names){
  test = opengender::add_category_predictions(given_names[1,],dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))
  size = nrow(given_names)
  end = (floor(size/10000)+1)
  ContribImputation = uncount(tibble(test), size)
  print(end)
  #For loop to sequentially run the imputation over the file without running out of memory(hopefully)
  for (i in 1:end){
    print(i)
    if (i == end){
      Given_imputation = opengender::add_category_predictions(given_names[((end-1)*10000 + 1):size,], dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))
      ContribImputation[((end-1)*10000 + 1):size,] = Given_imputation
    }else{
      Given_imputation = opengender::add_category_predictions(given_names[((i-1)*10000 + 1):(i* 10000),], dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))
      ContribImputation[((i-1)*10000 + 1):(i* 10000),] = Given_imputation
    }
  }

  return (ContribImputation)
}

# parse_name(test_name)


Reviewers = ImportedData2 %>% unnest(Reviewers)
ReviewersEditedTibble = Reviewers[,"Reviewers"] %>% mutate(given = Reviewers %>% sapply(parse_name))
View(ReviewersEditedTibble)
test = opengender::add_category_predictions(ReviewersEditedTibble[1,], dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))
View(test)
ReviewersImputation = split_and_rejoin_imputation(ReviewersEditedTibble)
View(ReviewersImputation)
ReviewersWithImputation = Reviewers %>% bind_cols(ReviewersImputation)

# saveRDS(ReviewersWithImputation, file = "./Data(rds files)/ReviewersWithCatImputation.rds")


# Contribs = ImportedData %>% unnest(data)
#ContribsEdited = Contribs[,"Given Name"] %>% rename(Given_Name = "Given Name") %>% ungroup()

ContribsEdited <- Contribs %>% ungroup() %>% dplyr::select(Given_Name = `Given Name`)

# library(parallel)
# debug(parse_name)
system.time(
ContribsEditedTibbleSamp <-
  ContribsEdited %>%
    dplyr::slice_head(n=1000) %>%
    mutate(given = parse_name(Given_Name))
)


ContribsEditedTibble = ContribsEdited %>% mutate(given = parse_name(Given_Name))

test = opengender::add_category_predictions(ContribsEditedTibble[1,],  dicts = c("rosenmanGiven"), col_map = c(input_key = "given"))
ContribImputation = split_and_rejoin_imputation(ContribsEditedTibble)
ContribWithImputation = Contribs %>% bind_cols(ContribImputation)



# saveRDS(ContribWithImputation, file = "./Data(rds files)/ContribWithCatImputation.rds")
# Contribs = ImportedData %>% unnest(data)
#
# ContribsEditedTibble = Contribs[,"Given Name"] %>% rename(given = 'Given Name')%>% mutate(given = trimws(given)) %>% mutate(given = word(given, 1))
#


# View(ImportedData)





Funding = ImportedData3 %>% unnest(info)
FundingEditedTibble = tibble(Given_Name = Funding$info$given_name) %>% ungroup
FundingEditedTibble = FundingEditedTibble %>% mutate(given = Given_Name %>% sapply(parse_name))

View(FundingEditedTibble)

FundingImputation = split_and_rejoin_imputation(FundingEditedTibble)

FundingWithImputation = Funding %>% bind_cols(FundingImputation)


# saveRDS(FundingWithImputation, file = "./Data(rds files)/FundingWithCatImputation.rds")



