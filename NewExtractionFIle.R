library(tidyverse)
library(xml2)
library(docstring)
source("Extraction_Functions.R")

ALLFILES = list.files(path = "./allofplos", pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

length(ALLFILES)

Maindf  = tibble()

i = 0

for(file in ALLFILES[1:10000]){
  i = i+1

  filepath = file.path('./allofplos',file)
  xmlfront = read_xml(filepath) %>% xml_child(search = 'front')
  contrib_nodes = xml_find_all(xmlfront, "//contrib")
  df = get_contrib_roles(contrib_nodes)
  Maindf <- bind_rows(Maindf, df)
  if(i%%1000 == 0){
  print(paste(i,'thousand done'))
  }

}

View(Maindf)
View(maindataframe)

