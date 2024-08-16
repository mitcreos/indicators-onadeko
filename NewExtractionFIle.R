library(tidyverse)
library(xml2)
library(docstring)
source("Extraction_Functions.R")

# error in file /allofplos/journal.ppat.1003829.xml deleted it from files

ALLFILES = list.files(path = "./allofplos", pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


# y = c(ALLFILES[1:10], 'journal.pone.0254062.xml')
# y

Data = map(ALLFILES, getfromfile, .progress = TRUE)  %>% list_rbind() %>% group_by(DOI,Peer) %>% nest()


saveRDS(Data, file = "Contrib_dataV4.rds")

View(Data)


