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

# Data = map(ALLFILES, getfromfile, .progress = TRUE)  %>% list_rbind() %>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()


# saveRDS(Data, file = "./Data(rds files)/Contrib_dataV7.rds")

# View(Data)

# Reviewer_names = map(ALLFILES, get_peer_review_names, .progress = TRUE) %>% list_rbind()

# saveRDS(Reviewer_names, file = "Reviewer_names.rds")
#
#
# View(Reviewer_names)
#
Funding = map(ALLFILES, get_funding_info, .progress = TRUE)  %>% list_rbind()
#
View(Funding)

saveRDS(Funding, file = "./Data(rds files)/Funding_Info.rds")

