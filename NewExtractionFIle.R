library(tidyverse)
library(xml2)
library(docstring)
source("Extraction_Functions.R")

# error in file /allofplos/journal.ppat.1003829.xml deleted it from files

ALLFILES = list.files(path = "./allofplos", pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# ALLFILES = ALLFILES[1:100]


# y = c(ALLFILES[1:10], 'journal.pone.0254062.xml')
# y

# test = getfromfile(ALLFILES[1]) %>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()
# size = length(ALLFILES)
# print(size)
# Data = uncount(tibble(test), size)
# #
# for(i in 1:size){
#   x = getfromfile(ALLFILES[i])
#   if (i%%100 == 0){
#     print(paste(i,'/',size))
#     # print(i)
#
#   }
#   if (nrow(x) == 0){
#     # print('null')
#     # x = test %>% mutate(across(everything(), ~ "NUL"))
#     x = test %>% mutate(DOI = "NULL")
#     # view(x)
#     y = x %>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()
#   }else{
#     y = x %>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()
#   }
#   Data[i,] = y
  #%>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()
# }

# Data = map(ALLFILES, getfromfile, .progress = TRUE)  %>% list_rbind() %>% group_by(DOI,Peer, DataAv, PubDate, RecDate, AccDate) %>% nest()
Data = map(ALLFILES, getfromfile, .progress = TRUE)  %>% list_rbind() %>% group_by(DOI) %>% nest()

# filtered_tibble <- Data %>% filter(DOI != "NULL")

# saveRDS(Data, file = "./Data(rds files)/Contrib_dataV8.rds")
saveRDS(Data, file = "./Data(rds files)/Contrib_dataV8(DOIandinstitution).rds")

# saveRDS(filtered_tibble, file = "./Data(rds files)/Contrib_dataV8.rds")
# concat = Contrib_dataV7[1:358085,]
# saveRDS(concat, file = "./Data(rds files)/Contrib_dataV8.rds")

# library(furrr)
# plan(multisession)
#
# result <- concat %>%
#   left_join(Data, by = "DOI") %>%
#   mutate(
#     data = future_map2(data.x, data.y, ~ bind_cols(.x, select(.y, 'Institution')))
#   )  # Keep only the desired columns






# View(Data)
# View(filtered_tibble)

# Reviewer_names = map(ALLFILES, get_peer_review_names, .progress = TRUE) %>% list_rbind()

# saveRDS(Reviewer_names, file = "Reviewer_names.rds")
#
#
# View(Reviewer_names)
#
Funding = map(ALLFILES[1:100], get_funding_info, .progress = TRUE)  %>% list_rbind()
#
View(Funding)

# saveRDS(Funding, file = "./Data(rds files)/Funding_Info.rds")

