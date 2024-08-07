library(tidyverse)
library(xml2)
library(docstring)
source("Extraction_Functions.R")

ALLFILES = list.files(path = "./allofplos", pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

length(ALLFILES)

# Maindf  = tibble()
#
# i = 0
#
# for(file in ALLFILES){
#   i = i+1
#
#   filepath = file.path('./allofplos',file)
#   xmlfront = read_xml(filepath) %>% xml_child(search = 'front')
#   contrib_nodes = xml_find_all(xmlfront, "//contrib")
#   df = get_contrib_roles(contrib_nodes)
#   Maindf <- bind_rows(Maindf, df)
#   if(i%%1000 == 0){
#   print(paste(i,'thousand done'))
#   }
#
# }
#
# View(Maindf)
#


Mainmat <- matrix(ncol = 3)

filesdone = 0
filessaved = 0

for(file in ALLFILES[]){
  filepath = file.path('./allofplos',file)
  xmlfront = read_xml(filepath) %>% xml_child(search = 'front')
  contrib_nodes = xml_find_all(xmlfront, "//contrib")
  mat = get_contrib_roles_matrix(contrib_nodes)
  Mainmat <- rbind(Mainmat, mat)
  filesdone = filesdone+1
  if(filesdone%%1000 == 0){
    print(paste(filesdone,'files done','|',filessaved,'files saved'))
  }
  if (filesdone%%30000 == 0){
    filessaved = filessaved + 1
    filename = paste(c('contrib_roles',filessaved,'.csv'),collapse = '')
    matfilepath = file.path('./Contributor_roles_files',filename)
    Mainmat <- Mainmat[rowSums(is.na(Mainmat)) != ncol(Mainmat),]
    # write.csv(Mainmat,file=matfilepath)
    # Mainmat <- matrix(ncol = 3)
  }
}
# write.csv(Mainmat,file='./Contributor_roles_files/contrib_roles14.csv')
View(Mainmat)

