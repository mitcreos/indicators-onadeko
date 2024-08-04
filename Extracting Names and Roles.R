library(tidyverse)
library(xml2)
library(docstring)




# x = list.files(path = "./allofplos", pattern = NULL, all.files = FALSE,
#            full.names = FALSE, recursive = FALSE,
#            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# length(x)



#

###error in item 351021 of the files

get_name_and_role <- function(listofplosfiles){
  #' get_name_and_role
  #'
  #' Takes in a list of xml file names from the allofploss folder as input and returns a tibble with
  #' the contributior name and role
  #'
i = 0
for(file in listofplosfiles){
  filepath = file.path('./allofplos',file)
  xml = read_xml(filepath)
  contrib_roles = xml_find_all(xml, "//contrib") %>% xml_attr("contrib-type")
  if (length(contrib_roles) > 0){
    contrib_names = xml_find_all(xml, "//contrib//name") %>% xml_text()
    if(length(contrib_roles) == length(contrib_names)){
      dataframe = tibble(Role = contrib_roles,Name = contrib_names)

    maindataframe <- bind_rows(maindataframe, dataframe)
  }

  }
  i = i + 1
  if (i %% 1000 == 0){
    print(paste("Files Done:",i))
  }
}
return (maindataframe)
}




get_name_and_role(c('journal.pbio.0000002.xml'))

