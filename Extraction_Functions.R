library(tidyverse)
library(xml2)
library(docstring)


get_contrib_roles = function(contrib_nodes){
  #'Extract contributor information
  #'
  #'Takes in a vector of contrib nodes and extracts name, contributor type, and contributor role from it
  dt <- tibble()
  for (item in contrib_nodes){
    name = item %>% xml_child(search = "name") %>% xml_text()
    contrib_type = item %>% xml_attr("contrib-type")
    roles = paste(item %>% xml_find_all("role") %>% xml_text(), collapse = ', ')
    dt <- bind_rows(dt, tibble(Name = name, ContribType = contrib_type, Roles = roles))
  }
  return (dt)
}

get_contrib_roles_matrix = function(contrib_nodes){
  #'Extract contributor information
  #'
  #'Takes in a vector of contrib nodes and extracts name, contributor type, and contributor role from it
    tempmatrix <- matrix(data = NA, ncol = 3,nrow = 5000)
    for (i in 1:length(contrib_nodes)){
    contrib_node = contrib_nodes[i]
    name = contrib_node %>% xml_child(search = "name") %>% xml_text()
    contrib_type = contrib_node %>% xml_attr("contrib-type")
    roles = paste(contrib_node %>% xml_find_all("role") %>% xml_text(), collapse = ', ')
    tempmatrix[i,] <- c(name, contrib_type, roles)
    }
    tempmatrix <- tempmatrix[rowSums(is.na(tempmatrix)) != ncol(tempmatrix),]
  return (tempmatrix)
}



# testfile = 'journal.pone.0254062.xml'
#
# filepath = file.path('./allofplos',testfile)
# xml = read_xml(filepath)
#
# xml
# items = xml %>% xml_find_all("//contrib")
# items
# item = items[1]
#
# roles = paste(item %>% xml_find_all("role") %>% xml_text(), collapse = ', ')
# roles
#
# test = c('xAQ','y','z')
# x = paste(test, collapse=', ' )
# x
