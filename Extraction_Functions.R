library(tidyverse)
library(xml2)
library(docstring)

# testfile = 'journal.pone.0254062.xml'
#
# filepath = file.path('./allofplos',testfile)
# xml = read_xml(filepath)
# testfiles = c('journal.pone.0254062.xml','journal.pone.0254062.xml')
# file = 'journal.pone.0277452.xml'
# #

# xml
# items = xml %>% xml_find_all("//contrib")
# items
#

# 10.1371/journal.pbio.2004644


get_contrib_roles_matrix_map_test = function(contrib_node){
  #'Extract contributor information
  #'
  #'Takes in a contrib node and extracts name, contributor type, and contributor role from it

  doi = contrib_node %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
  surname = contrib_node %>% xml_child(search = "name") %>% xml_child(search = "surname") %>% xml_text()
  given_name = contrib_node %>% xml_child(search = "name") %>% xml_child(search = "given-names") %>% xml_text()
  # name = tibble(surname, given_name)
  contrib_type = contrib_node %>% xml_attr("contrib-type")
  orcid = contrib_node %>% xml_find_first(xpath = ".//contrib-id[@contrib-id-type='orcid']") %>% xml_text()
  orcid = gsub('https://orcid.org/', '',orcid)
  roles = tibble(contrib_node %>% xml_find_all("role") %>% xml_text())
  peer_info = !is.na(contrib_node %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text())
  data = tibble('DOI' = doi, 'Surname' = surname,'Given Name' = given_name, 'Contrib_type' = contrib_type,'Orcid' = orcid, 'Role' = roles, 'Peer' = peer_info) %>% nest(Role = Role)
  return(data)
}

getfromfile<- function(file){
  filepath = file.path('./allofplos',file)
  xmlfront = read_xml(filepath) %>% xml_child(search = 'front')
  contrib_nodes = xml_find_all(xmlfront, ".//contrib")
  # df = map_dfr(contrib_nodes, get_contrib_roles_matrix_map_test)
  df = map(contrib_nodes, get_contrib_roles_matrix_map_test) %>% list_rbind()
  return(df)
}


# item = items[1]
# #
# View(getfromfile(file))


#
# roles = paste(item %>% xml_find_all("role") %>% xml_text(), collapse = ', ')
# roles
#
# test = c('xAQ','y','z')
# x = paste(test, collapse=', ' )
# x
