library(tidyverse)
library(xml2)
library(docstring)

testfile = 'journal.pone.0254062.xml'
#
filepath = file.path('./allofplos',testfile)
xml = read_xml(filepath)

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




get_peer_review_names = function(file){
  #'Extract contributor information
  #'
  #'Takes in a contrib node and extracts name, contributor type, and contributor role from it
  filepath = file.path('./allofplos',file)
  xml = read_xml(filepath)
  doi = xml %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
  peer_info = !is.na(xml %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text())
  if (length(peer_info) > 0){
    reviewers = c()
    p = xml %>% xml_find_all(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_find_all(xpath = ".//body") %>% xml_find_all(xpath = ".//p") %>% xml_text()
    helper = function(item){
      if(grepl('Yes:',item)){
        name = sub(".*Yes:", "", item)
        return(name)
      }
    }
    testr = map(p, helper) %>% discard(is.null)
    if (length(testr) > 0){
    data = tibble('DOI' = doi, 'Reviewers' = testr) %>% nest(Reviewers = Reviewers)
    return (data)
    }
  }
}


# View(get_peer_review_names('journal.pone.0254062.xml'))
# filepath = file.path('./allofplos',filepath = file.path('./allofplos','journal.pbio.0020268.xml')
#   xml = read_xml(filepath)
#   doi = xml %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
#   peer_info = !is.na(xml %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text()))
# xml = read_xml(filepath)
# doi = xml %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
# peer_info = !is.na(xml %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text())
#
