library(tidyverse)
library(xml2)
library(docstring)
library(rplos)

get_contrib_roles_matrix_map_test = function(contrib_node){
  #'Extract contributor information
  #'
  #'Takes in a contrib node and extracts name, contributor type, and contributor role from it

  doi = contrib_node %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()

  epub_date = contrib_node %>% xml_find_first(xpath = "//pub-date[@pub-type='epub']")
  edate = paste(epub_date %>% xml_child(search = "day") %>% xml_text(), epub_date %>% xml_child(search = "month") %>% xml_text(), epub_date %>% xml_child(search = "year") %>% xml_text(), sep = '-' )

  hist_date_rec = contrib_node %>% xml_find_first(xpath = "//history/date[@date-type='received']")
  recdate = paste(hist_date_rec %>% xml_child(search = "day") %>% xml_text(), hist_date_rec %>% xml_child(search = "month") %>% xml_text(), hist_date_rec %>% xml_child(search = "year") %>% xml_text(), sep = '-' )

  hist_date_acc = contrib_node %>% xml_find_first(xpath = "//history/date[@date-type='accepted']")
  accdate = paste(hist_date_acc %>% xml_child(search = "day") %>% xml_text(), hist_date_acc %>% xml_child(search = "month") %>% xml_text(), hist_date_acc %>% xml_child(search = "year") %>% xml_text(), sep = '-' )

  dates = tibble('EpubDate' = edate, 'RecDate' = recdate, 'AccDate' = accdate)

  surname = contrib_node %>% xml_child(search = "name") %>% xml_child(search = "surname") %>% xml_text()
  given_name = contrib_node %>% xml_child(search = "name") %>% xml_child(search = "given-names") %>% xml_text()

  affnum = contrib_node %>% xml_find_all(xpath = ".//xref[@ref-type='aff']") %>% xml_attr("rid")
  instlist = c()
  for(num in affnum){
    affpath = paste("//aff[@id='", num, "']",sep="")
    institution = contrib_node %>% xml_find_first(xpath = affpath) %>% xml_child(search = "addr-line") %>% xml_text()
    if (is.na(institution)){
      institution = contrib_node %>% xml_find_first(xpath = affpath) %>% xml_text()
    }
    instlist = c(instlist, institution)
  }

  insttibble = tibble(instlist)
  # View(insttibble)

  # affpath = paste("//aff[@id='", affnum, "']",sep="")
  # institution = contrib_node %>% xml_find_first(xpath = affpath) %>% xml_child(search = "addr-line") %>% xml_text()
  # if (is.na(institution)){
  #   institution = contrib_node %>% xml_find_first(xpath = affpath) %>% xml_text()
  # }

  contrib_type = contrib_node %>% xml_attr("contrib-type")
  orcid = contrib_node %>% xml_find_first(xpath = ".//contrib-id[@contrib-id-type='orcid']") %>% xml_text()
  orcid = gsub('https://orcid.org/', '',orcid)
  roles = tibble(contrib_node %>% xml_find_all(".//role") %>% xml_text())
  peer_info = !is.na(contrib_node %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text())
  data_info = !is.na(contrib_node %>% xml_find_first(xpath = "//custom-meta[@id='data-availability']") %>% xml_text())

  if (nrow(roles) == 0){
    roles = tibble('None Stated')
  }
  # data = tibble('DOI' = doi, 'Dates' = dates, 'Surname' = surname,'Given Name' = given_name, 'Institution' = institution, 'Contrib_type' = contrib_type,'Orcid' = orcid, 'Role' = roles, 'Peer' = peer_info, 'Data' = data_info) %>% nest (Role = Role, Dates = Dates)
   data = tibble('DOI' = doi, 'PubDate' = edate, 'RecDate' = recdate, 'AccDate' =accdate,'Surname' = surname,'Given Name' = given_name, 'Contrib_type' = contrib_type,'Orcid' = orcid, 'Role' = roles, 'Peer' = peer_info, 'Data' = data_info) %>% nest(Role=Role)
  # View(data)
  return(data)
}

getfromfile<- function(file){
  filepath = file.path('./allofplos',file)
  contrib_nodes = read_xml(filepath) %>% xml_child(search = 'front')  %>% xml_find_all(xpath = './/contrib-group') %>% xml_find_all(xpath = './/contrib')
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


get_funding_info = function(file){
  #'Extract contributor information
  #'
  #'Takes in a contrib node and extracts name, contributor type, and contributor role from it
  filepath = file.path('./allofplos',file)
  xml = read_xml(filepath)
  doi = xml %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
  award_nodes = xml %>% xml_find_all(xpath = './/funding-group/award-group')
  extractinfo <- function(award_node){
    surname = award_node %>% xml_child(search = 'principal-award-recipient') %>% xml_child(search = "name") %>% xml_child(search = "surname") %>% xml_text()
    given_name = award_node %>% xml_child(search = 'principal-award-recipient') %>% xml_child(search = "name") %>% xml_child(search = "given-names") %>% xml_text()
    institution = award_node %>% xml_find_first(xpath = ".//institution")  %>% xml_text()
    table = tibble(surname, given_name, institution)
    return(table)
  }

  df = map(award_nodes, extractinfo) %>% list_rbind()
  data = tibble('DOI' = doi, 'info' = df) %>% nest(info = info)
  return(data)
}

#

# plosviews("10.1371/journal.pone.0254062")

# # View(get_peer_review_names('journal.pone.0254062.xml'))
# filepath = file.path('./allofplos','journal.pbio.0020268.xml')
# file1 = 'journal.pbio.0020268.xml'
# file2 = 'journal.pone.0254062.xml'
# file3 = 'journal.pbio.0000001.xml'
# View(getfromfile(file2))
# View(get_peer_review_names(file2))
# View(get_funding_info(file2))
# xml = read_xml(filepath)
# xmlfront = read_xml(filepath) %>% xml_child(search = 'front') %>% xml_find_all(xpath = './/contrib-group') %>% xml_find_all(xpath = './/contrib')
# # doi = xml %>% xml_find_first(xpath = "//article-id[@pub-id-type='doi']") %>% xml_text()
# # peer_info = !is.na(xml %>% xml_find_first(xpath = "//sub-article[@article-type='aggregated-review-documents']") %>% xml_text())
# xmlfront
#

