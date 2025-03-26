library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)
library(ggplot2)


#Loading in files
ContImputraw = readRDS("./Data(rds files)/ContribWithCatImputation.rds")
#getting rid of corrections/annotationa
ContImput <- ContImputraw %>% filter(!str_detect(DOI, "annotation")) %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}")) %>% filter(Contrib_type == 'author')
FundImput = readRDS("./Data(rds files)/FundingWithCatImputation.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
RevImput = readRDS("./Data(rds files)/ReviewersWithCatImputation.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
# roles_tibble = ContImput %>% unnest(Role)
# journal_codes <- ContImput %>% pull(Journal_Code) %>% unique()
journal_codes <- FundImput %>% pull(Journal_Code) %>% unique()


##### USE AUTHORS ONLY



journal_fund  <- (FundImput %>% filter(Journal_Code == 'pbio'))['og_classification_details'] %>% unnest()

# stat <- function(race){

  for (journal in journal_codes){
    # Filter tibble for the current journal code
    # journal_cont <- (ContImput %>% filter(Journal_Code == journal))

    journal_fund  <- (FundImput %>% filter(Journal_Code == journal))['og_classification_details'] %>% unnest(cols = c(og_classification_details))
    journal_rev  <- (RevImput %>% filter(Journal_Code == journal))['og_classification_details'] %>% unnest(cols = c(og_classification_details))
    #
    # journal_fund  <- (FundImput %>% filter(Journal_Code == journal))['og_classification_pr_whi']
    # journal_rev  <- (RevImput %>% filter(Journal_Code == journal))['og_classification_pr_whi']

    # print(paste(journal,"Number of Cont rows:",nrow(journal_cont)))
    print(paste(journal,"Number of Fun rows:",nrow(journal_fund)))
    print(paste(journal,"Number of Rev rows:",nrow(journal_rev)))

    # Contprob_W = opengender::gender_mean(journal_cont[race])
    # print(paste(journal,"Avg race cont",Contprob_F))

    # Fundprob_W = opengender::gender_mean(journal_fund)
    # print(paste(journal,"Avg race fund",Fundprob_W))

    Fundprobs_R = journal_fund %>%  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    print(paste(journal,"Avg race fund"))
    print(Fundprobs_R)


    Revprob_R = journal_rev %>%  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    print(paste(journal,"Avg race rev"))
    print(Revprob_R)
  }

# }

# stat('pr_whi')

for (journal in journal_codes) {
  # Filter datasets for the current journal
  journal_cont <- ContImput %>% filter(Journal_Code == journal)

  # Race distribution when peer reviewed
  Peer_Reviewed <- journal_cont %>% filter(Peer == TRUE)
  print(paste(journal,"amount papers rev:",nrow(Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))

  Peerprob_R <- Peer_Reviewed['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))

  print(paste(journal, "Peer Reviewed:"))
  print(Peerprob_R)

  # Gender distribution when not peer reviewed
  Not_Peer_Reviewed <- journal_cont %>% filter(Peer == FALSE)
  print(paste(journal,"amount papers not rev:",nrow(Not_Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))

  NotPeerprob_R <- Not_Peer_Reviewed['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Not Peer Reviewed:"))
  print(NotPeerprob_R)

  # Gender distribution when peer reviewed (first name listed)
  Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == TRUE) %>% distinct(DOI, .keep_all = TRUE)
  Peerprob_Fn_F <- Peer_Reviewed_Fn['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Peer Reviewed First Name:"))
  print(Peerprob_Fn_F)

  # Gender distribution when not peer reviewed (first name listed)
  Not_Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == FALSE) %>% distinct(DOI, .keep_all = TRUE)
  NotPeerprob_Fn_F <- Not_Peer_Reviewed_Fn['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Not Peer Reviewed First Name:"))
  print(NotPeerprob_Fn_F)

  # Gender distribution when data available
  DataAv <- journal_cont %>% filter(DataAv == TRUE)
  print(paste(journal,"amount papers data av:",nrow(DataAv %>% distinct(DOI, .keep_all = FALSE))))
  DataAvprob_F <- DataAv['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Data Available:"))
  print(DataAvprob_F)

  # Gender distribution when data not available
  Not_DataAv <- journal_cont %>% filter(DataAv == FALSE)
  print(paste(journal,"amount papers not data av:",nrow(Not_DataAv %>% distinct(DOI, .keep_all = FALSE))))
  NotDataAvprob_F <- Not_DataAv['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Data Not Available:"))
  print(NotDataAvprob_F)

  # Gender distribution when data available (first name listed)
  DataAvFn <- journal_cont %>% filter(DataAv == TRUE) %>% distinct(DOI, .keep_all = TRUE)
  DataAvFnProb_F <- DataAvFn['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Data Available First Name:"))
  print(DataAvFnProb_F)

  # Gender distribution when data not available (first name listed)
  NotDataAvFn <- journal_cont %>% filter(DataAv == FALSE) %>% distinct(DOI, .keep_all = TRUE)
  NotDataAvFnProb_F <- NotDataAvFn['og_classification_details'] %>%
    unnest(cols = c(og_classification_details))%>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  print(paste(journal, "Data Not Available First Name:"))
  print(NotDataAvFnProb_F)
}






### UNDONE BELOW THIS LINE ####







# for (journal in journal_codes) {
#   # Filter datasets for the current journal
#   journal_cont <- ContImput %>% filter(Journal_Code == journal)
#
#   # Looking by Authors vs Editors
#   Authors <- journal_cont %>% filter(Contrib_type == 'author')
#   print(paste(journal, "Amount Authors:", nrow(Authors)))
#   AuthorsProb_F <- opengender::gender_mean(Authors['og_classification_pr_whi'])
#   print(paste(journal, "Authors Gender Probability:", AuthorsProb_F))
#
#   Editors <- journal_cont %>% filter(Contrib_type == 'editor')
#   print(paste(journal, "Amount Editors:", nrow(Editors)))
#   EditorsProb_F <- opengender::gender_mean(Editors['og_classification_pr_whi'])
#   print(paste(journal, "Editors Gender Probability:", EditorsProb_F))
#
#   # First names (Distinct DOIs)
#   AuthorsFn <- Authors %>% distinct(DOI, .keep_all = TRUE)
#   print(paste(journal, "Distinct Authors (DOI-based):", nrow(AuthorsFn)))
#   AuthorsFnProb_F <- opengender::gender_mean(AuthorsFn['og_classification_pr_whi'])
#   print(paste(journal, "Authors First Name Gender Probability:", AuthorsFnProb_F))
#
#   EditorsFn <- Editors %>% distinct(DOI, .keep_all = TRUE)
#   print(paste(journal, "Distinct Editors (DOI-based):", nrow(EditorsFn)))
#   EditorsFnProb_F <- opengender::gender_mean(EditorsFn['og_classification_pr_whi'])
#   print(paste(journal, "Editors First Name Gender Probability:", EditorsFnProb_F))
# }

for (journal in journal_codes) {
  # Filter datasets for the current journal
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  journal_rev <- RevImput %>% filter(Journal_Code == journal)
  journal_fund <- FundImput %>% filter(Journal_Code == journal)

  # FINDING RELATIONSHIPS
  # concat_contrib <- journal_cont[, c(1, 14)] %>%
  #   group_by(DOI) %>%
  #   summarise(
  #     Avg_F_Percent = mean(og_classification_pr_whi, na.rm = TRUE),
  #   )

  concat_contrib <- journal_cont[, c(1, 15)] %>%
    group_by(DOI) %>%
    unnest(cols = c(og_classification_details)) %>%
    select(-any_of(c("fuzzy_dist", "n"))) %>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))



  concat_rev <- journal_rev[, c(1, 6)] %>%
    group_by(DOI) %>%
    unnest(cols = c(og_classification_details)) %>%
    select(-any_of(c("fuzzy_dist", "n"))) %>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))

  concat_fund <- journal_fund[, c(1, 6)] %>%
    group_by(DOI) %>%
    unnest(cols = c(og_classification_details)) %>%
    select(-any_of(c("fuzzy_dist", "n"))) %>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))


  # Only getting first instance
  concat_contrib2 <- journal_cont[, c(1, 15)] %>%
    distinct(DOI, .keep_all = TRUE) %>%
    unnest(cols = c(og_classification_details)) %>%
    ungroup() %>%
    select(-any_of(c("fuzzy_dist", "n")))

  Contprob_R_first <- concat_contrib2 %>%
    select(-any_of(c("DOI"))) %>%
    summarize(across(is.numeric(), ~ mean(.x, na.rm = TRUE)))

  print(paste(journal, "First Instance Race Probability Contributors:"))
  print(Contprob_R_first)

  concat_rev2 <- journal_rev[, c(1, 6)] %>%
    distinct(DOI, .keep_all = TRUE) %>%
    unnest(cols = c(og_classification_details)) %>%
    select(-any_of(c("fuzzy_dist", "n")))

  concat_fund2 <- journal_fund[, c(1, 6)] %>%
    distinct(DOI, .keep_all = TRUE) %>%
    unnest(cols = c(og_classification_details)) %>%
    select(-any_of(c("fuzzy_dist", "n")))

  # Joining Contributor & Reviewer Data
  joined_data_cr <- inner_join(concat_contrib, concat_rev, by = "DOI", suffix = c("_contrib", "_revs"))

  joined_data_cr2 <- inner_join(concat_contrib2, concat_rev, by = "DOI", suffix = c("_contrib", "_revs"))

  #rec commented

  # # Compute Correlation
  # if (nrow(joined_data_cr) > 1) {
  #   correlationcr <- cor(
  #     joined_data_cr$Avg_F_PercentContributors,
  #     joined_data_cr$Avg_F_PercentReviewers,
  #     use = "complete.obs"
  #   )
  #   print(paste(journal, "Correlation between Female Percentages Contributors & Reviewers:", correlationcr))
  # } else {
  #   print(paste(journal, "Not enough data for correlation Contributors & Reviewers"))
  # }
  #
  # if (nrow(joined_data_cr2) > 1) {
  #   correlationcr2 <- cor(
  #     joined_data_cr2$og_classification_pr_whi,
  #     joined_data_cr2$Avg_F_Percent,
  #     use = "complete.obs"
  #   )
  #   print(paste(journal, "Correlation between Female Percentages CR2:", correlationcr2))
  # } else {
  #   print(paste(journal, "Not enough data for correlation CR2"))
  # }
}

# rec commented


# roles_tibble = ContImput %>% unnest(Role)
#
# role_gender_avg <- roles_tibble %>% group_by(.[[11]]) %>%
#   summarise(Average_Gender_Score = mean(og_classification_pr_whi, na.rm = TRUE), .groups = "drop")
#
# saveRDS(role_gender_avg, "./Data(rds files)/role_by_genderall.rds")

for (journal in journal_codes) {
  # Filter datasets for the current journal
  filename = paste("./Data(rds files)/role_by_genderall_",journal,".rds",sep = '')
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  roles_tibble = journal_cont %>% unnest(Role)
  role_gender_avg <- roles_tibble %>% group_by(.[[11]]) %>%
    summarise(Average_Gender_Score = mean(og_classification_pr_whi, na.rm = TRUE), .groups = "drop")
  saveRDS(role_gender_avg, filename)
}



