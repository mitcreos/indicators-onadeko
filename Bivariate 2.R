library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)
library(ggplot2)


#Loading in files
ContImputraw = readRDS("./Data(rds files)/Data_with_Imputation/ContribWithImputation1.rds")
#getting rid of corrections/annotationa
ContImput <- ContImputraw %>% filter(!str_detect(DOI, "annotation")) %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}")) %>% filter(Contrib_type == 'author')
FundImput = readRDS("./Data(rds files)/FundingWithImputation2.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
RevImput = readRDS("./Data(rds files)/ReviewersWithImputation2.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
# roles_tibble = ContImput %>% unnest(Role)
journal_codes <- ContImput %>% pull(Journal_Code) %>% unique()


##### USE AUTHORS ONLY






for (journal in journal_codes){
  # Filter tibble for the current journal code
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  journal_fund  <- FundImput %>% filter(Journal_Code == journal)
  journal_rev  <- RevImput %>% filter(Journal_Code == journal)
  print(paste(journal,"Number of Cont rows:",nrow(journal_cont)))
  print(paste(journal,"Number of Fun rows:",nrow(journal_fund)))
  print(paste(journal,"Number of Rev rows:",nrow(journal_rev)))

  Contprob_F = opengender::gender_mean(journal_cont['og_pr_F'])
  print(paste(journal,"Avg Gen cont",Contprob_F))

  Fundprob_F = opengender::gender_mean(journal_fund['og_pr_F'])
  print(paste(journal,"Avg Gen fund",Fundprob_F))

  Revprob_F = opengender::gender_mean(journal_rev['og_pr_F'])
  print(paste(journal,"Avg Gen rev",Revprob_F))
}


for (journal in journal_codes) {
  # Filter datasets for the current journal
  journal_cont <- ContImput %>% filter(Journal_Code == journal)

  # Gender distribution when peer reviewed
  Peer_Reviewed <- journal_cont %>% filter(Peer == TRUE)
  print(paste(journal,"amount papers rev:",nrow(Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))

  Peerprob_F <- opengender::gender_mean(Peer_Reviewed['og_pr_F'])
  print(paste(journal, "Peer Reviewed:", Peerprob_F))

  # Gender distribution when not peer reviewed
  Not_Peer_Reviewed <- journal_cont %>% filter(Peer == FALSE)
  print(paste(journal,"amount papers not rev:",nrow(Not_Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))
  NotPeerprob_F <- opengender::gender_mean(Not_Peer_Reviewed['og_pr_F'])
  print(paste(journal, "Not Peer Reviewed:", NotPeerprob_F))

  # Gender distribution when peer reviewed (first name listed)
  Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == TRUE) %>% distinct(DOI, .keep_all = TRUE)
  Peerprob_Fn_F <- opengender::gender_mean(Peer_Reviewed_Fn['og_pr_F'])
  print(paste(journal, "Peer Reviewed First Name:", Peerprob_Fn_F))

  # Gender distribution when not peer reviewed (first name listed)
  Not_Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == FALSE) %>% distinct(DOI, .keep_all = TRUE)
  NotPeerprob_Fn_F <- opengender::gender_mean(Not_Peer_Reviewed_Fn['og_pr_F'])
  print(paste(journal, "Not Peer Reviewed First Name:", NotPeerprob_Fn_F))

  # Gender distribution when data available
  DataAv <- journal_cont %>% filter(DataAv == TRUE)
  print(paste(journal,"amount papers data av:",nrow(DataAv %>% distinct(DOI, .keep_all = FALSE))))
  DataAvprob_F <- opengender::gender_mean(DataAv['og_pr_F'])
  print(paste(journal, "Data Available:", DataAvprob_F))

  # Gender distribution when data not available
  Not_DataAv <- journal_cont %>% filter(DataAv == FALSE)
  print(paste(journal,"amount papers not data av:",nrow(Not_DataAv %>% distinct(DOI, .keep_all = FALSE))))
  NotDataAvprob_F <- opengender::gender_mean(Not_DataAv['og_pr_F'])
  print(paste(journal, "Data Not Available:", NotDataAvprob_F))

  # Gender distribution when data available (first name listed)
  DataAvFn <- journal_cont %>% filter(DataAv == TRUE) %>% distinct(DOI, .keep_all = TRUE)
  DataAvFnProb_F <- opengender::gender_mean(DataAvFn['og_pr_F'])
  print(paste(journal, "Data Available First Name:", DataAvFnProb_F))

  # Gender distribution when data not available (first name listed)
  NotDataAvFn <- journal_cont %>% filter(DataAv == FALSE) %>% distinct(DOI, .keep_all = TRUE)
  NotDataAvFnProb_F <- opengender::gender_mean(NotDataAvFn['og_pr_F'])
  print(paste(journal, "Data Not Available First Name:", NotDataAvFnProb_F))
}



# for (journal in journal_codes) {
#   # Filter datasets for the current journal
#   journal_cont <- ContImput %>% filter(Journal_Code == journal)
#
#   # Looking by Authors vs Editors
#   Authors <- journal_cont %>% filter(Contrib_type == 'author')
#   print(paste(journal, "Amount Authors:", nrow(Authors)))
#   AuthorsProb_F <- opengender::gender_mean(Authors['og_pr_F'])
#   print(paste(journal, "Authors Gender Probability:", AuthorsProb_F))
#
#   Editors <- journal_cont %>% filter(Contrib_type == 'editor')
#   print(paste(journal, "Amount Editors:", nrow(Editors)))
#   EditorsProb_F <- opengender::gender_mean(Editors['og_pr_F'])
#   print(paste(journal, "Editors Gender Probability:", EditorsProb_F))
#
#   # First names (Distinct DOIs)
#   AuthorsFn <- Authors %>% distinct(DOI, .keep_all = TRUE)
#   print(paste(journal, "Distinct Authors (DOI-based):", nrow(AuthorsFn)))
#   AuthorsFnProb_F <- opengender::gender_mean(AuthorsFn['og_pr_F'])
#   print(paste(journal, "Authors First Name Gender Probability:", AuthorsFnProb_F))
#
#   EditorsFn <- Editors %>% distinct(DOI, .keep_all = TRUE)
#   print(paste(journal, "Distinct Editors (DOI-based):", nrow(EditorsFn)))
#   EditorsFnProb_F <- opengender::gender_mean(EditorsFn['og_pr_F'])
#   print(paste(journal, "Editors First Name Gender Probability:", EditorsFnProb_F))
# }


for (journal in journal_codes) {
  # Filter datasets for the current journal
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  journal_rev <- RevImput %>% filter(Journal_Code == journal)
  journal_fund <- FundImput %>% filter(Journal_Code == journal)

  # FINDING RELATIONSHIPS
  concat_contrib <- journal_cont[, c(1, 9, 14)] %>%
    group_by(DOI) %>%
    summarise(
      Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
    )

  concat_rev <- journal_rev[, c(1, 6)] %>%
    group_by(DOI) %>%
    summarise(
      Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
    )

  concat_fund <- journal_fund[, c(1, 5)] %>%
    group_by(DOI) %>%
    summarise(
      Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
    )

  # Only getting first instance
  concat_contrib2 <- journal_cont[, c(1, 9, 14)] %>%
    distinct(DOI, .keep_all = TRUE)

  Contprob_F_first <- opengender::gender_mean(concat_contrib2['og_pr_F'])
  print(paste(journal, "First Instance Gender Probability Contributors:", Contprob_F_first))

  concat_rev2 <- journal_rev[, c(1, 6)] %>%
    distinct(DOI, .keep_all = TRUE)

  concat_fund2 <- journal_fund[, c(1, 5)] %>%
    distinct(DOI, .keep_all = TRUE)

  # Joining Contributor & Reviewer Data
  joined_data_cr <- inner_join(concat_contrib, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

  joined_data_cr2 <- inner_join(concat_contrib2, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

  # Compute Correlation
  if (nrow(joined_data_cr) > 1) {
    correlationcr <- cor(
      joined_data_cr$Avg_F_PercentContributors,
      joined_data_cr$Avg_F_PercentReviewers,
      use = "complete.obs"
    )
    print(paste(journal, "Correlation between Female Percentages Contributors & Reviewers:", correlationcr))
  } else {
    print(paste(journal, "Not enough data for correlation Contributors & Reviewers"))
  }

  if (nrow(joined_data_cr2) > 1) {
    correlationcr2 <- cor(
      joined_data_cr2$og_pr_F,
      joined_data_cr2$Avg_F_Percent,
      use = "complete.obs"
    )
    print(paste(journal, "Correlation between Female Percentages CR2:", correlationcr2))
  } else {
    print(paste(journal, "Not enough data for correlation CR2"))
  }
}


# roles_tibble = ContImput %>% unnest(Role)
# role_gender_avg <- roles_tibble %>% group_by(.[[11]]) %>%
#   summarise(Average_Gender_Score = mean(og_pr_F, na.rm = TRUE), .groups = "drop")
# saveRDS(role_gender_avg, "./Data(rds files)/RolesByGender/roles_by_gender_all.rds")

for (journal in journal_codes) {
  # Filter datasets for the current journal
  filename = paste("./Data(rds files)/RolesByGender/roles_by_gender_",journal,".rds",sep = '')
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  roles_tibble = journal_cont %>% unnest(Role)
  role_gender_avg <- roles_tibble %>% group_by(.[[11]]) %>%
    summarise(Average_Gender_Score = mean(og_pr_F, na.rm = TRUE), .groups = "drop")
  saveRDS(role_gender_avg, filename)
}



