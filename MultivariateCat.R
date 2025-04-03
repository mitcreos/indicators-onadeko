library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)
library(ggplot2)

img_file0 = "./wrk_imgs/MultivatiateCat0.RData"
img_file1 = "./wrk_imgs/MultivatiateCat1.RData"
img_file2 = "./wrk_imgs/MultivatiateCat2.RData"
img_file3 = "./wrk_imgs/MultivatiateCat3.RData"



if (file.exists(img_file0)){
  load(img_file0)
}else{
  #Loading in files
  ContImputraw = readRDS("./Data(rds files)/ContribWithCatImputation.rds")
  #getting rid of corrections/annotationa
  ContImput <- ContImputraw %>% filter(!str_detect(DOI, "annotation")) %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}")) %>% filter(Contrib_type == 'author')
  FundImput = readRDS("./Data(rds files)/FundingWithCatImputation.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
  RevImput = readRDS("./Data(rds files)/ReviewersWithCatImputation.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
  # roles_tibble = ContImput %>% unnest(Role)
  # journal_codes <- ContImput %>% pull(Journal_Code) %>% unique()
  journal_codes <- FundImput %>% pull(Journal_Code) %>% unique()
  save.image(img_file0)
}

  ##### USE AUTHORS ONLY


if (file.exists(img_file1)){
  load(img_file1)
}else{
  journal_fund  <- (FundImput %>% filter(Journal_Code == 'pbio'))['og_classification_details'] %>% unnest()

  # stat <- function(race){
  probs_list <- list()
  for (journal in journal_codes){
    # Filter tibble for the current journal code
    # journal_cont <- (ContImput %>% filter(Journal_Code == journal))
    # journal_fund  <- (FundImput %>% filter(Journal_Code == journal))['og_classification_details'] %>% unnest(cols = c(og_classification_details))
    # journal_rev  <- (RevImput %>% filter(Journal_Code == journal))['og_classification_details'] %>% unnest(cols = c(og_classification_details))

    journal_cont <- (ContImput %>% filter(Journal_Code == journal)) ['og_classification_pr_whi']
    journal_fund  <- (FundImput %>% filter(Journal_Code == journal))['og_classification_pr_whi']
    journal_rev  <- (RevImput %>% filter(Journal_Code == journal))['og_classification_pr_whi']

    print(paste(journal,"Number of Cont rows:",nrow(journal_cont)))
    print(paste(journal,"Number of Fun rows:",nrow(journal_fund)))
    print(paste(journal,"Number of Rev rows:",nrow(journal_rev)))

    Contprob_W = opengender::class_mean(journal_cont)
    print(paste(journal,"Avg race cont",Contprob_W))

    Fundprob_W = opengender::class_mean(journal_fund)
    print(paste(journal,"Avg race fund",Fundprob_W))

    Revprob_W = opengender::class_mean(journal_fund)
    print(paste(journal,"Avg race rev",Revprob_W))


    # Fundprobs_R = journal_fund %>%  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal,"Avg race fund"))
    # print(Fundprobs_R)
    #
    #
    # Revprob_R = journal_rev %>%  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal,"Avg race rev"))
    # print(Revprob_R)

    tibble_row = tibble(journal_code = journal,
                        NumContRows = nrow(journal_cont),
                        NumFundRows = nrow(journal_fund),
                        NumRevRows = nrow(journal_rev),
                        Contprob_W = Contprob_W,
                        Fundprob_W = Fundprob_W,
                        Revprob_W = Revprob_W)

    probs_list[[length(probs_list)+1]] <- tibble_row
  }

  prob_W <- bind_rows(probs_list)
  save.image(img_file1)
}

  # }

  # stat('pr_whi')
if (file.exists(img_file2)){
  load(img_file2)
}else{
  probs_list2 <- list()
  for (journal in journal_codes) {
    # Filter datasets for the current journal
    journal_cont <- ContImput %>% filter(Journal_Code == journal)

    # Race distribution when peer reviewed
    Peer_Reviewed <- journal_cont %>% filter(Peer == TRUE)
    print(paste(journal,"amount papers rev:",nrow(Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))

    Peerprob_W <- opengender::class_mean(Peer_Reviewed['og_classification_pr_whi'])
    print(paste(journal, "Peer Reviewed:", Peerprob_W))

    # Race distribution when not peer reviewed
    Not_Peer_Reviewed <- journal_cont %>% filter(Peer == FALSE)
    print(paste(journal,"amount papers not rev:",nrow(Not_Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))
    NotPeerprob_W <- opengender::class_mean(Not_Peer_Reviewed['og_classification_pr_whi'])
    print(paste(journal, "Not Peer Reviewed:", NotPeerprob_W))

    # Race distribution when peer reviewed (first name listed)
    Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == TRUE) %>% distinct(DOI, .keep_all = TRUE)
    Peerprob_Fn_W <- opengender::class_mean(Peer_Reviewed_Fn['og_classification_pr_whi'])
    print(paste(journal, "Peer Reviewed First Name:", Peerprob_Fn_W))

    # Race distribution when not peer reviewed (first name listed)
    Not_Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == FALSE) %>% distinct(DOI, .keep_all = TRUE)
    NotPeerprob_Fn_W <- opengender::class_mean(Not_Peer_Reviewed_Fn['og_classification_pr_whi'])
    print(paste(journal, "Not Peer Reviewed First Name:", NotPeerprob_Fn_W))

    # Race distribution when data available
    DataAv <- journal_cont %>% filter(DataAv == TRUE)
    print(paste(journal,"amount papers data av:",nrow(DataAv %>% distinct(DOI, .keep_all = FALSE))))
    DataAvprob_W <- opengender::class_mean(DataAv['og_classification_pr_whi'])
    print(paste(journal, "Data Available:", DataAvprob_W))

    # Race distribution when data not available
    Not_DataAv <- journal_cont %>% filter(DataAv == FALSE)
    print(paste(journal,"amount papers not data av:",nrow(Not_DataAv %>% distinct(DOI, .keep_all = FALSE))))
    NotDataAvprob_W <- opengender::class_mean(Not_DataAv['og_classification_pr_whi'])
    print(paste(journal, "Data Not Available:", NotDataAvprob_W))

    # Race distribution when data available (first name listed)
    DataAvFn <- journal_cont %>% filter(DataAv == TRUE) %>% distinct(DOI, .keep_all = TRUE)
    DataAvFnProb_W <- opengender::class_mean(DataAvFn['og_classification_pr_whi'])
    print(paste(journal, "Data Available First Name:", DataAvFnProb_W))

    # Race distribution when data not available (first name listed)
    NotDataAvFn <- journal_cont %>% filter(DataAv == FALSE) %>% distinct(DOI, .keep_all = TRUE)
    NotDataAvFnProb_W <- opengender::class_mean(NotDataAvFn['og_classification_pr_whi'])
    print(paste(journal, "Data Not Available First Name:", NotDataAvFnProb_W))

    tibble_row = tibble(journal_code = journal,
                        NumPapersPeerR = nrow(Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE)),
                        NumPapersNotPeerR = nrow(Not_Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE)),
                        PRprob = Peerprob_W,
                        NotPRprob = NotPeerprob_W,
                        PRprobfn= Peerprob_Fn_W,
                        NotPRprobfn = NotPeerprob_Fn_W,
                        NumPapersDataAv = nrow(DataAv %>% distinct(DOI, .keep_all = FALSE)),
                        NumPapersNotDataAv = nrow(Not_DataAv %>% distinct(DOI, .keep_all = FALSE)),
                        DataAVprob = DataAvprob_W,
                        NotDataAVprob = NotDataAvprob_W,
                        DataAVprobfn= DataAvFnProb_W,
                        NotDataAVprobfn = NotDataAvFnProb_W,)


    probs_list2[[length(probs_list2)+1]] <- tibble_row


    # Peerprob_R <- Peer_Reviewed['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    #
    # print(paste(journal, "Peer Reviewed:"))
    # print(Peerprob_R)
    #
    # # Race distribution when not peer reviewed
    # Not_Peer_Reviewed <- journal_cont %>% filter(Peer == FALSE)
    # print(paste(journal,"amount papers not rev:",nrow(Not_Peer_Reviewed %>% distinct(DOI, .keep_all = FALSE))))
    #
    # NotPeerprob_R <- Not_Peer_Reviewed['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Not Peer Reviewed:"))
    # print(NotPeerprob_R)
    #
    # # Race distribution when peer reviewed (first name listed)
    # Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == TRUE) %>% distinct(DOI, .keep_all = TRUE)
    # Peerprob_Fn_F <- Peer_Reviewed_Fn['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Peer Reviewed First Name:"))
    # print(Peerprob_Fn_F)
    #
    # # Race distribution when not peer reviewed (first name listed)
    # Not_Peer_Reviewed_Fn <- journal_cont %>% filter(Peer == FALSE) %>% distinct(DOI, .keep_all = TRUE)
    # NotPeerprob_Fn_F <- Not_Peer_Reviewed_Fn['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Not Peer Reviewed First Name:"))
    # print(NotPeerprob_Fn_F)
    #
    # # Race distribution when data available
    # DataAv <- journal_cont %>% filter(DataAv == TRUE)
    # print(paste(journal,"amount papers data av:",nrow(DataAv %>% distinct(DOI, .keep_all = FALSE))))
    # DataAvprob_F <- DataAv['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Data Available:"))
    # print(DataAvprob_F)
    #
    # # Race distribution when data not available
    # Not_DataAv <- journal_cont %>% filter(DataAv == FALSE)
    # print(paste(journal,"amount papers not data av:",nrow(Not_DataAv %>% distinct(DOI, .keep_all = FALSE))))
    # NotDataAvprob_F <- Not_DataAv['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Data Not Available:"))
    # print(NotDataAvprob_F)
    #
    # # Race distribution when data available (first name listed)
    # DataAvFn <- journal_cont %>% filter(DataAv == TRUE) %>% distinct(DOI, .keep_all = TRUE)
    # DataAvFnProb_F <- DataAvFn['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Data Available First Name:"))
    # print(DataAvFnProb_F)
    #
    # # Race distribution when data not available (first name listed)
    # NotDataAvFn <- journal_cont %>% filter(DataAv == FALSE) %>% distinct(DOI, .keep_all = TRUE)
    # NotDataAvFnProb_F <- NotDataAvFn['og_classification_details'] %>%
    #   unnest(cols = c(og_classification_details))%>%
    #   summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    # print(paste(journal, "Data Not Available First Name:"))
    # print(NotDataAvFnProb_F)
  }
  prob_PRDAV <- bind_rows(probs_list2)
  save.image(img_file2)
}






  ### UNDONE BELOW THIS LINE ####
if (file.exists(img_file3)){
  load(img_file3)
}else{
probs_list3 <- list()
  for (journal in journal_codes) {
    # Filter datasets for the current journal
    journal_cont <- ContImput %>% filter(Journal_Code == journal)
    journal_rev <- RevImput %>% filter(Journal_Code == journal)
    journal_fund <- FundImput %>% filter(Journal_Code == journal)

    # FINDING RELATIONSHIPS
    concat_contrib <- journal_cont[, c(1, 14)] %>%
      group_by(DOI) %>%
      summarise(
        Avg_W_Percent = mean(og_classification_pr_whi, na.rm = TRUE),
      )

    concat_rev <- journal_rev[, c(1, 5)] %>%
      group_by(DOI) %>%
      summarise(
        Avg_W_Percent = mean(og_classification_pr_whi, na.rm = TRUE),
      )

    concat_fund <- journal_fund[, c(1, 5)] %>%
      group_by(DOI) %>%
      summarise(
        Avg_W_Percent = mean(og_classification_pr_whi, na.rm = TRUE),
      )

    # Only getting first instance
    concat_contrib2 <- journal_cont[, c(1, 14)] %>%
      distinct(DOI, .keep_all = TRUE)

    Contprob_W_first <- opengender::class_mean(concat_contrib2['og_classification_pr_whi'])
    print(paste(journal, "First Instance Race Probability Contributors:", Contprob_W_first))

    concat_rev2 <- journal_rev[, c(1, 5)] %>%
      distinct(DOI, .keep_all = TRUE)

    concat_fund2 <- journal_fund[, c(1, 5)] %>%
      distinct(DOI, .keep_all = TRUE)

    # Joining Contributor & Reviewer Data
    joined_data_cr <- inner_join(concat_contrib, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

    joined_data_cr2 <- inner_join(concat_contrib2, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

    # Compute Correlation

    if (nrow(joined_data_cr) > 1) {
      correlationcr <- cor(
        joined_data_cr$Avg_W_PercentContributors,
        joined_data_cr$Avg_W_PercentReviewers,
        use = "complete.obs"
      )
      print(paste(journal, "Correlation between White Percentages Contributors & Reviewers:", correlationcr))
    } else {
      correlationcr <- -2
      print(paste(journal, "Not enough data for correlation Contributors & Reviewers"))
    }

    if (nrow(joined_data_cr2) > 1) {
      correlationcr2 <- cor(
        joined_data_cr2$og_classification_pr_whi,
        joined_data_cr2$Avg_W_Percent,
        use = "complete.obs"
      )
      print(paste(journal, "Correlation between White Percentages fn CR2:", correlationcr2))
    } else {
      correlationcr2 = -2
      print(paste(journal, "Not enough data for correlation fn CR2"))
    }

    tibble_row = tibble(journal_code = journal,
                        ContRevCor = correlationcr,
                        ContRevCorFn = correlationcr2,
                        )


    probs_list3[[length(probs_list3)+1]] <- tibble_row

  }
Correlations <- bind_rows(probs_list3)

roles_tibble = ContImput %>% unnest(Role)

role_race_avg <- roles_tibble %>% group_by(.[[11]]) %>%
  summarise(Average_Race_Score = mean(og_classification_pr_whi, na.rm = TRUE), .groups = "drop")

saveRDS(role_race_avg, "./Data(rds files)/RolesByRace/roles_by_race_all.rds")


#Race by role
for (journal in journal_codes) {
  # Filter datasets for the current journal
  filename = paste("./Data(rds files)/RolesByRace/roles_by_race_",journal,".rds",sep = '')
  journal_cont <- ContImput %>% filter(Journal_Code == journal)
  roles_tibble = journal_cont %>% unnest(Role)
  role_race_avg <- roles_tibble %>% group_by(.[[11]]) %>%
    summarise(Average_Race_Score = mean(og_classification_pr_whi, na.rm = TRUE), .groups = "drop")
  saveRDS(role_race_avg, filename)
}


save.image(img_file3)
}


