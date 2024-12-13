library(tidyverse)
library(xml2)
library(docstring)
library(rplos)
library(opengender)
library(stringr)
library(humaniformat)


#Average Gender ratio
#NOTES:
#used mean may want to cut off male/female at the .5 threshold for prediction condtitional on gender

#Probability of a funder being female
FundImput = readRDS("./Data(rds files)/FundingWithImputation2.rds")
Fundprob_F = opengender::gender_mean(FundImput['og_pr_F'])
Fundprob_F

#Probability of a reviewer being female
RevImput = readRDS("./Data(rds files)/ReviewersWithImputation2.rds")
Revprob_F = opengender::gender_mean(RevImput['og_pr_F'])
Revprob_F

#Overall probability of a contributor being female
ContImput = readRDS("./Data(rds files)/Data_with_Imputation/ContribWithImputation1.rds")
Contprob_F = opengender::gender_mean(ContImput['og_pr_F'])
Contprob_F

#Gender distribution when peer reviewed
Peer_Reviewed = ContImput %>% filter(Peer == TRUE)
Peerprob_F = opengender::gender_mean(Peer_Reviewed['og_pr_F'])
Peerprob_F

#Gender distribution when not peer reviewed
Not_Peer_Reviewed = ContImput %>% filter(Peer == FALSE)
NotPeerprob_F = opengender::gender_mean(Not_Peer_Reviewed['og_pr_F'])
Not_Peer_Reviewed

#Gender distribution when data available
DataAv = ContImput %>% filter(DataAv == TRUE)
DataAvprob_F = opengender::gender_mean(DataAv['og_pr_F'])
DataAvprob_F

#Gender distribution when data not available
Not_DataAv = ContImput %>% filter(DataAv == FALSE)
NotDataAvprob_F = opengender::gender_mean(Not_DataAv['og_pr_F'])
NotDataAvprob_F

Authors = ContImput %>% filter(Contrib_type == 'author')

#ImportedData2 = readRDS("./Data(rds files)/PlosData/Reviewer_names.rds")

#FINDING RELATIONSHIPS
concat_contrib = ContImput[, c(1, 9, 14)] %>%
  group_by(DOI) %>%
  summarise(
    Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
  )

concat_rev= RevImput[, c(1, 6)] %>%
  group_by(DOI) %>%
  summarise(
    Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
  )

joined_data <- inner_join(concat_contrib, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

correlation <- cor(
  joined_data$Avg_F_PercentContributors,
  joined_data$Avg_F_PercentReviewers,
  use = "complete.obs"
)
print(paste("Correlation between Female percentages:", correlation))

# Visualize relationships
# Scatter plot
plot(
  joined_data$Avg_F_PercentContributors,
  joined_data$Avg_F_PercentReviewers,
  main = "Contributors vs Reviewers (Average Female Percentages)",
  xlab = "Contributors",
  ylab = "Reviewers",
  pch = 16
)














