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
  ContImput <- ContImputraw %>% filter(!str_detect(DOI, "annotation")) %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
FundImput = readRDS("./Data(rds files)/FundingWithImputation2.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
RevImput = readRDS("./Data(rds files)/ReviewersWithImputation2.rds") %>% mutate(Journal_Code = str_extract(DOI, "(?<=journal\\.)[a-zA-Z]{4}"))
journal_codes <- ContImput %>% pull(Journal_Code) %>% unique()




#Average Gender ratio

#Overall probability of a contributor being female
Contprob_F = opengender::gender_mean(ContImput['og_pr_F'])
Contprob_F

#by Journal

#Probability of a fundee being female
Fundprob_F = opengender::gender_mean(FundImput['og_pr_F'])
Fundprob_F

#Probability of a reviewer being female
Revprob_F = opengender::gender_mean(RevImput['og_pr_F'])
Revprob_F


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



#Gender distribution when data available first name listed
DataAvFn = ContImput %>% filter(DataAv == TRUE) %>% distinct(DOI, .keep_all = TRUE)
DataAvFnProb_F = opengender::gender_mean(DataAv2['og_pr_F'])
DataAvFnProb_F

#Gender distribution when data not available first name listed
NotDataAvFn = ContImput %>% filter(DataAv == FALSE) %>% distinct(DOI, .keep_all = TRUE)
NotDataAvFnProb_F = opengender::gender_mean(Not_DataAv2['og_pr_F'])
NotDataAvFnProb_F


# Looking by Authors vs Editors
Authors = ContImput %>% filter(Contrib_type == 'author')
Editors = ContImput %>% filter(Contrib_type == 'editor')

AuthorsProb_F = opengender::gender_mean(Authors['og_pr_F'])
AuthorsProb_F

EditorsProb_F = opengender::gender_mean(Editors['og_pr_F'])
EditorsProb_F

# first names
AuthorsFn = Authors %>% distinct(DOI, .keep_all = TRUE)
EditorsFn  = Editors %>% distinct(DOI, .keep_all = TRUE)

AuthorsFnProb_F = opengender::gender_mean(Authors_PF2['og_pr_F'])
AuthorsFnProb_F

EditorsFnProb_F = opengender::gender_mean(Editors_PF2['og_pr_F'])
EditorsFnProb_F

# ImportedData2 = readRDS("./Data(rds files)/PlosData/Reviewer_names.rds")

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

concat_fund= FundImput[, c(1, 5)] %>%
  group_by(DOI) %>%
  summarise(
    Avg_F_Percent = mean(og_pr_F, na.rm = TRUE),
  )

# Only getting first instance
concat_contrib2 = ContImput[, c(1, 9, 14)] %>%
  distinct(DOI, .keep_all = TRUE)


Contprob_F_first = opengender::gender_mean(concat_contrib2['og_pr_F'])
Contprob_F_first

concat_rev2 = RevImput[, c(1, 6)] %>%
  distinct(DOI, .keep_all = TRUE)



concat_fund2 = FundImput[, c(1, 5)] %>%
  distinct(DOI, .keep_all = TRUE)


joined_data_cr <- inner_join(concat_contrib, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

# joined_data_cf <- inner_join(concat_contrib, concat_fund, by = "DOI", suffix = c("Contributors", "Funder"))

joined_data_cr2 <- inner_join(concat_contrib2, concat_rev, by = "DOI", suffix = c("Contributors", "Reviewers"))

# joined_data_cf2 <- inner_join(concat_contrib2, concat_fund, by = "DOI", suffix = c("Contributors", "Funder"))

correlationcr <- cor(
  joined_data_cr$Avg_F_PercentContributors,
  joined_data_cr$Avg_F_PercentReviewers,
  use = "complete.obs"
)

# correlationcf <- cor(
#   joined_data_cf$Avg_F_PercentContributors,
#   joined_data_cf$Avg_F_PercentFunder,
#   use = "complete.obs"
# )

print(paste("Correlation between Female percentages:", correlationcr))

# print(paste("Correlation between Female percentages:", correlationcf))

correlationcr2 <- cor(
  joined_data_cr2$og_pr_F,
  joined_data_cr2$Avg_F_Percent,
  use = "complete.obs"
)
# correlationcr2 <- cor(
#   joined_data_cr2$og_pr_FContributors,
#   joined_data_cr2$og_pr_FReviewers,
#   use = "complete.obs"
# )

# correlationcf2 <- cor(
#   joined_data_cf2$og_pr_FContributors,
#   joined_data_cf2$og_pr_FFunder,
#   use = "complete.obs"
# )

print(paste("Correlation between Female percentages cr2:", correlationcr2))
# print(paste("Correlation between Female percentages cf2:", correlationcf2))


# Visualize relationships
# Scatter plot
# plot(
#   joined_data$Avg_F_PercentContributors,
#   joined_data$Avg_F_PercentReviewers,
#   main = "Contributors vs Reviewers (Average Female Percentages)",
#   xlab = "Contributors",
#   ylab = "Reviewers",
#   pch = 16
# )


##PLOTS FOR CONTRIBUTOR VS REVIEWER COUNTS

ggplot(joined_data_cr, aes(x = Avg_F_PercentContributors, y = Avg_F_PercentReviewers)) +
  # geom_point(alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01, alpha = 0.5)+
  labs(title = "Contributors vs Reviewers (Average Female Percentages)", x = "Contributors", y = "Reviewers") +
  theme_minimal()

ggplot(joined_data_cr, aes(x = Avg_F_PercentContributors, y = Avg_F_PercentReviewers)) +
  # geom_point(alpha = 0.5) +
  geom_bin2d(bins = 3) + # Adjust 'bins' to control the number of bins
  # scale_fill_gradient(low = "blue", high = "pink")
  labs(title = "Contributors vs Reviewers (Average Female Percentages)", x = "Contributors", y = "Reviewers") +
  theme_minimal()


ggplot(joined_data_cr2, aes(x = og_pr_FContributors, y = og_pr_FReviewers)) +
  # geom_point(alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01, alpha = 0.5)+
  labs(title = "Contributors vs Reviewers (Average Female Percentages)", x = "Contributors", y = "Reviewers") +
  theme_minimal()

ggplot(joined_data_cr2, aes(x = og_pr_FContributors, y = og_pr_FReviewers)) +
  # geom_point(alpha = 0.5) +
  geom_bin2d(bins = 3) + # Adjust 'bins' to control the number of bins
  # scale_fill_gradient(low = "blue", high = "pink")
  labs(title = "Contributors vs Reviewers (Average Female Percentages)", x = "Contributors", y = "Reviewers") +
  theme_minimal()

##PLOTS FOR CONTRIBUTOR VS FUNDING COUNTS


# ggplot(joined_data_cf, aes(x = Avg_F_PercentContributors, y = Avg_F_PercentFunder)) +
#   geom_point(alpha = 0.5) +
#   # geom_jitter(width = 0.01, height = 0.01, alpha = 0.5)+
#   labs(title = "Contributors vs Funders (Average Female Percentages)", x = "Contributors", y = "Funders") +
#   theme_minimal()
#
# ggplot(joined_data_cf2, aes(x = og_pr_FContributors, y = og_pr_FFunder)) +
#   geom_point(alpha = 0.5) +
#   # geom_jitter(width = 0.01, height = 0.01, alpha = 0.5)+
#   labs(title = "Contributors vs Funders (Average Female Percentages)", x = "Contributors", y = "Funders") +
#   theme_minimal()

##SEEING IF a article had funding info and going by gender

funded_info <- concat_contrib %>%
  mutate(present = if_else(DOI %in% concat_fund$DOI, TRUE, FALSE))


# ggplot(funded_info, aes(x = present)) +
#   geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
#   labs(title = "Histogram", x = "Values", y = "Frequency") +
#   theme_minimal()

ggplot(funded_info, aes(x = Avg_F_Percent, y = 0, color = present)) +
  # geom_point(size = .1,alpha = .5) + # Adjust point size
  geom_jitter(width = 0, height = 10, alpha = 0.5)+
  scale_color_manual(values = c("red", "green"), labels = c("Not Present", "Present")) +
  labs(title = "Scatter Plot of Names with Presence and Value",
       x = 'gender',
       y = NULL,
       color = "Presence in Second Tibble") +
  theme_minimal()





