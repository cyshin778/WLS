# Dataframe for GSCA

setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(writexl)

df <- read.csv("wls_merged_250716.csv")

df$sex <- ifelse(df$sex == 2, 1, 0)

new_df <- df[, c('sex','Marital_status_R6_new','Depression_score_R6',
                 'age_R5','EV1','EV2','EV3','EV4','EV5','EV6','EV7','EV8','EV9','EV10',
                 'Personal_Income_R6', 'Education_years_R6',
                 'Depression_GPS','MDD_GPS', 'Neuroticism_GPS',
                 'EA_GPS','IQ_GPS','CP_GPS','Autonomy_R5',
                 'EnvironmentalMastery_R5',
                 'PersonalGrowth_R5',
                 'PositiveRelationship_R5',
                 'SelfAcceptance_R5',
                 'PurposeinLife_R5',
                 'Optimism_R5',
                 'Mattering_R5',
                 'Support_money_R5', 'Support_problem_R5', 'Support_sick_R5',
                 'Love_social_R5','Listen_social_R5',
                 'Social_friends_R5', 'Social_relatives_R5',
                 'Social_church1_R5',
                 'Social_church2_R5',
                 'Social_veteran_R5',
                 'Social_fraternal_R5',
                 'Social_business_R5',
                 'Social_pta_R5',
                 'Social_community_R5',
                 'Social_nationality_R5',
                 'Social_sport_R5',
                 'Social_countryclub_R5',
                 'Social_youth_R5',
                 'Social_profesional_R5',
                 'Social_political_R5',
                 'Social_neighborhood_R5',
                 'Social_charity_R5',
                 'Social_hobby_R5',
                 'Social_involvement_R5')]

write_xlsx(new_df, path = "C:/Users/chaeyoon/OneDrive/Desktop/GSCA Pro Windows 1.2.1.0/project/newdf_250716.xlsx")
