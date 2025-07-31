setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")


df <- read.csv("wls_merged_250620.csv")

df2 <- df
df2$sex <- as.factor(df2$sex)
df2$Marital_status_R6_new <- as.factor(df2$Marital_status_R6_new)


library(mediation)

# mediator 리스트
mediators <- c('Autonomy_R5',
               'EnvironmentalMastery_R5',
               'PersonalGrowth_R5',
               'PositiveRelationship_R5',
               'SelfAcceptance_R5',
               'PurposeinLife_R5',
               'Optimism_R5',
               'Mattering_R5',
               'PersonalResource_composite',
               'Support_money_R5', 'Support_problem_R5', 'Support_sick_R5','SocialSupport_composite',
               'Social_friends_R5', 'Social_relatives_R5','SocialParticipation1_composite',
               'Social_involvement_R5','SocialParticipation2_composite')

# 결과 저장용 빈 리스트
results_list <- list()

# 반복 분석
set.seed(1234)  # reproducibility
for (med in mediators) {
  
  # mediator 모델
  med_model <- lm(as.formula(
    paste0(med, " ~ PscychiatricGPS_composite2 + sex + Marital_status_R6_new + ",
           "Personal_Income_R6 + Education_years_R6 + ",
           "EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV9 + EV10")),
    data = df2)
  
  # outcome 모델
  out_model <- lm(as.formula(
    paste0("Depression_score_R6 ~ PscychiatricGPS_composite2 + ", med, 
           " + sex + Marital_status_R6_new + ",
           "Personal_Income_R6 + Education_years_R6 + ",
           "EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV9 + EV10")),
    data = df2)
  
  # mediation 분석
  med_out <- mediate(med_model, out_model,
                     treat = "PscychiatricGPS_composite2",
                     mediator = med,
                     boot = TRUE, sims = 1000)
  
  # 결과 저장
  results_list[[med]] <- data.frame(
    Mediator = med,
    ACME = med_out$d0,
    ACME_p = med_out$d0.p,
    ADE = med_out$z0,
    ADE_p = med_out$z0.p,
    Total_Effect = med_out$tau.coef,
    Total_p = med_out$tau.p,
    Prop_Mediated = med_out$n0,
    Prop_Mediated_p = med_out$n0.p
  )
}

# 결과 합치기
mediation_results <- do.call(rbind, results_list)

# 보기 좋게 정렬 (간접효과 p-value 기준)
mediation_results <- mediation_results[order(mediation_results$ACME_p), ]

# FDR correction
mediation_results$ACME_p_fdr <- p.adjust(mediation_results$ACME_p, method = "fdr")
mediation_results$ADE_p_fdr <- p.adjust(mediation_results$ADE_p, method = "fdr")
mediation_results$Prop_Mediated_p_fdr <- p.adjust(mediation_results$Prop_Mediated_p, method = "fdr")


# 결과 출력
print(mediation_results)
