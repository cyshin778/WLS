setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")


df <- read.csv("wls_merged_250620.csv")
df2 <- df


# additive effects (main effects)
# Moderation analysis에서 X -> Y, M -> Y 사이에 인과관계 성립해야하기 때문에 X,M 각각의 main effect 확인

PersonalResource <- c('Autonomy_R5',
                      'EnvironmentalMastery_R5',
                      'PersonalGrowth_R5',
                      'PositiveRelationship_R5',
                      'SelfAcceptance_R5',
                      'PurposeinLife_R5',
                      'Optimism_R5',
                      'Mattering_R5',
                      'PersonalResource_composite')

SocialSupport <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5','SocialSupport_composite')

SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5','SocialParticipation1_composite')

SocialParticipation2 <- c('Social_involvement_R5','SocialParticipation2_composite')

moderators <- c(PersonalResource, SocialSupport, SocialParticipation1, SocialParticipation2)

# 필요한 패키지
library(dplyr)
library(stats)

# 결과를 저장할 리스트
results <- list()

# 반복문
for (mod in moderators) {
  # 회귀식 생성
  formula <- as.formula(paste0(
    "Depression_score_R6 ~ IntelligenceGPS_composite + ", mod,
    " + factor(sex) + age_R5 + factor(Marital_status_R6_new) + ",
    "Personal_Income_R6 + Education_years_R6 + ",
    paste0("EV", 1:10, collapse = " + ")
  ))
  
  # 선형 회귀 모델
  model <- lm(formula, data = df2)
  coefs <- summary(model)$coefficients
  
  # 관심 변수만 추출
  for (term in c("IntelligenceGPS_composite", mod)) {
    if (term %in% rownames(coefs)) {
      results[[length(results) + 1]] <- data.frame(
        term = term,
        moderator = mod,
        estimate = coefs[term, "Estimate"],
        std_error = coefs[term, "Std. Error"],
        p_value = coefs[term, "Pr(>|t|)"]
      )
    }
  }
}

# 리스트를 데이터프레임으로 변환
results_df <- bind_rows(results)

# FDR 보정
results_df$p_fdr <- p.adjust(results_df$p_value, method = "fdr")

results_df$Sig <- cut(
  results_df$p_fdr,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)


# 결과 확인
print(results_df)











