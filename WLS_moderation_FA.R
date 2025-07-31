library(readr)
df <- read.csv("C:/Users/ChaeYoon Shin/OneDrive/Desktop/GENNIE Lab/wls_phenotype_data.csv", header=TRUE)
df$Neuroticism_GPS_scaled <- scale(df$Neuroticism_GPS)
df$Depression_GPS_scaled <- scale(df$Depression_GPS)
df$MDD_GPS_scaled <- scale(df$MDD_GPS)
df$Anxiety_GPS_scaled <- scale(df$Anxiety_GPS)

## EFA

library(tidyr)
library(dplyr)


caregive_long <- df %>%
  select(c(
    "Caregiving_give_weeks_R4", "Caregiving_give_hours_R4",
    "Caregiving_give_transport_R4", "Caregiving_give_transport_R5",
    "Caregiving_give_housework_R4","Caregiving_give_housework_R5",
    "Caregiving_give_advice_R4","Caregiving_give_advice_R5",
    "Caregiving_give_childcare_R4","Caregiving_give_childcare_R5",
    "Volunteer_other_num_R5", "Volunteer_hours_R5", "Volunteer_satisfied_R5",
    "Caregiving_get_weeks_R4","Caregiving_get_hours_R4",
    "Caregiving_get_transport_R4", "Caregiving_get_transport_R5",
    "Caregiving_get_housework_R4","Caregiving_get_housework_R5",
    "Caregiving_get_advice_R4","Caregiving_get_advice_R5",
    "Caregiving_get_childcare_R4","Caregiving_get_computer_R5",
    "Spouse_Caregiving_get_weeks_R4", "Spouse_Caregiving_get_hours_R4",
    "Support_money_R4","Support_money_R5",
    "Support_problem_R4","Support_problem_R5",
    "Support_sick_R4","Support_sick_R5",
    "Support_computer_R5"
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


bartlett.test(value ~ variable, data = caregive_long)






library(psych)

caregive <- df[, c(
                 "Caregiving_give_weeks_R4", "Caregiving_give_hours_R4",
                 "Caregiving_give_transport_R4", "Caregiving_give_transport_R5",
                 "Caregiving_give_housework_R4","Caregiving_give_housework_R5",
                 "Caregiving_give_advice_R4","Caregiving_give_advice_R5",
                 "Caregiving_give_childcare_R4","Caregiving_give_childcare_R5",
             "Volunteer_other_num_R5", "Volunteer_hours_R5", "Volunteer_satisfied_R5",
             "Caregiving_get_weeks_R4","Caregiving_get_hours_R4",
             "Caregiving_get_transport_R4", "Caregiving_get_transport_R5",
             "Caregiving_get_housework_R4","Caregiving_get_housework_R5",
             "Caregiving_get_advice_R4","Caregiving_get_advice_R5",
             "Caregiving_get_childcare_R4","Caregiving_get_computer_R5",
             "Spouse_Caregiving_get_weeks_R4", "Spouse_Caregiving_get_hours_R4",
             "Support_money_R4","Support_money_R5",
             "Support_problem_R4","Support_problem_R5",
             "Support_sick_R4","Support_sick_R5",
             "Support_computer_R5")]



bartlett.test(caregive)


# Bartlett's Test of Sphericity


# Kaiser-Meyer-Olkin (KMO) Test
KMO(caregive)
             
             
             
careget <- df[, c("Caregiving_get_weeks_R4","Caregiving_get_hours_R4",
             "Caregiving_get_transport_R4", "Caregiving_get_transport_R5","Caregiving_get_transport_R6",
             "Caregiving_get_housework_R4","Caregiving_get_housework_R5","Caregiving_get_housework_R6",
             "Caregiving_get_advice_R4","Caregiving_get_advice_R5","Caregiving_get_advice_R6",
             "Caregiving_get_childcare_R4","Caregiving_get_computer_R5","Caregiving_get_computer_R6",
             "Spouse_Caregiving_get_weeks_R4", "Spouse_Caregiving_get_hours_R4",
             "Support_money_R4","Support_money_R5","Support_money_R6",
             "Support_problem_R4","Support_problem_R5","Support_problem_R6",
             "Support_sick_R4","Support_sick_R5","Support_sick_R6",
             "Support_computer_R5","Support_computer_R6")]

bartlett.test(careget)






fa.parallel(care, fm="ml", fa="fa",
            n.iter=100)

library(nFactors)
nScree(caregiveget)

eigen(cor(caregiveget))library(readr)



## CFA
library(lavaan)

# PRS

cfa_prs <- "PRS =~ Depression_GPS_scaled + Neuroticism_GPS_scaled + MDD_GPS_scaled"
fit_prs <- cfa(model=cfa_prs, data=df)
summary(fit_prs, fit.measures=TRUE, standardized=TRUE)



set.seed(123)  # 재현성을 위해 설정
n <- nrow(df)
train_index <- sample(1:n, size = round(0.7 * n))  # 70% 훈련 데이터
train_df <- df[train_index, ]
test_df <- df[-train_index, ]

# 훈련 데이터로 모델 적합
fit_train <- cfa(model = cfa_prs, data = train_df)

# 테스트 데이터에서 적합도 확인
fit_test <- cfa(model = cfa_prs, data = test_df)

# 적합도 지표 비교
summary(fit_train, fit.measures = TRUE)
summary(fit_test, fit.measures = TRUE)



# 조절변수
cfa <- "caregiving =~ Caregiving_give_weeks_R4 + Caregiving_give_hours_R4 +
Caregiving_give_transport_R4 + Caregiving_give_transport_R5 +
Caregiving_give_housework_R4 + Caregiving_give_housework_R5 +
Caregiving_give_advice_R4 + Caregiving_give_advice_R5 +
Caregiving_give_childcare_R4 + Caregiving_give_childcare_R5"

fit <- cfa(model=cfa, data=df)

summary(fit, fit.measures=TRUE, standardized=TRUE)


# 모수 추정치
parameterEstimates(fit, standardized = TRUE)
standardizedsolution(fit)

coef(fit)

# 상관계수 잔차행렬
residuals(fit, type="cor")$cov

# 적합도 지표
fitMeasures(fit)
names(fitMeasures(fit))
fitmeasures(fit, c("chisq","df","pvalue","gfi","rmsea","cfi"))
# 모든 지표들이 권장수준을 충족시키지 못함..

# 수정 지표와 모델 개선
summary(fit, modindices=TRUE)
modindices(fit)

modindices(fit, sort.=TRUE, minimum.value=3)
