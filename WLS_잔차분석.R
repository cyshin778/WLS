setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

df <- read.csv("wls_merged_250620.csv")

df2 <- df
df2$sex <- as.factor(df2$sex)
df2$Marital_status_R6_new <- as.factor(df2$Marital_status_R6_new)


eq1 <- lm("Depression_score_R6 ~ PsychiatricGPS_composite2 * PersonalResource_composite + sex + age_R5 + Marital_status_R6_new +
          Personal_Income_R6 + Education_years_R6 + EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV9 + EV10", data = df2)

layout(matrix(c(1,2,3,4),2,2))
plot(eq1)