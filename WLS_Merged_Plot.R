library(readr)
df <- read_csv("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project/merged_file.csv")

df$GPS_PR <- relevel(factor(df$GPS_PR), ref = "GPSLow_PRLow")

model <- glm(Depression_R6_binary ~ factor(GPS_PR) + factor(sex) + age_R5 + factor(Marital_status_R6_new) + Personal_Income_R6 + Education_years_R6, 
             data = df, 
             family = binomial(link = "logit"))


library(sjPlot)

# forest plot of OR

plot_model(model, 
           type = "est", 
           transform = "exp", 
           show.values = TRUE, 
           value.offset = 0.3) +
  ggtitle("Forest Plot of Odds Ratios")


# interaction plot

df$PR_group <- cut(df$PersonalResource,
                   breaks = quantile(df$PersonalResource, probs = c(0, 0.2, 0.8, 1), na.rm = TRUE),
                   labels = c("Low", "Mid", "High"),
                   include.lowest = TRUE)


model2 <- lm(Depression_score_R6 ~ PsychiatricGPS * PR_group +
               factor(sex) + age_R5 + factor(Marital_status_R6_new) + Personal_Income_R6 + Education_years_R6,
               data = df)

plot_model(model2, type = "pred", terms = c("PsychiatricGPS", "PR_group"))





