df <- read.csv("wls_merged.csv")

df2 <- df

# 그룹화

quantiles1 <- quantile(df2$PsychiatricGPS, probs = c(0.10, 0.90), na.rm = TRUE)
quantiles2 <- quantile(df2$Childhood_ELS_R5, probs = c(0.10, 0.90), na.rm = TRUE)
quantiles3 <- quantile(df2$IntelligenceGPS, probs = c(0.10, 0.90), na.rm = TRUE)

# 그룹 나누기
df2$PsychiatricGPS_group <- cut(df2$PsychiatricGPS,
                                breaks = c(-Inf, quantiles1[1], quantiles1[2], Inf),
                                labels = c("low", "middle", "high"),
                                right = TRUE)

df2$Childhood_ELS_R5_group <- cut(df2$Childhood_ELS_R5,
                                  breaks = c(-Inf, quantiles2[1], quantiles2[2], Inf),
                                  labels = c("low", "middle", "high"),
                                  right = TRUE)

df2$IntelligenceGPS_group <- cut(df2$IntelligenceGPS,
                                breaks = c(-Inf, quantiles3[1], quantiles3[2], Inf),
                                labels = c("low", "middle", "high"),
                                right = TRUE)

model1 <- lm(Depression_score_R6 ~ PersonalResource * PsychiatricGPS_group + age_R5 + factor(Marital_status_R6_new) + factor(sex) + 
              Personal_Income_R6 + Education_years_R6, data = df2)

model2 <- lm(Depression_score_R6 ~ PersonalResource * Childhood_ELS_R5_group + age_R5 + factor(Marital_status_R6_new) + factor(sex) + 
               Personal_Income_R6 + Education_years_R6, data = df2)

model3 <- lm(Depression_score_R6 ~ PersonalResource * IntelligenceGPS_group + age_R5 + factor(Marital_status_R6_new) + factor(sex) + 
               Personal_Income_R6 + Education_years_R6, data = df2)


# Plot1 - plot with raw data 

library(ggplot2)

ggplot(df2, aes(x = PersonalResource, y = Depression_score_R6, color = PsychiatricGPS_group, linetype = PsychiatricGPS_group)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_manual(values = c("green", "blue", "red")) +
  labs(
    x = "Personal Resource",
    y = "Depression Score (R6)",
    color = "Psychiatric PRS Group",
    linetype = "Psychiatric PRS Group"
  ) +
  theme_minimal(base_size = 14)


# Plot2 - plot predicted values & marginal effect of the model

library(ggeffects)
library(ggplot2)

# X: prs 
predict <- ggpredict(model1, terms = c("PersonalResource","PsychiatricGPS_group"))

ggplot(predict, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("green", "blue", "red")) +
  scale_fill_manual(values = c("green", "blue", "red")) +
  labs(
    x = "Personal Resource",
    y = "Predicted Depression Score (R6)",
    color = "Psychiatric PRS",
    fill = "Psychiatric PRS",
    linetype = "Psychiatric PRS"
  ) +
  theme_minimal(base_size = 14)



# X: Childhood_ELS_R5
predict2 <- ggpredict(model2, terms = c("PersonalResource", "Childhood_ELS_R5_group"))

ggplot(predict2, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("green", "blue", "red")) +
  scale_fill_manual(values = c("green", "blue", "red")) +
  labs(
    x = "Personal Resource",
    y = "Predicted Depression Score (R6)",
    color = "Childhood ELS",
    fill = "Childhood ELS",
    linetype = "Childhood ELS"
  ) +
  theme_minimal(base_size = 14)


# X: Intelligence GPS

predict3 <- ggpredict(model3, terms = c("PersonalResource","IntelligenceGPS_group"))

ggplot(predict3, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("green", "blue", "red")) +
  scale_fill_manual(values = c("green", "blue", "red")) +
  labs(
    x = "Personal Resource",
    y = "Predicted Depression Score (R6)",
    color = "Intelligence PRS",
    fill = "Intelligence PRS",
    linetype = "Intelligence PRS"
  ) +
  theme_minimal(base_size = 14)





# two-way ANOVA
anova_model <- aov(data = df2, Depression_score_R6 ~ PsychiatricGPS_group * PR_group +
               age_R5 + factor(Marital_status_R6_new) + factor(sex) +
               Personal_Income_R6 + Education_years_R6)
summary(anova_model)

# Interaction Plot

interaction.plot(x.factor = df2$PR_group,
                 trace.factor = df2$PsychiatricGPS_group,
                 response = df2$Depression_score_R6,
                 fun = mean,
                 type = 'b',
                 xlab = 'Level of Personal Resource',
                 ylab = 'Depression score R6',
                 trace.label = 'Level of Psychiatric GPS',
                 col = c("green","blue","red"),
                 pch = c(16,16,16))
