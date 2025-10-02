setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

## Note: covariates to R4

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_categorization_standardization.csv")
df2 <- df

# Three_GPS
df2$Three_GPS <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS", "MDD_GPS")], na.rm = TRUE)

# TWo_GPS 
df2$Two_GPS <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)

# Intelligence_GPS
df2$Intelligence_GPS <- rowMeans(df2[, c("IQ_GPS", "EA_GPS","CP_GPS")], na.rm = TRUE)


# SocialSupport1_R4
df2$SocialSupport1_R4 <- rowMeans(df2[, c('Support_money_R4', 'Support_problem_R4', 'Support_sick_R4')], na.rm = TRUE)

# SocialParticipation1_composite
df2$SocialParticipation1_R4 <- rowMeans(df2[, c('Social_friends_R4', 'Social_relatives_R4')], na.rm = TRUE)


# Marital_status_R4
df2$Marital_status_R4 <- ifelse(df2$Marital_status_R4 == 1, 1, 0)



################################# Variables ################################################

# moderators (prs)
mod <- c('Two_GPS',
         'Three_GPS',
         'Intelligence_GPS',
         'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
         'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6') ## R5, R6 선택 !!

# Covariates
cov_cont <- c('age_R4', 'Personal_Wage1_R4', 'College_years_R4')
cov_factor <- c('sex', 'Marital_status_R4')
df2[cov_factor] <- lapply(df2[cov_factor], factor)
cov_ev <- paste0('EV', 1:10)

cov_list <- c(cov_cont, cov_factor, cov_ev)
COVARS <- paste(cov_list, collapse = " + ")

# Independent variables
PsychWellbeing <- c('Autonomy_R4_z', 'EnvironmentalMastery_R4_z', 'PersonalGrowth_R4_z',
                      'PositiveRelationship_R4_z', 'SelfAcceptance_R4_z', 'PurposeinLife_R4_z','PsychWellbeing_R4')

SocialSupport1 <- c('Support_money_R4', 'Support_problem_R4', 'Support_sick_R4', 'SocialSupport1_R4')
SocialParticipation1 <- c('Social_friends_R4', 'Social_relatives_R4', 'SocialParticipation1_R4')
indep <- c(PsychWellbeing, SocialSupport1, SocialParticipation1)


# ------------------------- Main (Additive) Effects -------------------------

library(dplyr)
library(car)

results_main <- list()
vif_list <- list()
vif_summary <- list()

for (indep_var in indep) {
  for (mod_var in mod) {
    
    formula_str <- paste(outcome, "~", indep_var, "+", mod_var, "+", COVARS)
    
    model <- lm(as.formula(formula_str), data = df2)
    
    # vif_values <- vif(model)
    # vif_list[[paste(indep_var, mod_var, sep = "_x_")]] <- vif_values
    # 
    # vif_summary[[length(vif_summary) + 1]] <- data.frame(
    #   Independent = indep_var,
    #   Moderator = mod_var,
    #   Variable = names(vif_values),
    #   VIF = as.numeric(vif_values)
    # )
    
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)
    
    for (term in c(indep_var, mod_var)) {
      if (term %in% rownames(coefs)) {
        results_main[[length(results_main) + 1]] <- data.frame(
          Independent = indep_var,
          Moderator = mod_var,
          Term = term,
          Estimate = round(coefs[term, "Estimate"], 4),
          Std_Error = round(coefs[term, "Std. Error"], 4),
          CI_95_Low = round(confint_df[term, 1], 4),
          CI_95_High = round(confint_df[term, 2], 4),
          P_value = signif(coefs[term, "Pr(>|t|)"], 4)
        )
      }
    }
  }
}

# Bind and process results
results_df <- bind_rows(results_main)

# VIF summary
# vif_df <- bind_rows(vif_summary)
# filter(vif_df, VIF > 5)

# FDR correction per independent variable
results_df$P_FDR <- NA
results_df$FDR_sig <- ""

for (indep_var in unique(results_df$Independent)) {
  idx <- results_df$Independent == indep_var
  fdr_p <- p.adjust(results_df$P_value[idx], method = "fdr")
  results_df$P_FDR[idx] <- fdr_p
  
  results_df$FDR_sig[idx] <- sapply(fdr_p, function(pval) {
    if (is.na(pval)) return("")
    else if (pval < 0.001) return("***")
    else if (pval < 0.01) return("**")
    else if (pval < 0.05) return("*")
    else if (pval < 0.1) return(".")
    else return("")
  })
}


# Output

write.csv(results_df, "Results/R4_main_effect_results_depressionscoreR6_250829.csv", row.names = FALSE)


# ------------------------- Moderation Effects -------------------------

results_list <- list()
vif_list <- list()
vif_summary <- list()

for (indep_var in indep) {
  for (mod_var in mod) {
    
    interaction_term <- paste0(indep_var, ":", mod_var)
    formula_str <- paste(outcome, "~", indep_var, "*", mod_var, "+", COVARS)
    
    model <- lm(as.formula(formula_str), data = df2)
    
    # check multicollinearity
    # vif_values <- vif(model)
    # vif_list[[paste(indep_var, mod_var, sep = "_x_")]] <- vif_values
    # 
    # vif_summary[[length(vif_summary) + 1]] <- data.frame(
    #   Independent = indep_var,
    #   Moderator = mod_var,
    #   Variable = names(vif_values),
    #   VIF = as.numeric(vif_values)
    # )
    
    terms_to_extract <- c(indep_var, mod_var, interaction_term)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)  # Wald confidence intervals
    
    available_terms <- intersect(terms_to_extract, rownames(coefs))
    
    # Extract values
    beta <- round(coefs[available_terms, "Estimate"], 4)
    se <- round(coefs[available_terms, "Std. Error"], 4)
    p <- signif(coefs[available_terms, "Pr(>|t|)"], 4)
    lowci <- round(confint_df[available_terms, 1], 4)
    highci <- round(confint_df[available_terms, 2], 4)
    
    # Combine into dataframe
    res_df <- data.frame(
      Model = paste(indep_var, "x", mod_var),
      Independent = indep_var,
      Moderator = mod_var,
      Term = available_terms,
      Estimate = beta,
      Std_Error = se,
      CI_95_Low = lowci,
      CI_95_High = highci,
      P_value = p,
      stringsAsFactors = FALSE
    )
    
    results_list[[paste(indep_var, mod_var, sep = "_x_")]] <- res_df
  }
}

# Combine all results
final_results <- bind_rows(results_list)
# vif_df <- bind_rows(vif_summary)
# filter(vif_df, VIF > 5)

# FDR correction (interaction terms only)
final_results$P_FDR <- NA
final_results$FDR_sig <- ""

for (indep_var in unique(final_results$Independent)) {
  interaction_mask <- final_results$Independent == indep_var &
    final_results$Term == paste0(indep_var, ":", final_results$Moderator)
  
  # Get raw p-values for interaction terms
  pvals <- final_results$P_value[interaction_mask]
  
  # Apply FDR correction
  fdr_pvals <- round(p.adjust(pvals, method = "fdr"), 4)
  final_results$P_FDR[interaction_mask] <- fdr_pvals
  
  final_results$FDR_sig[interaction_mask] <- sapply(fdr_pvals, function(pval) {
    if (is.na(pval)) return("")
    else if (pval < 0.001) return("***")
    else if (pval < 0.01) return("**")
    else if (pval < 0.05) return("*")
    else if (pval < 0.1) return(".")
    else return("")
  })
}

write.csv(final_results, "Results/R4_moderation_results_depressionscoreR6_250829.csv", row.names = FALSE)


# ------------------------- Forest Plot -------------------------


library(dplyr)
library(ggplot2)
library(ggeffects)

target_mod <- "Neuroticism_GPS"

# 1. Filter interaction terms
interaction_data <- final_results %>%
  filter(Moderator == target_mod,
         Term == paste0(Independent, ":", Moderator)) %>%
  mutate(
    Significance = cut(
      P_FDR,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", "")
    ),
    SignifLevel = as.numeric(factor(Significance, levels = c("***", "**", "*", ".", ""), labels = c(1, 2, 3, 4, 5))),
    Beta_CI_Label = sprintf("%.3f [%.3f, %.3f]", Estimate, CI_95_Low, CI_95_High)
  )


# 2. Sort by significance and effect size
interaction_data_sorted <- interaction_data %>%
  arrange(SignifLevel, desc(abs(Estimate))) %>%
  mutate(Independent = factor(Independent, levels = rev(unique(Independent))))

max_ci <- max(interaction_data_sorted$CI_95_High)

# 라벨 위치: 전체 max CI_95_High + 여유 공간
interaction_data_sorted$Significance_x <- max_ci + 0.005
interaction_data_sorted$Beta_CI_Label_x <- max_ci + 0.010


# 3. Plot
p <- ggplot(interaction_data_sorted, aes(x = Estimate, y = Independent)) +
  geom_point(shape = 15, size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = Significance_x, label = Significance), size = 3, hjust = 0) +
  geom_text(aes(x = Beta_CI_Label_x, label = Beta_CI_Label), size = 3, hjust = 0) +
  theme_minimal() +
  labs(
    title = paste("Moderation Effects (", target_mod, ")", sep = ""),
    subtitle = "FDR-corrected: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
    x = "Beta Value of Interaction Term",
    y = "Independent Variable"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(
    xlim = c(
      min(interaction_data_sorted$CI_95_Low) - 0.01,
      max_ci + 0.05  # 오른쪽 공간 확보
    )
  )


ggsave("Results/figures/forest_R4_neuroticismGPS_depressionscoreR6.pdf", plot = p, width = 8, height = 5, units = "in")