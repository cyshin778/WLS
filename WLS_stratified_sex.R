setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

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

# IntelligenceGPS_composite
df2$Intelligence_GPS <- rowMeans(df2[, c("IQ_GPS", "EA_GPS","CP_GPS")], na.rm = TRUE)

# PsychWellbeing_R5
df2$PsychWellbeing_R5 <- rowMeans(df2[, c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                                          'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5')], na.rm = TRUE)

# PersonalResource_R5
df2$PersonalResource_R5 <- rowMeans(df2[, c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                                            'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                                            'Optimism_R5', 'Mattering_R5')], na.rm = TRUE)

# SocialSupport1_R5
df2$SocialSupport1_R5 <- rowMeans(df2[, c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5')], na.rm = TRUE)

# SocialSupport2_R5
df2$SocialSupport2_R5 <- rowMeans(df2[, c('Love_social_R5','Listen_social_R5')], na.rm = TRUE)

# SocialParticipation1_R5
df2$SocialParticipation1_R5 <- rowMeans(df2[, c('Social_friends_R5', 'Social_relatives_R5')], na.rm = TRUE)



# Marital_status_R5
df2$Marital_status_R5 <- ifelse(df2$Marital_status_R5 == 1, 1, 0)



################################# Variables ################################################

# moderators (prs)
mod <- c('Three_GPS',
         'Two_GPS',
         'Intelligence_GPS',
         'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
         'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income1_R5', 'Education_years_R5')
cov_factor <- c('Marital_status_R5')
df2[cov_factor] <- lapply(df2[cov_factor], factor)
cov_ev <- paste0('EV', 1:10)

cov_list <- c(cov_cont, cov_factor, cov_ev)
COVARS <- paste(cov_list, collapse = " + ")

# Independent variables
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_R5','PsychWellbeing_R5')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_R5')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_R5')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_R5')
Social_involvement_R5 <- c('Social_involvement_R5')
indep <- c(PersonalResource, SocialSupport1, SocialSupport2,
           SocialParticipation1, Social_involvement_R5)


df_male <- df2[df2$sex == 1, ]
df_female <- df2[df2$sex == 2, ]

run_analysis <- function(data, sex_label) {
  
  # ------------------------- Main Effects -------------------------
  results_main <- list()
  
  for (indep_var in indep) {
    for (mod_var in mod) {
      formula_str <- paste(outcome, "~", indep_var, "+", mod_var, "+", COVARS)
      model <- lm(as.formula(formula_str), data = data)
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
  
  results_df <- bind_rows(results_main)
  
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
  
write.csv(results_df, paste0("Results/main_effect_results_", sex_label, ".csv"), row.names = FALSE)
  
# ------------------------- Moderation Effects -------------------------
results_list <- list()

for (indep_var in indep) {
  for (mod_var in mod) {
    interaction_term <- paste0(indep_var, ":", mod_var)
    formula_str <- paste(outcome, "~", indep_var, "*", mod_var, "+", COVARS)
    model <- lm(as.formula(formula_str), data = data)
    
    terms_to_extract <- c(indep_var, mod_var, interaction_term)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)
    available_terms <- intersect(terms_to_extract, rownames(coefs))
    
    res_df <- data.frame(
      Model = paste(indep_var, "x", mod_var),
      Independent = indep_var,
      Moderator = mod_var,
      Term = available_terms,
      Estimate = round(coefs[available_terms, "Estimate"], 4),
      Std_Error = round(coefs[available_terms, "Std. Error"], 4),
      CI_95_Low = round(confint_df[available_terms, 1], 4),
      CI_95_High = round(confint_df[available_terms, 2], 4),
      P_value = signif(coefs[available_terms, "Pr(>|t|)"], 4),
      stringsAsFactors = FALSE
    )
    
    results_list[[paste(indep_var, mod_var, sep = "_x_")]] <- res_df
  }
}

final_results <- bind_rows(results_list)

# FDR correction for interactions
final_results$P_FDR <- NA
final_results$FDR_sig <- ""

for (indep_var in unique(final_results$Independent)) {
  interaction_mask <- final_results$Independent == indep_var &
    final_results$Term == paste0(indep_var, ":", final_results$Moderator)
  
  fdr_pvals <- round(p.adjust(final_results$P_value[interaction_mask], method = "fdr"), 4)
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

write.csv(final_results, paste0("Results/moderation_results_", sex_label, ".csv"), row.names = FALSE)

# ------------------------- Forest Plot -------------------------
target_mod <- "Neuroticism_GPS"

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

interaction_data_sorted <- interaction_data %>%
  arrange(SignifLevel, desc(abs(Estimate))) %>%
  mutate(Independent = factor(Independent, levels = rev(unique(Independent))))

max_ci <- max(interaction_data_sorted$CI_95_High)
interaction_data_sorted$Significance_x <- max_ci + 0.005
interaction_data_sorted$Beta_CI_Label_x <- max_ci + 0.014

p <- ggplot(interaction_data_sorted, aes(x = Estimate, y = Independent)) +
  geom_point(shape = 15, size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = Significance_x, label = Significance), size = 3, hjust = 0) +
  geom_text(aes(x = Beta_CI_Label_x, label = Beta_CI_Label), size = 3, hjust = 0) +
  theme_minimal() +                # 먼저 theme_minimal() 적용
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = paste("Moderation Effects (", target_mod, ") - ", sex_label, sep = ""),
    x = "Beta Value of Interaction Term",
    y = "Independent Variable"
  ) +
  coord_cartesian(
    xlim = c(
      min(interaction_data_sorted$CI_95_Low) - 0.01,
      max_ci + 0.08
    )
  )


ggsave(paste0("Results/figures/forest_neuroticismGPS_", sex_label, ".pdf"), plot = p, width = 8, height = 6, units = "in")
}

run_analysis(df_male, "male")
run_analysis(df_female, "female")

