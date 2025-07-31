setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250620.csv")
df2 <- df


library(corrplot)
prs_vars <- df2[, c("IQ_GPS", "CP_GPS", "EA_GPS")]
cor_matrix <- cor(prs_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

################################# Variables ################################################

# Main Variables
indep <- c('PsychiatricGPS_composite2', 'PsychiatricGPS_composite1',
           'IntelligenceGPS_composite', 'Childhood_ELS_R5',
           'PTSD_GPS','ASP_GPS','Depression_GPS',
           'Insomnia_GPS', 'MDD_GPS', 'Neuroticism_GPS',
           'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)

cov_list <- c(cov_cont, cov_factor)
COVARS <- paste(cov_list, collapse = " + ")

# Moderators
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
SocialParticipation2 <- c('Social_involvement_R5', 'SocialParticipation2_composite')
moderators <- c(PersonalResource, SocialSupport, SocialParticipation1, SocialParticipation2)


################################# Main(Additive) Effects ################################################

library(dplyr)

results_main <- list()

for (indep_var in indep) {
  for (mod in moderators) {
    
    formula_str <- paste(
      outcome, "~", indep_var, "+", mod, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
    )
    
    model <- lm(as.formula(formula_str), data = df2)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)
    
    for (term in c(indep_var, mod)) {
      if (term %in% rownames(coefs)) {
        results_main[[length(results_main) + 1]] <- data.frame(
          Independent = indep_var,
          Moderator = mod,
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

# FDR correction per independent variable (for both indep & mod terms)
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
print(results_df)
write.csv(results_df, "main_effect_results.csv", row.names = FALSE)



################################# Moderation ################################################

results_list <- list()

for (indep_var in indep) {
  for (mod_var in moderators) {
    
    interaction_term <- paste0(indep_var, ":", mod_var)
    formula_str <- paste(
      outcome, "~", indep_var, "*", mod_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + ")
    )
    
    model <- lm(as.formula(formula_str), data = df2)
    
    terms_to_extract <- c(indep_var, mod_var, interaction_term)
    coefs <- summary(model)$coefficients
    confint_df <- confint(model, level = 0.95)  # Wald confidence intervals
    
    available_terms <- intersect(terms_to_extract, rownames(coefs))
    
    # Extract values
    beta <- round(coefs[available_terms, "Estimate"], 4)
    se <- round(coefs[available_terms, "Std. Error"], digits = 4)
    p <- signif(coefs[available_terms, "Pr(>|t|)"], digits = 4)
    lowci <- round(confint_df[available_terms, 1], 4)
    highci <- round(confint_df[available_terms, 2], 4)
    
    # Combine into dataframe (without FDR yet)
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

final_results <- bind_rows(results_list)

#= FDR correction per independent variable (interaction terms only) =#
final_results$P_FDR <- NA
final_results$FDR_sig <- ""

for (indep_var in unique(final_results$Independent)) {
  interaction_mask <- final_results$Independent == indep_var &
    final_results$Term == paste0(indep_var, ":", final_results$Moderator)
  
  # Get raw p-values for current set
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

print(final_results)

write.csv(final_results, "moderation_results.csv", row.names = FALSE)


################################# Forest Plot ################################################

library(dplyr)
library(ggplot2)
library(ggeffects)

target_indep <- "Neuroticism_GPS"  # Select the independent variable manually

# 1. Filter interaction terms
interaction_data <- final_results %>%
  filter(Independent == target_indep,
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
  mutate(Moderator = factor(Moderator, levels = rev(unique(Moderator))))


# 3. Plot

ggplot(interaction_data_sorted, aes(x = Estimate, y = Moderator)) +
  geom_point(shape = 15, size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = max(CI_95_High) * 0.85, label = Significance), size = 3, hjust = 0) +
  geom_text(aes(x = max(CI_95_High) * 0.95, label = Beta_CI_Label), size = 3, hjust = 0) +
  theme_minimal() +
  labs(
    title = paste("Moderation Effects (", target_indep, ")", sep = ""),
    subtitle = "FDR-corrected: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
    x = "Beta Value of Interaction Term",
    y = "Moderator"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(min(interaction_data_sorted$CI_95_Low) - 0.01,
                           max(interaction_data_sorted$CI_95_High) + 0.04))



################################# Interaction Plot ################################################

library(dplyr)
library(ggplot2)
library(ggeffects)

# function for interaction plot

make_interaction_plot_grouped <- function(indep_var, mod_var, data, outcome_var, covariate_str) {
  # Create grouping for independent variable (10% and 90% quantiles)
  quantiles <- quantile(data[[indep_var]], probs = c(0.10, 0.90), na.rm = TRUE)
  group_var <- paste0(indep_var, "_group")
  
  data[[group_var]] <- cut(data[[indep_var]],
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                           labels = c("low", "middle", "high"),
                           right = TRUE)
  
  # Build formula with grouped indep_var and mod_var interaction + covariates + EV1-10
  formula_str <- paste(
    outcome_var, "~", group_var, "*", mod_var, "+", covariate_str, "+", paste0("EV", 1:10, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  model <- lm(model_formula, data = data)
  
  # Get predicted values for plotting interaction
  pred_df <- ggpredict(model, terms = c(mod_var, group_var))
  
  # Plot
  p <- ggplot(pred_df, aes(x = x, y = predicted, color = group, fill = group, linetype = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    scale_color_manual(values = c("green", "blue", "red")) +
    scale_fill_manual(values = c("green", "blue", "red")) +
    labs(
      title = paste("Moderation Effect"),
      x = mod_var,
      y = paste("Predicted", outcome_var),
      color = group_var,
      fill = group_var,
      linetype = group_var
    ) +
    theme_minimal(base_size = 14)
  
  return(p)
}

# use function to plot
plot1 <- make_interaction_plot_grouped(
  indep_var = "PsychiatricGPS_composite1",
  mod_var = "SelfAcceptance_R5",
  data = df2,
  outcome_var = "Depression_score_R6",
  covariate_str = COVARS
)

print(plot1)


################################# Mediation ################################################
library(mediation)
library(dplyr)

results_list <- list()

for (indep_var in indep) {
  for (med in moderators) {
    
    # Mediator model
    med_formula <- as.formula(
      paste(med, "~", indep_var, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + "))
    )
    med_model <- lm(med_formula, data = df2)
    
    # Outcome model
    out_formula <- as.formula(
      paste(outcome, "~", indep_var, "+", med, "+", COVARS, "+", paste0("EV", 1:10, collapse = " + "))
    )
    out_model <- lm(out_formula, data = df2)
    
    # Mediation analysis
    med_out <- mediate(
      med_model, out_model,
      treat = indep_var,
      mediator = med,
      boot = TRUE, sims = 1000
    )
    
    # Save results
    res_df <- data.frame(
      Independent = indep_var,
      Mediator = med,
      ACME = round(med_out$d0, 4),
      ACME_p = round(med_out$d0.p, 4),
      ADE = round(med_out$z0, 4),
      ADE_p = round(med_out$z0.p, 4),
      Total_Effect = round(med_out$tau.coef, 4),
      Total_p = round(med_out$tau.p, 4),
      Prop_Mediated = round(med_out$n0, 4),
      Prop_Mediated_p = round(med_out$n0.p, 4),
      stringsAsFactors = FALSE
    )
    
    results_list[[paste(indep_var, med, sep = "_x_")]] <- res_df
  }
}

# Combine all into one data frame
mediation_results <- bind_rows(results_list)

# FDR correction per independent variable
mediation_results$ACME_p_fdr <- NA
mediation_results$ADE_p_fdr <- NA
mediation_results$Prop_Mediated_p_fdr <- NA

for (indep_var in unique(mediation_results$Independent)) {
  idx <- mediation_results$Independent == indep_var
  
  mediation_results$ACME_p_fdr[idx] <- p.adjust(mediation_results$ACME_p[idx], method = "fdr")
  mediation_results$ADE_p_fdr[idx] <- p.adjust(mediation_results$ADE_p[idx], method = "fdr")
  mediation_results$Prop_Mediated_p_fdr[idx] <- p.adjust(mediation_results$Prop_Mediated_p[idx], method = "fdr")
}

# Optional: Sort by most significant mediation
mediation_results <- mediation_results[order(mediation_results$ACME_p), ]

# Save results
write.csv(mediation_results, "mediation_results.csv", row.names = FALSE)

# View
print(mediation_results)


