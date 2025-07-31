setwd("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project")

library(parameters)
library(dplyr)
library(ggplot2)
library(ggeffects)

df <- read.csv("wls_merged_250719.csv")
df2 <- df

# DepGPS_composite2 
df2$row_mean <- rowMeans(df2[, c("Depression_GPS", "Neuroticism_GPS")], na.rm = TRUE)
df2$DepGPS_composite2 <- as.numeric(scale(df2$row_mean))

df_male <- df2 %>% filter(sex == 1)
df_female <- df2 %>% filter(sex == 2)


################################# Variables ################################################

# moderators (prs)
mod <- c('DepGPS_composite',
         'DepGPS_composite2',
         'IntelligenceGPS_composite',
         'Depression_GPS', 'MDD_GPS', 'Neuroticism_GPS',
         'EA_GPS','IQ_GPS','CP_GPS')

outcome <- c('Depression_score_R6')

# Covariates
cov_cont <- c('age_R5', 'Personal_Income_R6', 'Education_years_R6')
cov_factor <- c('sex', 'Marital_status_R6_new')
df2[cov_factor] <- lapply(df2[cov_factor], factor)
cov_ev <- paste0('EV', 1:10)

cov_list <- c(cov_cont, cov_factor, cov_ev)
COVARS <- paste(cov_list, collapse = " + ")

# Independent variables
PersonalResource <- c('Autonomy_R5', 'EnvironmentalMastery_R5', 'PersonalGrowth_R5',
                      'PositiveRelationship_R5', 'SelfAcceptance_R5', 'PurposeinLife_R5',
                      'Optimism_R5', 'Mattering_R5', 'PersonalResource_composite')

SocialSupport1 <- c('Support_money_R5', 'Support_problem_R5', 'Support_sick_R5', 'SocialSupport1_composite')
SocialSupport2 <- c('Love_social_R5','Listen_social_R5','SocialSupport2_composite')
SocialParticipation1 <- c('Social_friends_R5', 'Social_relatives_R5', 'SocialParticipation1_composite')
Social_involvement_R5 <- c('Social_involvement_R5')
indep <- c(PersonalResource, SocialSupport1, SocialSupport2, SocialParticipation1, Social_involvement_R5)



# ---------- Function to Run and Save Results by Sex ----------

analyze_by_sex <- function(data, sex_label) {
  results_main <- list()
  results_mod <- list()
  
  for (indep_var in indep) {
    for (mod_var in mod) {
      
      # Additive Effects (Main)
      formula_main <- paste(outcome, "~", indep_var, "+", mod_var, "+", COVARS)
      model_main <- lm(as.formula(formula_main), data = data)
      
      coefs <- summary(model_main)$coefficients
      confint_df <- confint(model_main)
      
      for (term in c(indep_var, mod_var)) {
        if (term %in% rownames(coefs)) {
          results_main[[length(results_main) + 1]] <- data.frame(
            Sex = sex_label,
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
      
      # Moderation Effects (Interaction)
      formula_mod <- paste(outcome, "~", indep_var, "*", mod_var, "+", COVARS)
      model_mod <- lm(as.formula(formula_mod), data = data)
      
      coefs_mod <- summary(model_mod)$coefficients
      confint_mod <- confint(model_mod)
      terms_to_extract <- c(indep_var, mod_var, paste0(indep_var, ":", mod_var))
      available_terms <- intersect(terms_to_extract, rownames(coefs_mod))
      
      res_df <- data.frame(
        Sex = sex_label,
        Model = paste(indep_var, "x", mod_var),
        Independent = indep_var,
        Moderator = mod_var,
        Term = available_terms,
        Estimate = round(coefs_mod[available_terms, "Estimate"], 4),
        Std_Error = round(coefs_mod[available_terms, "Std. Error"], 4),
        CI_95_Low = round(confint_mod[available_terms, 1], 4),
        CI_95_High = round(confint_mod[available_terms, 2], 4),
        P_value = signif(coefs_mod[available_terms, "Pr(>|t|)"], 4),
        stringsAsFactors = FALSE
      )
      results_mod[[length(results_mod) + 1]] <- res_df
    }
  }
  
  main_df <- bind_rows(results_main)
  mod_df <- bind_rows(results_mod)
  
  # FDR corrections - main effects
  main_df$P_FDR <- NA
  main_df$FDR_sig <- ""
  
  for (indep_var in unique(main_df$Independent)) {
    idx <- main_df$Independent == indep_var
    fdr_vals <- p.adjust(main_df$P_value[idx], method = "fdr")
    main_df$P_FDR[idx] <- fdr_vals
    main_df$FDR_sig[idx] <- symnum(fdr_vals,
                                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                   symbols = c("***", "**", "*", ".", ""))
  }
  
  # FDR corrections - interaction terms only
  mod_df$P_FDR <- NA
  mod_df$FDR_sig <- ""
  
  for (indep_var in unique(mod_df$Independent)) {
    int_mask <- mod_df$Independent == indep_var & mod_df$Term == paste0(indep_var, ":", mod_df$Moderator)
    fdr_vals <- p.adjust(mod_df$P_value[int_mask], method = "fdr")
    mod_df$P_FDR[int_mask] <- fdr_vals
    mod_df$FDR_sig[int_mask] <- symnum(fdr_vals,
                                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                       symbols = c("***", "**", "*", ".", ""))
  }
  
  # Save
  write.csv(main_df, paste0("Results/main_effect_", tolower(sex_label), "_250731.csv"), row.names = FALSE)
  write.csv(mod_df, paste0("Results/moderation_effect_", tolower(sex_label), "_250731.csv"), row.names = FALSE)
  
  return(list(main = main_df, mod = mod_df))
}


# Run for each sex
res_male <- analyze_by_sex(df_male, "Male")
res_female <- analyze_by_sex(df_female, "Female")



# ---------- Forest Plot --------------

target_mod <- "DepGPS_composite2"
gender <- "Male"
data <- res_male$mod


# 1. Filter interaction terms
interaction_data <- data %>%
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


# 3. Plot
ggplot(interaction_data_sorted, aes(x = Estimate, y = Independent)) +
  geom_point(shape = 15, size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_95_Low, xmax = CI_95_High), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = max(CI_95_High) + 0.005, label = Significance), size = 3, hjust = 0) +
  geom_text(aes(x = max(CI_95_High) + 0.010, label = Beta_CI_Label), size = 3, hjust = 0) +
  theme_minimal() +
  labs(
    title = sprintf("Moderation Effects (%s, %s)", target_mod, gender),
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
  coord_cartesian(xlim = c(min(interaction_data_sorted$CI_95_Low) - 0.01,
                           max(interaction_data_sorted$CI_95_High) + 0.04))




