#######################################################################

# Replication and extension of Amit et al. (2013, Experiment 2)

#######################################################################

# Load packages -------------------------------------------------------

packages <- c("tidyverse", "lme4", "car")

lapply(packages, library, character.only = TRUE)

# Load data -----------------------------------------------------------

raw <- read.csv("replication_data.csv")

replication <- raw %>% 
  filter(attention_check == 1)

replication <- replication %>% 
  mutate(
    comf_acquaint   = comf_acquaint - 1,
    comf_close      = comf_close - 1,
    approp_acquaint = approp_acquaint - 1,
    approp_close    = approp_close - 1,
  )

cards_long <- replication %>% 
  pivot_longer(
    cols      = starts_with("cards"),
    names_to  = "card",
    values_to = "choice"
  ) %>% 
  filter(choice != "") %>% 
  extract(
    col  = "card",
    into  = "card",
    regex = "cards_.*_(\\d?\\d)"
  ) %>% 
  mutate(
    card   = paste("card_", card, sep = ""),
    choice = case_when(
      choice == "P" ~ 1,
      choice == "T" ~ 0
    )
  )

cards <- cards_long %>% 
  pivot_wider(
    id_cols     = "id",
    names_from  = "card",
    values_from = "choice"
  ) %>% 
  mutate(
    distant = card_1 + card_3 + card_6, 
    close   = card_7 + card_9 + card_14
  )

replication <- replication %>% 
  left_join(select(cards, id, distant, close), by = "id")

mc_long <- replication %>% 
  pivot_longer(
    cols      = starts_with("manipulation_check"),
    names_to  = "target",
    values_to = "perceived_distance"
  )

cards_sample <- cards_long %>% 
  pivot_wider(
    id_cols     = "id",
    names_from  = "card",
    values_from = "choice"
  ) %>% 
  mutate(
    distant = card_12 + card_2 + card_6, 
    close   = card_7  + card_9 + card_14
  )

sample_specific <- replication %>% 
  select(-close, -distant) %>% 
  left_join(select(cards_sample, id, distant, close), by = "id")

measured_dist_data <- cards_long %>% 
  mutate(
    distance_measured = case_when(
      card == "card_1"  ~ manipulation_check_1,
      card == "card_2"  ~ manipulation_check_2,
      card == "card_3"  ~ manipulation_check_3,
      card == "card_4"  ~ manipulation_check_4,
      card == "card_5"  ~ manipulation_check_5,
      card == "card_6"  ~ manipulation_check_6,
      card == "card_7"  ~ manipulation_check_7,
      card == "card_8"  ~ manipulation_check_8,
      card == "card_9"  ~ manipulation_check_9,
      card == "card_10" ~ manipulation_check_10,
      card == "card_11" ~ manipulation_check_11,
      card == "card_12" ~ manipulation_check_12,
      card == "card_13" ~ manipulation_check_13,
      card == "card_14" ~ manipulation_check_14
    )
  )

measured_dist_data$amit_wording <- factor(measured_dist_data$amit_wording, levels = c("original", "new"))

# Mixed ANOVA for Hypothesis 1 ----------------------------------------

distance_factor <- as.factor(c("close", "distant"))

h1_idata <- data.frame(distance_factor)

cards_bound <- cbind(replication$close, replication$distant)

wording <- as.factor(replication$amit_wording)

mixed_lm <- lm(cards_bound ~ wording)

h1_mixed_anova <- Anova(mixed_lm, idata = h1_idata, idesign = ~ distance_factor, type = 3)

rep_org <- replication %>% 
  filter(amit_wording == "original")

t_original <- t.test(rep_org$close, rep_org$distant, paired = TRUE)

rep_new <- replication %>% 
  filter(amit_wording == "new")

t_new <- t.test(rep_new$close, rep_new$distant, paired = TRUE)

## Partial Omega Squared

omega_sq <- function(aov_mod, N) {
  
  aov_summary <- summary(aov_mod)$univariate.tests
  
  ss_effect   <- aov_summary[, 1] 
  df_effect   <- aov_summary[, 2] 
  ss_error    <- aov_summary[, 3] 
  df_error    <- aov_summary[, 4] 
  
  ms_effect   <- ss_effect/df_effect
  ms_error    <- ss_error/df_error
  
  eta         <- ss_effect / (ss_effect + ss_error)
  omega       <- ( df_effect * (ms_effect - ms_error) ) / ( df_effect * ms_effect + (N + df_effect) * ms_error)
  
  return(list(partial_eta_sq = eta, partial_omega_sq = omega))
  
} 

partial_omega_h1 <- omega_sq(h1_mixed_anova, nrow(mixed_lm$fitted.values))

# Mixed ANOVA for sample-specific targets -----------------------------

mc_table <- 
mc_long %>% 
  group_by(target) %>% 
  summarise(
    mean   = mean(perceived_distance, na.rm = TRUE),
    sd     = sd(perceived_distance, na.rm = TRUE),
    median = median(perceived_distance, na.rm = TRUE)
  ) %>% 
  arrange(desc(mean)) %>% 
  knitr::kable()

cards_bound_samspc <- cbind(sample_specific$close, sample_specific$distant)

wording_samspc <- as.factor(sample_specific$amit_wording)

mixed_lm_samspc <- lm(cards_bound_samspc ~ wording_samspc)

h1_mixed_anova_samspc <- Anova(mixed_lm_samspc, idata = h1_idata, idesign = ~ distance_factor, type = 2)

## Partial Omega Squared

partial_omega_samspc <- omega_sq(h1_mixed_anova_samspc, nrow(mixed_lm_samspc$fitted.values))

## Simple Effects

rep_org_samspc <- sample_specific %>% 
  filter(amit_wording == "original")

t_original_samspc <- t.test(rep_org_samspc$close, rep_org_samspc$distant, paired = TRUE)

rep_new_samspc <- sample_specific %>% 
  filter(amit_wording == "new")

t_new_samspc <- t.test(rep_new_samspc$close, rep_new_samspc$distant, paired = TRUE)

# Analysis of all targets and moderators ------------------------------

log_reg_measured_main_refit <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured + amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_refit <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_cov <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + comf_acquaint + comf_close + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_two <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>%
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured + comf_close*distance_measured + comf_acquaint*amit_wording + comf_close*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_three <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

all_target_model_comparison <- anova(log_reg_measured_main_refit, log_reg_measured_inter_refit, log_reg_measured_inter_cov, log_reg_measured_inter_two, log_reg_measured_inter_three)

## Simple slopes

measured_dist_data_new <- measured_dist_data

contrasts(measured_dist_data_new$amit_wording) <- contrasts(measured_dist_data_new$amit_wording) - 1

### Two-way interaction

log_reg_measured_inter_three_new <- measured_dist_data_new %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(comf_acquaint, comf_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


simple_slopes_twoway <- data.frame(
  amit_wording = c("Original instructions", "Revised instructions"),
  ci = c(
    paste("b =", format(round(summary(log_reg_measured_inter_three)$coefficients[2, 1], 3), nsmall = 3), " [", format(round(summary(log_reg_measured_inter_three)$coefficients[2, 1] - summary(log_reg_measured_inter_three)$coefficients[2, 2] * qnorm(.975), 3), nsmall = 3), ", ", format(round(summary(log_reg_measured_inter_three)$coefficients[2, 1] + summary(log_reg_measured_inter_three)$coefficients[2, 2] * qnorm(.975), 3), nsmall = 3), "]"),
    paste("b =", format(round(summary(log_reg_measured_inter_three_new)$coefficients[2, 1], 3), nsmall = 3), " [", format(round(summary(log_reg_measured_inter_three_new)$coefficients[2, 1] - summary(log_reg_measured_inter_three_new)$coefficients[2, 2] * qnorm(.975), 3), nsmall = 3), ", ", format(round(summary(log_reg_measured_inter_three_new)$coefficients[2, 1] + summary(log_reg_measured_inter_three_new)$coefficients[2, 2] * qnorm(.975), 3), nsmall = 3), "]")
  )
)

simple_slopes_twoway$amit_wording <- factor(simple_slopes_twoway$amit_wording, levels = c("Original instructions", "Revised instructions"))

### Three-way interactions

quant_acq <- quantile(measured_dist_data$comf_acquaint, probs = seq(0, 1, .25), na.rm = TRUE)
quant_close <- quantile(measured_dist_data$comf_close, probs = seq(0, 1, .25), na.rm = TRUE)

simple_slopes_acq <- data.frame(
  target_acq   = rep(NA, 4),
  org_intc     = rep(NA, 4),
  org_coef     = rep(NA, 4),
  org_se       = rep(NA, 4),
  org_p        = rep(NA, 4),
  new_intc     = rep(NA, 4),
  new_coef     = rep(NA, 4),
  new_se       = rep(NA, 4),
  new_p        = rep(NA, 4)
)

simple_slopes_close <- data.frame(
  target_close   = rep(NA, 4),
  org_intc       = rep(NA, 4),
  org_coef       = rep(NA, 4),
  org_se         = rep(NA, 4),
  org_p          = rep(NA, 4),
  new_intc       = rep(NA, 4),
  new_coef       = rep(NA, 4),
  new_se         = rep(NA, 4),
  new_p          = rep(NA, 4)
)

for (i in 1:4) {
  
  target_acq <- median(quant_acq[(i):(i+1)])
  target_close <- median(quant_close[(i):(i+1)])
  
  org_mod_acq <- measured_dist_data %>% 
    mutate(
      distance_measured = scale(distance_measured, scale = FALSE),
      comf_acquaint = comf_acquaint - target_acq,
      comf_close = scale(comf_close, scale = FALSE)
    ) %>% 
    filter(complete.cases(comf_acquaint, comf_close)) %>% 
    glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) %>% 
    summary()
  
  org_mod_close <- measured_dist_data %>% 
    mutate(
      distance_measured = scale(distance_measured, scale = FALSE),
      comf_acquaint = scale(comf_acquaint, scale = FALSE),
      comf_close = comf_close - target_close
    ) %>% 
    filter(complete.cases(comf_acquaint, comf_close)) %>% 
    glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) %>% 
    summary()
  
  new_mod_acq <- measured_dist_data_new %>% 
    mutate(
      distance_measured = scale(distance_measured, scale = FALSE),
      comf_acquaint = comf_acquaint - target_acq,
      comf_close = scale(comf_close, scale = FALSE)
    ) %>% 
    filter(complete.cases(comf_acquaint, comf_close)) %>% 
    glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) %>% 
    summary()
  
  new_mod_close <- measured_dist_data_new %>% 
    mutate(
      distance_measured = scale(distance_measured, scale = FALSE),
      comf_acquaint = scale(comf_acquaint, scale = FALSE),
      comf_close = comf_close - target_close
    ) %>% 
    filter(complete.cases(comf_acquaint, comf_close)) %>% 
    glmer(choice ~ distance_measured * amit_wording + comf_acquaint*distance_measured*amit_wording + comf_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) %>% 
    summary()
  
  simple_slopes_acq$target_acq[i] <- target_acq
  simple_slopes_acq$org_intc[i]   <- summary(org_mod_acq)$coefficients[1, 1]
  simple_slopes_acq$org_coef[i]   <- summary(org_mod_acq)$coefficients[2, 1]
  simple_slopes_acq$org_se[i]     <- summary(org_mod_acq)$coefficients[2, 2]
  simple_slopes_acq$org_p[i]      <- summary(org_mod_acq)$coefficients[2, 4]
  simple_slopes_acq$new_intc[i]   <- summary(new_mod_acq)$coefficients[1, 1]
  simple_slopes_acq$new_coef[i]   <- summary(new_mod_acq)$coefficients[2, 1]
  simple_slopes_acq$new_se[i]     <- summary(new_mod_acq)$coefficients[2, 2]
  simple_slopes_acq$new_p[i]      <- summary(new_mod_acq)$coefficients[2, 4]
  
  simple_slopes_close$target_close[i] <- target_close
  simple_slopes_close$org_intc[i]     <- summary(org_mod_close)$coefficients[1, 1]
  simple_slopes_close$org_coef[i]     <- summary(org_mod_close)$coefficients[2, 1]
  simple_slopes_close$org_se[i]       <- summary(org_mod_close)$coefficients[2, 2]
  simple_slopes_close$org_p[i]        <- summary(org_mod_close)$coefficients[2, 4]
  simple_slopes_close$new_intc[i]     <- summary(new_mod_close)$coefficients[1, 1]
  simple_slopes_close$new_coef[i]     <- summary(new_mod_close)$coefficients[2, 1]
  simple_slopes_close$new_se[i]       <- summary(new_mod_close)$coefficients[2, 2]
  simple_slopes_close$new_p[i]        <- summary(new_mod_close)$coefficients[2, 4]
  
}

### Simple slope predictions

pred_curve <- function(intercept, coefficient, sequence) {
  
  y <- intercept + coefficient * sequence
  
  prob <- exp(y) / (1 + exp(y))
  
  return(prob)
  
}

seq_values <- seq(1, 7, .1) - mean(measured_dist_data$distance_measured, na.rm = TRUE)

#### Two-way interaction

simple_slopes_predictions_twoway <- data.frame(
  amit_wording = sort(rep(c("Original instructions", "Revised instructions"), length(seq_values))),
  distance_measured = rep(seq_values + mean(measured_dist_data$distance_measured, na.rm = TRUE), 2),
  prop = c(
    pred_curve(summary(log_reg_measured_inter_three)$coefficients[1, 1], summary(log_reg_measured_inter_three)$coefficients[2, 1], seq_values),
    pred_curve(summary(log_reg_measured_inter_three_new)$coefficients[1, 1], summary(log_reg_measured_inter_three_new)$coefficients[2, 1], seq_values)
  )
)

simple_slopes_predictions_twoway$amit_wording <- factor(simple_slopes_predictions_twoway$amit_wording, levels = c("Original instructions", "Revised instructions"))

#### Three-way interactions

simple_slopes_predictions_acq <- data.frame(
  amit_wording      = sort(rep(c("Original instructions", "Revised instructions"), length(seq_values) * 4)),
  acq_interval      = sort(rep(c("0-25%", "25-50%", "50-75%", "75-100%"), length(seq_values))),
  distance_measured = rep(seq_values + mean(measured_dist_data$distance_measured, na.rm = TRUE), 4),
  prop              = NA
)

simple_slopes_predictions_close <- data.frame(
  amit_wording      = sort(rep(c("Original instructions", "Revised instructions"), length(seq_values) * 4)),
  close_interval      = sort(rep(c("0-25%", "25-50%", "50-75%", "75-100%"), length(seq_values))),
  distance_measured = rep(seq_values + mean(measured_dist_data$distance_measured, na.rm = TRUE), 4),
  prop              = NA
)

for (i in 1:length(unique(simple_slopes_predictions_acq$acq_interval))) {
  
  temp_quant <- unique(simple_slopes_predictions_acq$acq_interval)[i]
  
  simple_slopes_predictions_acq[simple_slopes_predictions_acq$acq_interval == temp_quant &
                                simple_slopes_predictions_acq$amit_wording == "Original instructions", 
                                ]$prop <- pred_curve(
                                  
                                  simple_slopes_acq$org_intc[i], 
                                  simple_slopes_acq$org_coef[i], 
                                  seq_values
                                  
                                  )
  
  simple_slopes_predictions_acq[simple_slopes_predictions_acq$acq_interval == temp_quant &
                                simple_slopes_predictions_acq$amit_wording == "Revised instructions", 
                                ]$prop <- pred_curve(
                                  
                                  simple_slopes_acq$new_intc[i], 
                                  simple_slopes_acq$new_coef[i], 
                                  seq_values
    
                                  )
  
  
}

for (i in 1:length(unique(simple_slopes_predictions_close$close_interval))) {
  
  temp_quant <- unique(simple_slopes_predictions_close$close_interval)[i]
  
  simple_slopes_predictions_close[simple_slopes_predictions_close$close_interval == temp_quant &
                                  simple_slopes_predictions_close$amit_wording == "Original instructions", 
                                ]$prop <- pred_curve(
                                  
                                  simple_slopes_close$org_intc[i], 
                                  simple_slopes_close$org_coef[i], 
                                  seq_values
                                  
                                  )
  
  simple_slopes_predictions_close[simple_slopes_predictions_close$close_interval == temp_quant &
                                simple_slopes_predictions_close$amit_wording == "Revised instructions", 
                                ]$prop <- pred_curve(
                                  
                                  simple_slopes_close$new_intc[i], 
                                  simple_slopes_close$new_coef[i], 
                                  seq_values
    
                                  )
  
  
}

simple_slopes_predictions_acq$amit_wording <- factor(simple_slopes_predictions_acq$amit_wording, levels = c("Original instructions", "Revised instructions"))
simple_slopes_predictions_close$amit_wording <- factor(simple_slopes_predictions_close$amit_wording, levels = c("Original instructions", "Revised instructions"))

simple_slope_cis_acq <- 
simple_slopes_acq %>% 
  mutate(
    org_lower = org_coef - org_se * qnorm(.975),
    org_upper = org_coef + org_se * qnorm(.975),
    new_lower = new_coef - new_se * qnorm(.975),
    new_upper = new_coef + new_se * qnorm(.975),
    org_ci    = paste("b =", format(round(org_coef, 3), nsmall = 3), " [", format(round(org_lower, 3), nsmall = 3), ", ", format(round(org_upper, 3), nsmall = 3), "]"),
    new_ci    = paste("b =", format(round(new_coef, 3), nsmall = 3), " [", format(round(new_lower, 3), nsmall = 3), ", ", format(round(new_upper, 3), nsmall = 3), "]")
  ) %>% 
  select(target_close, org_ci, new_ci) %>% 
  mutate(
    acq_interval = c("0-25%", "25-50%", "50-75%", "75-100%")
  ) %>% 
  pivot_longer(
    cols = ends_with("ci"),
    names_to = "amit_wording",
    values_to = "ci"
  ) %>% 
  mutate(
    amit_wording = case_when(
      amit_wording == "org_ci" ~ "Original instructions",
      amit_wording == "new_ci" ~ "Revised instructions"
    )
  )

simple_slope_cis_close <- 
simple_slopes_close %>% 
  mutate(
    org_lower = org_coef - org_se * qnorm(.975),
    org_upper = org_coef + org_se * qnorm(.975),
    new_lower = new_coef - new_se * qnorm(.975),
    new_upper = new_coef + new_se * qnorm(.975),
    org_ci    = paste("b =", format(round(org_coef, 3), nsmall = 3), " [", format(round(org_lower, 3), nsmall = 3), ", ", format(round(org_upper, 3), nsmall = 3), "]"),
    new_ci    = paste("b =", format(round(new_coef, 3), nsmall = 3), " [", format(round(new_lower, 3), nsmall = 3), ", ", format(round(new_upper, 3), nsmall = 3), "]")
  ) %>% 
  select(target_close, org_ci, new_ci) %>%
  mutate(
    close_interval = c("0-25%", "25-50%", "50-75%", "75-100%")
  ) %>% 
  pivot_longer(
    cols = ends_with("ci"),
    names_to = "amit_wording",
    values_to = "ci"
  ) %>% 
  mutate(
    amit_wording = case_when(
      amit_wording == "org_ci" ~ "Original instructions",
      amit_wording == "new_ci" ~ "Revised instructions"
    )
  )

simple_slope_cis_acq$amit_wording <- factor(simple_slope_cis_acq$amit_wording, levels = c("Original instructions", "Revised instructions"))
simple_slope_cis_close$amit_wording <- factor(simple_slope_cis_close$amit_wording, levels = c("Original instructions", "Revised instructions"))

## Visualizations

hist_acq <- 
  ggplot(replication,
         aes(
           x = comf_acquaint
         )) +
  geom_histogram(
    binwidth = 1,
    color = "black",
    fill = "grey"
  ) +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Comfort sending a picture to an acquaintance",
    y = "Frequency",
    subtitle = "People are somewhat uncomfortable sending pictures of\nthemselves and their partner to an acquaintance"
  ) +
  theme_classic() +
  theme(
    plot.subtitle = element_text(face = "italic")
  )

hist_close <- 
  ggplot(replication,
         aes(
           x = comf_close
         )) +
  geom_histogram(
    binwidth = 1,
    color = "black",
    fill = "grey"
  ) +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Comfort sending a picture to a close friend",
    y = "Frequency",
    subtitle = "People are generally comfortable sending pictures of\nthemselves and their partner to a close friend"
  ) +
  theme_classic() +
  theme(
    plot.subtitle = element_text(face = "italic")
  )

prop_tab <- measured_dist_data %>%
  filter(complete.cases(distance_measured)) %>% 
  group_by(amit_wording, distance_measured) %>% 
  summarise(
    prop = sum(choice)/n(), 
    n = n()
  )

prop_tab$amit_wording <- as.character(prop_tab$amit_wording)
prop_tab[prop_tab$amit_wording == "original", ]$amit_wording <- "Original instructions"
prop_tab[prop_tab$amit_wording == "new", ]$amit_wording      <- "Revised instructions"

prop_tab$amit_wording <- factor(prop_tab$amit_wording, levels = c("Original instructions", "Revised instructions"))

prop_main <- 
  ggplot(prop_tab,
         aes(
           x = distance_measured,
           y = prop
         )) +
  facet_grid(amit_wording ~ .) +
  geom_hline(
    linetype = "dashed",
    yintercept = .50
  ) +
  geom_line(
    data = simple_slopes_predictions_twoway,
    aes(
      x = distance_measured,
      y = prop
    ),
    size = 1.5,
    color = "#371E30",
    linetype = "dotted",
    alpha = .75
  ) +
  geom_text(
    data = simple_slopes_twoway,
    aes(
      x = 2.25,
      y = .95,
      label = ci
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_line(
    size = 1.5,
    color = "#DB324D",
    alpha = .90
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Perceived social distance",
    y = "Proportion of picture cards sent",
    title = "Distance x Wording",
    subtitle = 'Social distance predicts sending a picture card when the content\nis specified as a picture of "you and your girl-/boyfriend."\nOtherwise, the relationship is small and in the opposite direction.'
  ) +
  theme_classic() +
  theme(
    plot.subtitle = element_text(face = "italic")
  )


prop_tab_acq <- measured_dist_data %>%
  filter(complete.cases(comf_acquaint)) %>% 
  mutate(
    acq_interval = case_when(
      comf_acquaint <= quant_acq[[2]]                                   ~ "0-25%",
      comf_acquaint >  quant_acq[[2]] & comf_acquaint <= quant_acq[[3]] ~ "25-50%",
      comf_acquaint >  quant_acq[[3]] & comf_acquaint <= quant_acq[[4]] ~ "50-75%",
      comf_acquaint >  quant_acq[[4]]                                   ~ "75-100%"
    )
  ) %>% 
  group_by(amit_wording, distance_measured, acq_interval) %>% 
  summarise(
    prop = sum(choice)/n(), 
    n = n()
  )

prop_tab_acq$amit_wording <- as.character(prop_tab_acq$amit_wording)
prop_tab_acq[prop_tab_acq$amit_wording == "original", ]$amit_wording <- "Original instructions"
prop_tab_acq[prop_tab_acq$amit_wording == "new", ]$amit_wording      <- "Revised instructions"

prop_tab_acq$amit_wording <- factor(prop_tab_acq$amit_wording, levels = c("Original instructions", "Revised instructions"))

prop_acq <- 
  ggplot(prop_tab_acq,
         aes(
           x = distance_measured,
           y = prop
         )) +
  facet_grid(amit_wording ~ acq_interval) +
  geom_hline(
    linetype = "dashed",
    yintercept = .50
  ) +
  geom_line(
    data = simple_slopes_predictions_acq,
    aes(
      x = distance_measured,
      y = prop
    ),
    size = 1.5,
    color = "#371E30",
    linetype = "dotted",
    alpha = .75
  ) +
  geom_text(
    data = simple_slope_cis_acq,
    aes(
      x = 3.20,
      y = .95,
      label = ci
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_line(
    size = 1.5,
    color = "#DB324D",
    alpha = .90
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Perceived social distance",
    y = "Proportion of picture cards sent",
    tag = "Comfort - Acquaintance (quantiles)",
    title = "Distance x Wording x Comfort - Acquaintance",
    subtitle = 'Social distance predicts sending a picture card most when the content is specified and people are\nuncomfortable sending pictures of themselves and their partner to acquaintances (and they generally\nare). Otherwise, the relationship is smaller, nonexistent, or in the opposite direction.\n'
  ) +
  theme_classic() +
  theme(
    plot.tag.position = c(.52, .825),
    plot.tag = element_text(face = "plain", size = 10.5), 
    plot.subtitle = element_text(face = "italic")
  )

prop_tab_close <- measured_dist_data %>%
  filter(complete.cases(comf_close)) %>% 
  mutate(
    close_interval = case_when(
      comf_close <= quant_acq[[2]]                                ~ "0-25%",
      comf_close >  quant_acq[[2]] & comf_close <= quant_acq[[3]] ~ "25-50%",
      comf_close >  quant_acq[[3]] & comf_close <= quant_acq[[4]] ~ "50-75%",
      comf_close >  quant_acq[[4]]                                ~ "75-100%"
    )
  ) %>% 
  group_by(amit_wording, distance_measured, close_interval) %>% 
  summarise(
    prop = sum(choice)/n(), 
    n = n()
  )

prop_tab_close$amit_wording <- as.character(prop_tab_close$amit_wording)
prop_tab_close[prop_tab_close$amit_wording == "original", ]$amit_wording <- "Original instructions"
prop_tab_close[prop_tab_close$amit_wording == "new", ]$amit_wording      <- "Revised instructions"

prop_tab_close$amit_wording <- factor(prop_tab_close$amit_wording, levels = c("Original instructions", "Revised instructions"))

prop_close <- 
  ggplot(prop_tab_close,
         aes(
           x = distance_measured,
           y = prop
         )) +
  facet_grid(amit_wording ~ close_interval) +
  geom_hline(
    linetype = "dashed",
    yintercept = .50
  ) +
  geom_line(
    data = simple_slopes_predictions_close,
    aes(
      x = distance_measured,
      y = prop
    ),
    size = 1.5,
    color = "#371E30",
    linetype = "dotted",
    alpha = .75
  ) +
  geom_text(
    data = simple_slope_cis_close,
    aes(
      x = 3.20,
      y = .95,
      label = ci
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_line(
    size = 1.5,
    color = "#DB324D",
    alpha = .90
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Perceived social distance",
    y = "Proportion of picture cards sent",
    tag = "Comfort - Close (quantiles)",
    title = "Distance x Wording x Comfort - Close",
    subtitle = 'Social distance predicts sending a picture card most when the content is specified and people are\ncomfortable sending pictures of themselves and their partner to close friends (and they generally\nare). Otherwise, the relationship is smaller, nonexistent, or in the opposite direction.\n'
  ) +
  theme_classic() +
  theme(
    plot.tag.position = c(.52, .825),
    plot.tag = element_text(face = "plain", size = 10.5),
    plot.subtitle = element_text(face = "italic")
  )

hist_grid <- cowplot::plot_grid(hist_acq, hist_close, align = "v", nrow = 2)

top_plots <- cowplot::plot_grid(prop_main, hist_grid, nrow = 1)

bottom_plots <- cowplot::plot_grid(prop_acq, prop_close, nrow = 2)

full_plots <- cowplot::plot_grid(top_plots, bottom_plots, nrow = 2, rel_heights = c(1, 1.70))

cowplot::save_plot("interaction_plot.png", full_plots, base_height = 16, base_width = 10)
cowplot::save_plot("interaction_plot.eps", full_plots, base_height = 16, base_width = 10)
cowplot::save_plot("interaction_plot.tiff", full_plots, base_height = 16, base_width = 10)

# Logistic Mixed Effects Regression for Original Critical Targets -----

log_reg_data <- cards_long %>% 
  filter(card == "card_1" | card == "card_3" | card == "card_6" |
           card == "card_7" | card == "card_9" | card == "card_14" ) %>% 
  mutate(
    distance = case_when(
      card == "card_1" | card == "card_3" | card == "card_6"  ~ 0,
      card == "card_7" | card == "card_9" | card == "card_14" ~ 1
    ),
    comf_acquaint = scale(comf_acquaint, scale = FALSE),
    comf_close = scale(comf_close, scale = FALSE),
    approp_acquaint = scale(approp_acquaint, scale = FALSE),
    approp_close = scale(approp_close, scale = FALSE)
  )

log_reg_data$amit_wording <- factor(log_reg_data$amit_wording, levels = c("original", "new"))

h1_melogreg <- glmer(choice ~ distance + amit_wording + (1 + distance|id) + (1|card), 
                     data = log_reg_data, family = binomial(link = "logit"))

h1_melogreg_inter <- glmer(choice ~ distance * amit_wording + (1 + distance|id) + (1|card), 
                           data = log_reg_data, family = binomial(link = "logit"))

h1_model_comparison <- anova(h1_melogreg, h1_melogreg_inter)

# Robustness check with perceived appropriateness ---------------------

log_reg_measured_inter_one_app <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    approp_acquaint = scale(approp_acquaint, scale = FALSE),
    approp_close = scale(approp_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(approp_acquaint, approp_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + approp_acquaint + approp_close + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_two_app <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    approp_acquaint = scale(approp_acquaint, scale = FALSE),
    approp_close = scale(approp_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(approp_acquaint, approp_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + approp_acquaint*distance_measured + approp_close*distance_measured + approp_acquaint*amit_wording + approp_close*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

log_reg_measured_inter_three_app <- measured_dist_data %>% 
  mutate(
    distance_measured = scale(distance_measured, scale = FALSE),
    approp_acquaint = scale(approp_acquaint, scale = FALSE),
    approp_close = scale(approp_close, scale = FALSE)
  ) %>% 
  filter(complete.cases(approp_acquaint, approp_close)) %>% 
  glmer(choice ~ distance_measured * amit_wording + approp_acquaint*distance_measured*amit_wording + approp_close*distance_measured*amit_wording + (1|id) + (1|card), data = ., family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


approp_lrt <- anova(log_reg_measured_inter_one_app, log_reg_measured_inter_two_app, log_reg_measured_inter_three_app)
