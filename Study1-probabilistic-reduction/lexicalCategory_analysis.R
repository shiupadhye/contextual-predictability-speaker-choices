library(tidyverse)
library(lme4)
library(ggsignif)
library(ggpubr)
library(lmtest)
library(patchwork)
library(cowplot)
library(emmeans)
library(relaimpo)
library(partR2)
library(ggeffects)
library(lmerTest)
library(dominanceanalysis)

scores <- read_csv('SWBD_DurAnalysisData.csv')
scores <- scores %>% filter(Class != 'OTHER')

# define backward predictability variants
scores$delta_bwPred <- scores$infillBw_logProb - scores$infillFw_logProb
scores <- scores %>% rename(condPMI = infill_pmiFP)

# analysis of function versus content words
HF_func_words <- c('i','and','the','that','a','to','you','of','it','in')
scores <- scores %>% 
  mutate(lexical_category = case_when(
    Class == 'Content'~ 'Content',
    Class == 'Function' & word %in% HF_func_words ~ 'High-Freq-Function',
    Class == 'Function' & !(word %in% HF_func_words) ~ 'Mid-to-Low-Freq-Function'
  ))

scores %>%
  group_by(lexical_category) %>%
  count()

##  main effect of func-content words
m.FuncContentDist.mainEffect.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + delta_bwPred + Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.mainEffect.deltaBwPred)

m.FuncContentDist.mainEffect.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + delta_bwPred + Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.mainEffect.condPMI)

## interaction
m.FuncContentDist.int.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb * Class + infillFw_logProb * Class + delta_bwPred * Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.int.deltaBwPred)

m.FuncContentDist.int.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb * Class + infillFw_logProb * Class + condPMI * Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.int.condPMI)

### visualize
predict_interaction_with_ci <- function(model, x_var, group_var, n_points = 100, 
                                        nsim = 100, seed = 123) {
  model_data <- model.frame(model)
  
  # Error check
  if (!all(c(x_var, group_var) %in% names(model_data))) {
    stop("x_var and group_var must be in the model data.")
  }
  
  # Preserve factor levels as in the model
  group_levels <- levels(model_data[[group_var]])
  
  # Create a prediction grid
  grid <- expand.grid(
    x = seq(min(model_data[[x_var]], na.rm = TRUE),
            max(model_data[[x_var]], na.rm = TRUE),
            length.out = n_points),
    group = factor(group_levels, levels = group_levels)
  )
  names(grid)[1:2] <- c(x_var, group_var)
  
  # Fill in other predictors as average (or base level for factors)
  all_vars <- all.vars(formula(model))
  other_vars <- setdiff(all_vars, c(x_var, group_var, as.character(attr(terms(model), "response"))))
  
  for (v in other_vars) {
    if (!v %in% names(grid)) {
      if (is.factor(model_data[[v]]) || is.character(model_data[[v]])) {
        grid[[v]] <- factor(levels(model_data[[v]])[1], levels = levels(model_data[[v]]))
      } else {
        grid[[v]] <- mean(model_data[[v]], na.rm = TRUE)
      }
    }
  }
  
  # Define prediction function for bootMer
  pred_fun <- function(fit) {
    predict(fit, newdata = grid, re.form = NA)
  }
  
  set.seed(seed)
  boot_results <- bootMer(
    model,
    pred_fun,
    nsim = nsim,
    type = "parametric",
    use.u = FALSE,
    verbose = FALSE
  )
  
  # Compute predictions and confidence intervals
  predicted <- apply(boot_results$t, 2, mean)
  ci_low <- apply(boot_results$t, 2, quantile, 0.025)
  ci_high <- apply(boot_results$t, 2, quantile, 0.975)
  
  output <- cbind(grid, predicted = predicted, ci_low = ci_low, ci_high = ci_high)
  
  # Ensure group variable stays a factor with correct labels
  output[[group_var]] <- factor(output[[group_var]], levels = group_levels)
  
  return(output)
}

unigramPred_class <- predict_interaction_with_ci(
  model = m.FuncContentDist.int.deltaBwPred,
  x_var = "unigram_logProb",
  group_var = "Class"
)

fwPred_class <- predict_interaction_with_ci(
  model = m.FuncContentDist.mainEffect.deltaBwPred ,
  x_var = "infillFw_logProb",
  group_var = "Class"
)

deltaBwPred_class <- predict_interaction_with_ci(
  model = m.FuncContentDist.mainEffect.deltaBwPred ,
  x_var = "deltaBwPred",
  group_var = "Class"
)

## visualizations
ggplot(unigramPred_class, aes(x = unigram_logProb, y = predicted, color = class, fill = class)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Log Unigram Predictability")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))
