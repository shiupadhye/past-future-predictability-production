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
scores$Class <- factor(scores$Class, levels = c('Function','Content'))

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

m.FuncContentDist.mainEffect.condPMI<- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + condPMI  + Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.mainEffect.condPMI)

## interaction
m.FuncContentDist.int.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb * Class + infillFw_logProb * Class + delta_bwPred * Class + (1|speaker_id), data = scores)
summary(m.FuncContentDist.int.deltaBwPred)

## get simple means
emtrends(m.FuncContentDist.int.deltaBwPred, specs = "Class", var = "unigram_logProb",infer = c(TRUE, TRUE))

emtrends(m.FuncContentDist.int.deltaBwPred, specs = "Class", var = "infillFw_logProb",infer = c(TRUE, TRUE))

emtrends(m.FuncContentDist.int.deltaBwPred, specs = "Class", var = "delta_bwPred",infer = c(TRUE, TRUE))


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
  x_var = "delta_bwPred",
  group_var = "Class"
)

condPMI_class <- predict_interaction_with_ci(
  model = m.FuncContentDist.mainEffect.condPMI ,
  x_var = "condPMI",
  group_var = "Class"
)

## visualizations
p1 <- ggplot(unigramPred_class, aes(x = unigram_logProb, y = predicted, color = Class, fill = Class)) +
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


p2 <- ggplot(fwPred_class, aes(x = infillFw_logProb, y = predicted, color = Class, fill = Class)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Log Forward Predictability")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))

p3 <- ggplot(deltaBwPred_class, aes(x = delta_bwPred, y = predicted, color = Class, fill = Class)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Delta Backward Predictability")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))

p4 <- ggplot(condPMI_class, aes(x = condPMI, y = predicted, color = Class, fill = Class)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Conditional PMI")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))

# save plots
saveRDS(p1,'plots/rds/interaction_plot_freq.rds')
saveRDS(p2,'plots/rds/interaction_plot_fwPred.rds')
saveRDS(p3,'plots/rds/interaction_plot_bwPred.rds')
saveRDS(p4,'plots/rds/interaction_plot_PMI.rds')

# Replication of Bell et al. 2009
scores.HF_func <- scores %>%
  filter(lexical_category == 'High-Freq-Function')

scores.LF_func <- scores %>%
  filter(lexical_category == 'Mid-to-Low-Freq-Function')

scores.content <- scores %>%
  filter(lexical_category == 'Content')

m.BellAnalysisRep.HF_func.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + delta_bwPred + (1|speaker_id), data = scores.HF_func)
summary(m.BellAnalysisRep.HF_func.deltaBwPred)
coef(summary(m.BellAnalysisRep.HF_func.deltaBwPred))

m.BellAnalysisRep.HF_func.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + condPMI + (1|speaker_id), data = scores.HF_func)
summary(m.BellAnalysisRep.HF_func.condPMI)

m.BellAnalysisRep.LF_func.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + delta_bwPred + (1|speaker_id), data = scores.LF_func)
summary(m.BellAnalysisRep.LF_func.deltaBwPred)

m.BellAnalysisRep.LF_func.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + condPMI + (1|speaker_id), data = scores.LF_func)
summary(m.BellAnalysisRep.LF_func.condPMI)

m.BellAnalysisRep.content.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + delta_bwPred + (1|speaker_id), data = scores.content)
summary(m.BellAnalysisRep.content.deltaBwPred)

m.BellAnalysisRep.content.condPMI <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb + infillFw_logProb + condPMI + (1|speaker_id), data = scores.content)
summary(m.BellAnalysisRep.content.condPMI)


m.lexCat.int.deltaBwPred <- lmer(duration * 1000 ~ word_len + uttrSR + age + sex + unigram_logProb * lexical_category + infillFw_logProb * lexical_category + delta_bwPred * lexical_category + (1|speaker_id), data = scores)
summary(m.lexCat.int.deltaBwPred)

unigramPred_lexCat <- predict_interaction_with_ci(
  model =m.lexCat.int.deltaBwPred ,
  x_var = "unigram_logProb",
  group_var = "lexical_category"
)

ggplot(unigramPred_lexCat, aes(x = unigram_logProb, y = predicted, color = lexical_category, fill = lexical_category)) +
  geom_line() +
  geom_point() +
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

fwPred_lexCat <- predict_interaction_with_ci(
  model =m.lexCat.int.deltaBwPred ,
  x_var = "infillFw_logProb",
  group_var = "lexical_category"
)

ggplot(fwPred_lexCat, aes(x = infillFw_logProb, y = predicted, color = lexical_category, fill = lexical_category)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Log Forward Predictability")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))


bwPred_lexCat <- predict_interaction_with_ci(
  model =m.lexCat.int.deltaBwPred ,
  x_var = "delta_bwPred",
  group_var = "lexical_category"
)

ggplot(bwPred_lexCat, aes(x = delta_bwPred, y = predicted, color = lexical_category, fill = lexical_category)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.4, color = NA) +
  labs(x = expression(bold("Delta BwPred")), y = "") +
  theme_minimal() +
  #theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face = "bold", color = "black"),  # x-axis text
        axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
        axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
        axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
        plot.title = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold"))



  
