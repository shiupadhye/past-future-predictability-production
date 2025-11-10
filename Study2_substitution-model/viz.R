library(grid)
library(tidyverse)
library(lme4)
library(ggsignif)
library(ggpubr)
library(shadowtext)
library(lmtest)
library(patchwork)
library(cowplot)
library(emmeans)
library(relaimpo)
library(partR2)
library(ggeffects)
library(lmerTest)
library(dominanceanalysis)

# model coeffs and comparison plots
lexSubsCoefData <- read_csv('results/model_coefficients_all.csv')
head(lexSubsCoefData)

lexSubsCoefData$significance <- ifelse(lexSubsCoefData$p < 0.001, "***",
                                   ifelse(lexSubsCoefData$p < 0.01, "**",
                                          ifelse(lexSubsCoefData$p < 0.05, "*", "ns")))

head(lexSubsCoefData)

lexSubsCoefData$Construct <- factor(lexSubsCoefData$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future","Communicative Reward")) 

lexSubsCoefData$Predictor <- factor(lexSubsCoefData$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Semantic Distance","Phonetic Distance","Backward Predictability","Unconditional PMI","Relative Backward Predictability","Conditional PMI")) 

lexSubsCoefData.filtered <- lexSubsCoefData %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Relative Backward Predictability")))

lexSubsCoefData.filtered$Model <- factor(lexSubsCoefData.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 

lexSubs.colors <- c('#3E8D31','#295C9D','#882B8D','#f89441')
plt.modelCoeffs<- ggplot(lexSubsCoefData.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = lexSubs.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance, 
                y = ifelse(Beta > 0, Beta + 0.3, Beta - 0.3)), 
            size = 5) +
  facet_wrap(~Model,scales = "free_x") +
  ylab('Estimated Effect Sizes') +
  xlab('Key Predictors') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=14))+  # facet labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.margin = margin(20, 5.5, 5.5, 40))  +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold"))

plt.modelCoeffs


### Model comparison
model_comp <- read_csv('results/model_comparisons.csv')
head(model_comp)

model_comp.loglik <- model_comp %>%
  filter(Criterion == 'LogLik')

model_comp.loglik$significance <- ifelse(model_comp.loglik$p < 0.001, "***",
                                         ifelse(model_comp.loglik$p < 0.01, "**",
                                                ifelse(model_comp.loglik$p< 0.05, "*", "ns")))

df.LL <- model_comp.loglik%>%
  filter(!Model %in% c('Baseline','Backward Predictability','Unconditional PMI')) %>%
  mutate(
    Model = factor(Model, levels = c(
      "Conditional PMI",
      "Relative Backward Predictability",
      "Both"
    )),
    Model = fct_reorder(Model, DeltaCriterion, .desc = FALSE)  # Best model on the left
  )

comparisons <- list(
  c("Relative Backward Predictability", "Conditional PMI"),
  c("Relative Backward Predictability", "Both"),
  c("Conditional PMI", "Both")
)

# Compute dynamic y-positions as a percentage of the max DLL
max_height <- max(df.LL$DeltaCriterion)
spacing <- max_height * 0.3  # 5% of max for spacing
y_positions <- max_height + seq(spacing, by = spacing, length.out = length(comparisons))

plt.modelComp.LL <- ggplot(df.LL, aes(x = Model, y = DeltaCriterion)) +
  geom_bar(stat = "identity", fill = '#882B8D', width = 0.7, alpha = 0.7) +
  ylab(expression(Delta * bold("Log-Likelihood"))) +
  xlab("Predictor added to baseline model") +
  ylim(0,50) +
  
  geom_signif(
    comparisons = comparisons,
    annotations = c("***","***","ns"),
    y_position = y_positions,
    tip_length = 0.02,
    textsize = 5,
    size = 0.8
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=10),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=10),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=10),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=10),  # y-axis title
    plot.title = element_text(face = "bold", color = "black")
  )

plt.modelComp.LL 


model_comp.BIC <- model_comp %>%
  filter(Criterion == 'BIC')

df.BIC <- model_comp.BIC%>%
  filter(!Model %in% c('Baseline','Backward Predictability','Unconditional PMI')) %>%
  mutate(
    Model = factor(Model, levels = c(
      "Relative Backward Predictability",
      "Conditional PMI",
      "Both"
    )),
    #Model = fct_reorder(Model, DeltaCriterion, .desc = TRUE)  # Best model on the left
  )

# Compute dynamic y-positions as a percentage of the max DLL
max_height <- max(df.BIC$DeltaCriterion)
spacing <- max_height * 0.3  # 5% of max for spacing
y_positions <- max_height + seq(spacing, by = spacing, length.out = length(comparisons))

df.BIC <- df.BIC %>%
  mutate(label = "Study 2: Modeling Substitution Identity")

plt.modelComp.BIC <- ggplot(df.BIC, aes(x = Model, y = DeltaCriterion)) +
  geom_bar(stat = "identity", fill = '#882B8D', width = 0.7, alpha = 0.7) +
  ylab(expression(Delta * bold("BIC"))) +
  xlab("Predictor added to baseline model") +
  
  #geom_signif(
  #  comparisons = comparisons,
  #  annotations = c("***","***","***"),
  #  y_position = y_positions,
  #  tip_length = 0.02,
  #  textsize = 5,
  #  size = 0.8
  #) +
  facet_grid(~label) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=18),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=18),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=18),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=18),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black",size=20)
  )

plt.modelComp.BIC 

# save
plt.study2.modelComp <- plt.modelCoeffs + plt.modelComp.LL + plot_layout(widths = c(3, 1))
plt.study2.modelComp

ggsave("plots/outputs/Study2_ModelCoeffs&LRT.pdf", plt.study2.modelComp, width = 14, height = 8)

ggsave("plots/outputs/Study2_ModelComp_BIC.pdf", plt.modelComp.BIC , width = 10, height = 8) 

## Fine-grained error analysis
### Semantic
lexSubsCoefData.sem <- read_csv('results/model_coefficients_sem.csv')
head(lexSubsCoefData.sem)

lexSubsCoefData.sem$significance <- ifelse(lexSubsCoefData.sem$p < 0.001, "***",
                                       ifelse(lexSubsCoefData.sem$p < 0.01, "**",
                                              ifelse(lexSubsCoefData.sem$p < 0.05, "*", "ns")))

head(lexSubsCoefData.sem)

lexSubsCoefData.sem$Construct <- factor(lexSubsCoefData.sem$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future","Communicative Reward")) 

lexSubsCoefData.sem$Predictor <- factor(lexSubsCoefData.sem$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Semantic Distance","Phonetic Distance","Backward Predictability","Unconditional PMI","Relative Backward Predictability","Conditional PMI")) 

lexSubsCoefData.sem.filtered <- lexSubsCoefData.sem %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Relative Backward Predictability")))

lexSubsCoefData.sem.filtered$Model <- factor(lexSubsCoefData.sem.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 

lexSubs.colors <- c('#3E8D31','#295C9D','#882B8D','#f89441')
plt.modelCoeffs.sem <- ggplot(lexSubsCoefData.sem.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = lexSubs.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance, 
                y = ifelse(Beta > 0, Beta + 0.3, Beta - 0.6)), 
            size = 5) +
  facet_wrap(~Model,scales = "free_x") +
  ylab('Estimated Effect Sizes') +
  xlab('') +
  theme_classic() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),  # x-axis text
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=10))+  # facet labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.margin = margin(20, 5.5, 5.5, 40))  +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold")) +
  ggtitle("Semantic Substitutions")

plt.modelCoeffs.sem

### phonological
lexSubsCoefData.phon <- read_csv('results/model_coefficients_phon.csv')
head(lexSubsCoefData.phon)

lexSubsCoefData.phon$significance <- ifelse(lexSubsCoefData.phon$p < 0.001, "***",
                                           ifelse(lexSubsCoefData.phon$p < 0.01, "**",
                                                  ifelse(lexSubsCoefData.phon$p < 0.05, "*", "")))

head(lexSubsCoefData.phon)

lexSubsCoefData.phon$Construct <- factor(lexSubsCoefData.phon$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future","Communicative Reward")) 

lexSubsCoefData.phon$Predictor <- factor(lexSubsCoefData.phon$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Semantic Distance","Phonetic Distance","Backward Predictability","Unconditional PMI","Relative Backward Predictability","Conditional PMI")) 

lexSubsCoefData.phon.filtered <- lexSubsCoefData.phon %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Relative Backward Predictability")))

lexSubsCoefData.phon.filtered$Model <- factor(lexSubsCoefData.phon.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 

lexSubs.colors <- c('#3E8D31','#295C9D','#882B8D','#f89441')
plt.modelCoeffs.phon <- ggplot(lexSubsCoefData.phon.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = lexSubs.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance, 
                y = ifelse(Beta > 0, Beta + 0.3, Beta - 0.3)), 
            size = 5) +
  facet_wrap(~Model,scales = "free_x") +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),  # x-axis text
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=10))+  # facet labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.margin = margin(20, 5.5, 5.5, 40))  +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold")) +
  ggtitle("Phonologically-Related Substitutions")

plt.modelCoeffs.phon

### mixed errors
lexSubsCoefData.mixed <- read_csv('results/model_coefficients_mixed.csv')
head(lexSubsCoefData.mixed)

lexSubsCoefData.mixed$significance <- ifelse(lexSubsCoefData.mixed$p < 0.001, "***",
                                            ifelse(lexSubsCoefData.mixed$p < 0.01, "**",
                                                   ifelse(lexSubsCoefData.mixed$p < 0.05, "*", "")))

head(lexSubsCoefData.mixed)

lexSubsCoefData.mixed$Construct <- factor(lexSubsCoefData.mixed$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future","Communicative Reward")) 

lexSubsCoefData.mixed$Predictor <- factor(lexSubsCoefData.mixed$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Semantic Distance","Phonetic Distance","Backward Predictability","Unconditional PMI","Relative Backward Predictability","Conditional PMI")) 

lexSubsCoefData.mixed.filtered <- lexSubsCoefData.mixed %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Relative Backward Predictability")))

lexSubsCoefData.mixed.filtered$Model <- factor(lexSubsCoefData.mixed.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 

lexSubs.colors <- c('#3E8D31','#295C9D','#882B8D','#f89441')
plt.modelCoeffs.mixed <- ggplot(lexSubsCoefData.mixed.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = lexSubs.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance, 
                y = ifelse(Beta > 0, Beta + 0.4, Beta - 1.1)), 
            size = 5) +
  facet_wrap(~Model,scales = "free_x") +
  ylab('Estimated Effect Sizes') +
  xlab('Key Predictors') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=10))+  # facet labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.margin = margin(20, 5.5, 5.5, 40))  +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold")) +
  ggtitle("Mixed Error Substitutions")

plt.modelCoeffs.mixed

### morphosyntactic
lexSubsCoefData.ms <- read_csv('results/model_coefficients_ms.csv')
head(lexSubsCoefData.ms)

lexSubsCoefData.ms$significance <- ifelse(lexSubsCoefData.ms$p < 0.001, "***",
                                             ifelse(lexSubsCoefData.ms$p < 0.01, "**",
                                                    ifelse(lexSubsCoefData.ms$p < 0.05, "*", "")))

head(lexSubsCoefData.ms)

lexSubsCoefData.ms$Construct <- factor(lexSubsCoefData.ms$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future","Communicative Reward")) 

lexSubsCoefData.ms$Predictor <- factor(lexSubsCoefData.ms$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Semantic Distance","Phonetic Distance","Backward Predictability","Unconditional PMI","Relative Backward Predictability","Conditional PMI")) 

lexSubsCoefData.ms.filtered <- lexSubsCoefData.ms %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Relative Backward Predictability")))

lexSubsCoefData.ms.filtered$Model <- factor(lexSubsCoefData.ms.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 

lexSubs.colors <- c('#3E8D31','#295C9D','#882B8D','#f89441')
plt.modelCoeffs.ms <- ggplot(lexSubsCoefData.ms.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = lexSubs.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance, 
                y = ifelse(Beta > 0, Beta + 0.4, Beta - 0.9)), 
            size = 5) +
  facet_wrap(~Model,scales = "free_x") +
  ylab('') +
  xlab('Key Predictors') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black"),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black"),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black"),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=10))+  # facet labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(plot.margin = margin(20, 5.5, 5.5, 40))  +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold")) +
  ggtitle("Morphosyntactic Substitutions")

plt.modelCoeffs.ms

p <- (plt.modelCoeffs.sem + plt.modelCoeffs.phon) / (plt.modelCoeffs.mixed + plt.modelCoeffs.ms)
p
ggsave("plots/outputs/Study2_FinegrainedSubstitutionsAnalysis.pdf", p , width = 16, height = 12) 
