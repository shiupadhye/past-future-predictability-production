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
durCoefData <- read_csv('results/model_coefficients.csv')
head(durCoefData)

durCoefData$significance <- ifelse(durCoefData$p < 0.001, "***",
                                   ifelse(durCoefData$p < 0.01, "**",
                                          ifelse(durCoefData$p < 0.05, "*", "ns")))

head(durCoefData)

durCoefData$Construct <- factor(durCoefData$Construct, levels = c("Frequency", "Predictability from the past","Predictability from the future")) 

durCoefData$Predictor <- factor(durCoefData$Predictor, levels = c("Unigram Predictability", "Forward Predictability","Backward Predictability","Relative Backward Predictability","Unconditional PMI","Conditional PMI")) 

## comparison of relative backward predictability and conditional PMI
durCoefData.filtered <- durCoefData %>%
  filter(!(Model %in% c("Model with Backward Predictability","Model with Unconditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Backward Predictability", "Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Backward Predictability", "Relative Backward Predictability")))

durCoefData.filtered$Model <- factor(durCoefData.filtered$Model, levels = c("Model with Relative Backward Predictability","Model with Conditional PMI")) 


dur.colors <- c('#3E8D31','#295C9D','#882B8D')
plt.modelCoeffs <- ggplot(durCoefData.filtered,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = dur.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(aes(label = significance), 
            position = position_dodge(width = 0.8), vjust = 1.8,size = 6) +
  facet_wrap(
    ~ Model,
    scales = "free_x",
    labeller = labeller(
      Model = function(x) str_replace_all(x, regex("Delta", ignore_case = TRUE), "\u0394")
    )
  ) +
  ylab('Estimated Effect Size') +
  xlab('Key Predictors') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=12),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=12),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=14),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=14),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=12),  # facet labels
    legend.position = "none") +
  scale_x_discrete(labels = function(x) {
    x <- str_replace_all(x, regex("Delta", ignore_case = TRUE), "\u0394")
    str_wrap(x, width = 10)
  })

plt.modelCoeffs

## comparison of backward predictability variants
durCoefData.all <- durCoefData %>%
  filter(!(Model == "Model with Backward Predictability" & Predictor %in% c("Relative Backward Predictability", "Unconditional PMI","Conditional PMI"))) %>%
  filter(!(Model == "Model with Unconditional PMI" & Predictor %in% c("Backward Predictability","Relative Backward Predictability", "Conditional PMI"))) %>%
  filter(!(Model == "Model with Relative Backward Predictability" & Predictor %in% c("Backward Predictability","Unconditional PMI","Conditional PMI"))) %>%
  filter(!(Model == "Model with Conditional PMI" & Predictor %in% c("Backward Predictability","Relative Backward Predictability", "Unconditional PMI")))

durCoefData.all$Model <- factor(durCoefData.all$Model, levels = c("Model with Backward Predictability","Model with Relative Backward Predictability","Model with Unconditional PMI","Model with Conditional PMI")) 

plt.modelCoeffs.all <- ggplot(durCoefData.all,aes(x=Predictor,y=Beta,fill=Construct)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.8)) +
  scale_fill_manual(values = dur.colors) +  # Using hex colors
  geom_errorbar(aes(ymin=Beta-SE,ymax=Beta+SE),position = position_dodge(width = 0.8),width=0.2)+
  geom_text(
    aes(
      y = ifelse(Beta > 0, Beta + SE + 1.95, Beta - SE - 1.95),
      label = significance
    ),
    position = position_dodge(width = 0.8),
    size = 6
  ) +
  facet_grid(
    ~ Model,
    scales = "free_x",
    labeller = labeller(
      Model = function(x) str_replace_all(x, regex("Delta", ignore_case = TRUE), "\u0394")
    )
  ) +
  ylab('Estimated Effect Size') +
  xlab('Key Predictors') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=12),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=12),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=14),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=14),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),  # plot title
    strip.text = element_text(face = "bold", color = "black",size=12),  # facet labels
    legend.position = "none") +
  scale_x_discrete(labels = function(x) {
    x <- str_replace_all(x, regex("Delta", ignore_case = TRUE), "\u0394")
    str_wrap(x, width = 10)
  })

plt.modelCoeffs.all


# Loglik
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
  ylim(0,5500) +
  
  geom_signif(
    comparisons = comparisons,
    annotations = c("***","***","***"),
    y_position = y_positions,
    tip_length = 0.02,
    textsize = 5,
    size = 0.8
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=12),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=12),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=14),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=14),  # y-axis title
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
  mutate(label = "Study 1: Modeling Durations")
  
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
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black",size=12),  # x-axis text
    axis.text.y = element_text(face = "bold", color = "black",size=12),  # y-axis text
    axis.title.x = element_text(face = "bold", color = "black",size=14),  # x-axis title
    axis.title.y = element_text(face = "bold", color = "black",size=14),  # y-axis title
    plot.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black",size=15)
  )

plt.modelComp.BIC 


## correlation plot
scores <- read_csv('SWBD_DurAnalysisData.csv')
scores <- scores %>%
  mutate(rel_bwPred = infillBw_logProb - infillFw_logProb,
         uncond_pmi = infillBw_logProb - unigram_logProb)

pred.scores.infillOnly <- scores[,c('unigram_logProb','infillFw_logProb','infillBw_logProb','rel_bwPred','uncond_pmi','infill_pmiFP')]

pred.scores.infillOnly <- pred.scores.infillOnly %>%
  rename("Unigram Predictability" = "unigram_logProb",
         "Forward Predictability" = "infillFw_logProb",
         "Backward Predictability" = "infillBw_logProb",
         "Relative Backward Predictability" = "rel_bwPred",
         "Unconditional PMI" = "uncond_pmi",
         "Conditional PMI" = "infill_pmiFP")


pred.scores.infillOnly.corr <- cor(pred.scores.infillOnly,use = "pairwise.complete.obs")
corr_melt.subset <- melt(pred.scores.infillOnly.corr)
#corr_melt.subset <- corr_melt.subset[as.numeric(corr_melt.subset$Var1) > as.numeric(corr_melt.subset$Var2), ]

corr_melt.subset$label <- sprintf("r = %.2f", corr_melt.subset$value)

# Plot
cor.plot.infillOnly <- ggplot(corr_melt.subset, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = label), color = "black", size = 4, fontface = "bold")+
  scale_fill_viridis(
    option = "viridis",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  ggtitle("Pairwise correlations between probabilistic variables") +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold") 
  )
  
cor.plot.infillOnly   

## corr (between models)

pred.scores.bwModels <- scores[,c('AutoregFw_logProb','AutoregBw_logProb','infillFw_logProb','infillBw_logProb')]

pred.scores.bwModels  <- pred.scores.bwModels %>%
  rename("Forward Predictability (separate model)" = "AutoregFw_logProb",
         "Backward Predictability (separate model)" = "AutoregBw_logProb",
         "Forward Predictability (infill model)" = "infillFw_logProb",
         "Backward Predictability (infill model)" = "infillBw_logProb")


pred.scores.bwModels.corr <- cor(pred.scores.bwModels,use = "pairwise.complete.obs")
corr_melt.bwModels <- melt(pred.scores.bwModels.corr)
#corr_melt.subset <- corr_melt.subset[as.numeric(corr_melt.subset$Var1) > as.numeric(corr_melt.subset$Var2), ]

corr_melt.bwModels$label <- sprintf("r = %.2f", corr_melt.bwModels$value)

# Plot
cor.plot.bwModels <- ggplot(corr_melt.bwModels, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = label), color = "black", size = 4, fontface = "bold")+
  scale_fill_viridis(
    option = "viridis",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  #ggtitle("Pairwise correlations between contextual variables estimated from seperate and infill-trained models") +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold") 
  )

cor.plot.bwModels

## corr (all predictors)

pred.scores <- scores[,c('unigram_logProb','AutoregFw_logProb','AutoregBw_logProb','infillFw_logProb','infillBw_logProb','rel_bwPred','uncond_pmi','infill_pmiFP')]

pred.scores <- pred.scores %>%
  rename("Unigram Predictability" = "unigram_logProb",
         "Forward Predictability (separate model)" = "AutoregFw_logProb",
         "Backward Predictability (separate mdoel)" = "AutoregBw_logProb",
         "Forward Predictability" = "infillFw_logProb",
         "Backward Predictability" = "infillBw_logProb",
         "Relative Backward Predictability" = "rel_bwPred",
         "Unconditional PMI" = "uncond_pmi",
         "Conditional PMI" = "infill_pmiFP")


pred.scores.corr <- cor(pred.scores,use = "pairwise.complete.obs")
corr_melt <- melt(pred.scores.corr)
#corr_melt.subset <- corr_melt.subset[as.numeric(corr_melt.subset$Var1) > as.numeric(corr_melt.subset$Var2), ]

corr_melt$label <- sprintf("r = %.2f", corr_melt$value)

# Plot
cor.plot <- ggplot(corr_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = label), color = "black", size = 4, fontface = "bold")+
  scale_fill_viridis(
    option = "viridis",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  ggtitle("Pairwise correlations between probabilistic variables") +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold") 
  )

cor.plot  

# save
plt.study1.modelComp <- plt.modelCoeffs + plt.modelComp.LL + plot_layout(widths = c(2, 1))

ggsave("plots/outputs/Study1_ModelCoeffs&LRT.pdf", plt.study1.modelComp, width = 14, height = 8) 

ggsave("plots/outputs/Study1_ModelCoeffs_all.pdf", plt.modelCoeffs.all, width = 16, height = 6) 

ggsave("plots/outputs/Study1_ModelComp_BIC.pdf", plt.modelComp.BIC , width = 10, height = 8) 

ggsave("plots/outputs/Study1_CorrPlotInfill.pdf", cor.plot.infillOnly, width = 8, height = 8) 

ggsave("plots/outputs/Study1_CorrPlotBwModels.pdf", cor.plot.bwModels, width = 8, height = 8) 

ggsave("plots/outputs/Study1_CorrPlot.pdf", cor.plot, width = 12, height = 12) 

# interaction plots
int.plot1 <- readRDS('plots/RDS/interaction_plot_freq.rds')
int.plot1 <- int.plot1 + theme(legend.position = "none")

int.plot2 <- readRDS('plots/RDS/interaction_plot_fwPred.rds')

int.plot3 <- readRDS('plots/RDS/interaction_plot_bwPred.rds')
int.plot3 <- int.plot3 + theme(legend.position = "none") +
  xlab("Relative Backward Predictability")

int.plot4 <- readRDS('plots/RDS/interaction_plot_PMI.rds')
int.plot4 <- int.plot4 + theme(legend.position = "none")

plt.combined <- (int.plot1  + int.plot2) / (int.plot3 + int.plot4)
p_grob <- patchworkGrob(plt.combined)
# draw both together
grid.newpage()
grid.draw(p_grob)
grid.text("Predicted Duration (ms)", x = 0.02, y = 0.5, rot = 90,
          gp = gpar(fontsize = 14, fontface = "bold"))

ggsave("plots/outputs/Study1_LexicalCategoryProbabilisticReduction.pdf", plt.combined, width = 12, height = 8) 
