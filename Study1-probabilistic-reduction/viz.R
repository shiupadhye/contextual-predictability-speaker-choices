library(grid)
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
