#### Raincloud Plots 3.0 for Sina ####

rm(list=ls())

# Packages, directory etc. ----

# install packages
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(ppcor)) install.packages("ppcor")
library(ppcor)
if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)
if (!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)
if (!require(sjmisc)) install.packages("sjmisc")
library(sjmisc)
if (!require(Rmisc)) install.packages("Rmisc")
library(Rmisc)
if (!require(cowplot)) install.packages("cowplot")
library(cowplot)
if (!require(remotes)) install.packages("remotes")
#remotes::install_github('jorvlan/raincloudplots')
if (!require(raincloudplots)) install.packages("raincloudplots")
library(raincloudplots)
if (!require(wesanderson)) install.packages("wesanderson")
library(wesanderson)
source("/Users/nimaheimbach/Desktop/sina_plots/R_rainclouds.R")

# working directory 

setwd("/Users/nimaheimbach/Desktop/sina_plots")

# load data 

long <- read.table('overview_all_long_bothSeeds.csv', header = T, sep = ',')

# define colors
single <- "#00AFBB"
mixed <- "#009E73"
rep <- "#E7B800"
switch <- "#D55E00"


# define h and w
h = 7
w = 7

#Plots of Extracted Sustained Activation Parameters (single & mixed) ----

### bilateral Precuneus ####
Pc_sust <- subset(long, long$condition == 'single' | long$condition == 'mixed', select = c('ID','group','condition','bil_Pc_transient'))
colnames(Pc_sust) <- c('ID','group','condition','parameter')
# condition as factor + sort
Pc_sust$condition <- as.factor(Pc_sust$condition)
Pc_sust$condition <- factor(Pc_sust$condition, levels = c('single','mixed'))

summary_simdat <- summarySE(Pc_sust, measurevar = "parameter", groupvars = c("condition","group"),na.rm = T)

p <- ggplot(Pc_sust,aes(x=condition,y=parameter,fill=condition,col=condition))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .6,adjust =4)+
  geom_point(aes(x = as.numeric(condition), y = parameter, colour = condition),  size = .25, shape = 20)+
  geom_line(aes(x=as.numeric(condition), group=ID), color = 'lightgray', alpha = .3, size = .25) +
  geom_point(data = summary_simdat, aes(x = as.numeric(condition)+.1, y = parameter, group = condition, colour = condition), shape = 18) + 
  geom_errorbar(data = summary_simdat, aes(x = as.numeric(condition)+.1, y = parameter, group = condition, colour = condition, ymin = parameter-ci, ymax =parameter+ci), width = .05, colour = "black") + xlab('Condition') + ylab('Parameter')+
  scale_fill_manual(values = c("#00AFBB","#009E73"))+
  scale_color_manual(values = c("#00AFBB","#009E73"))+
  guides(fill = FALSE, col = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA) ) +
  facet_wrap(~ group) 

p












# old; with jitter but then lines dont connect
#p <- ggplot(Pc_sust,aes(x=condition,y=parameter,fill=condition,col=condition))+
#  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .6,adjust =4)+
#  geom_point(aes(x = as.numeric(condition), y = parameter, colour = condition), position = position_jitter(width = .05, seed = 123), size = .25, shape = 20)+
#  geom_line(aes(x=as.numeric(condition), group=ID), color = 'lightgray', alpha = .3, position = position_jitter(width = .05, seed = 123)) +
#  geom_point(data = summary_simdat, aes(x = as.numeric(condition)+.1, y = parameter, group = condition, colour = condition), shape = 18) + 
#  geom_errorbar(data = summary_simdat, aes(x = as.numeric(condition)+.1, y = parameter, group = condition, colour = condition, ymin = parameter-ci, ymax =parameter+ci), width = .05, colour = "black") + xlab('Condition') + ylab('Parameter')+
#  scale_fill_manual(values = c("#00AFBB","#009E73"))+
#  scale_color_manual(values = c("#00AFBB","#009E73"))+
#  guides(fill = FALSE, col = FALSE) +
#  theme_bw() +
#  theme(
#      panel.grid.major = element_blank(), 
#      panel.grid.minor = element_blank(),
#      panel.background = element_rect(fill = "transparent",colour = NA),
#      plot.background = element_rect(fill = "transparent",colour = NA) ) +
#  facet_wrap(~ group) 

  
# geom_line(data = summary_simdat, aes(x = as.numeric(condition)+.1,y = parameter, group = 1), colour = "black")

