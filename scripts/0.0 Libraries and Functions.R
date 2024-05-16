

# Load Libraries ----------------------------------------------------------

library(metafor)
library(brms)
library(tidyverse)
library(ggpubr)
library(R.matlab)
library(openxlsx)
library(tidybayes)

# Define Custom Functions -------------------------------------------------

# Calculates d' with a correction
calc_d = function(hits, fas, cor = 0)
{
  if(hits == 1) 
    hits = 1 - cor
  else if(hits == 0)
    hits = cor
  if(fas == 0)
    fas = cor
  else if(fas == 1)
    fas = 1 - cor
  
  return(qnorm(hits) - qnorm(fas))
}

# Calculate SE
calc_se = function(x){sd(x)/length(x)**.5}

# Calculates C with a correction
calc_c = function(hits, fas, cor = 0)
{
  if(hits == 1) 
    hits = 1 - cor
  else if(hits == 0)
    hits = cor
  if(fas == 0)
    fas = cor
  else if(fas == 1)
    fas = 1 - cor
  
  return(((qnorm(hits) + qnorm(fas))/2)*-1)
}
