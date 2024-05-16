
# Note: This is the original study conducted by Ozubko + Fawcett using 
# unmatched foils. Experiment 1a in manuscript.
# sing_subj14 did not complete the experiment

# Load Data ---------------------------------------------------------------

# Study phase
e1_study_dat = read.csv('data/E1/sing_merged2.csv') %>%
  filter(!is.na(as.numeric(as.character(StudyTrials.thisTrialN)))) %>%
  select(words, oldNew, condition, participant, date)

# Test phase
e1_test_dat = read.csv('data/E1/sing_merged2.csv') %>%
  filter(!is.na(as.numeric(as.character(TestTrials.thisRepN)))) %>%
  select(words, oldNew, condition, participant, conf=keyConf.keys, conf_rt=keyConf.rt, 
         rkn=keyRKN.keys, rkn_rt=keyRKN.rt) %>%
  mutate(conf=as.numeric(as.character(conf))) %>%
  filter(participant != 'sing_subj14real') %>% 
  left_join(e1_study_dat %>% select(participant, words, condition)) 

# basic summary stats, calc d, etc. ---------------------------------------

# Test phase summary
e1_test_dat %>%
  group_by(participant, condition) %>%
  summarize(n=n(), m=mean(conf > 3), r=mean(rkn=='r'), f_raw=mean(rkn=='f'), recfam=mean(rkn=='r'|rkn=='f'), f=mean(rkn[rkn!='r']=='f')) -> e1_test_sum

# d' calculations
e1_test_sum %>%
  select(participant, condition, m) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'm') %>%
  # corrections for calculating d' conventionally to ensure no values of + or - Inf;
  # .016 derived from 1/2 the smallest proportion of hits in a given condition
  mutate(read_d = calc_d(read, None, cor = .016), 
         sing_d = calc_d(sing, None, cor = .016), 
         speak_d = calc_d(speak, None, cor = .016)) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> e1_d_sum

#d' calculations for recollection
e1_test_sum %>%
  select(participant, condition, r) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'r') %>%
  mutate(read_d_rec = calc_d(read, None, cor = .016), 
         sing_d_rec = calc_d(sing, None, cor = .016),
         speak_d_rec = calc_d(speak, None, cor = .016)) %>%
  select(participant, read_d_rec:speak_d_rec) %>%
  pivot_longer(cols = c(read_d_rec:speak_d_rec), names_to = 'condition', values_to = 'd') -> e1_d_sum_rec

#d' calculations for familiarity
e1_test_sum %>%
  select(participant, condition, f) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'f') %>%
  mutate(read_d_fam = calc_d(read, None, cor = .016), 
         sing_d_fam = calc_d(sing, None, cor = .016),
         speak_d_fam = calc_d(speak, None, cor = .016)) %>%
  select(participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'condition', values_to = 'd') -> e1_d_sum_fam

#d' calculations for familiarity raw
e1_test_sum %>%
  select(participant, condition, f_raw) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'f_raw') %>%
  mutate(read_d_fam = calc_d(read, None, cor = .016), 
         sing_d_fam = calc_d(sing, None, cor = .016),
         speak_d_fam = calc_d(speak, None, cor = .016)) %>%
  select(participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'condition', values_to = 'd') -> e1_d_sum_fam_raw

#d cor
cor(e1_d_sum[e1_d_sum$condition=='sing_d',]$d, e1_d_sum[e1_d_sum$condition=='speak_d',]$d)

cor(e1_d_sum[e1_d_sum$condition=='read_d',]$d, e1_d_sum[e1_d_sum$condition=='speak_d',]$d)

cor(e1_d_sum[e1_d_sum$condition=='sing_d',]$d, e1_d_sum[e1_d_sum$condition=='read_d',]$d)


#Probit models ---------------------------------------------------------

# modify dataframe to be suitable for models

e1_test_dat = e1_test_dat %>%
  mutate(condition = factor(condition)) %>%
  mutate(condition = relevel(condition, 'None'),
         is_old = as.numeric(condition != 'None') - 0.5,
         said_old = as.numeric(conf > 3),
         said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn =='f'))

# Set Priors

Prior <- c(prior(normal(1, 1), class = "b", coef=conditionread),
           prior(normal(1, 1), class = "b", coef=conditionsing),
           prior(normal(1, 1), class = "b", coef=conditionspeak),
           prior(normal(0, 1), class = "sd"),
           prior(normal(-1, 1), class = "Intercept"),
           prior(lkj(4), class = "cor"))

# fit model

e1_conf_large <- brm(said_old~condition + (condition|participant) + (condition|words),
                      family = bernoulli(link="probit"),
                      data = e1_test_dat,
                      backend='cmdstanr',
                      prior = Prior,
                      control = list(adapt_delta = .9),
                      init = 0,
                      cores = 8, chains = 8, iter = 15000, warmup = 7500,
                      sample_prior = 'yes')

#plots

e1_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  labs(title = 'E1a Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e1_conf_plot

e1_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e1_conf_con_plot

# fit model for recollection

e1_rec_large <- brm(said_rec~condition + (condition|participant) + (condition|words),
                     family = bernoulli(link="probit"),
                     data = e1_test_dat,
                     backend='cmdstanr',
                     prior = Prior,
                     control = list(adapt_delta = .9),
                     init = 0,
                     cores = 8, chains = 8, iter = 15000, warmup = 7500,
                     sample_prior = 'yes')

# plot recollection 

e1_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  labs(title = 'E1a Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e1_rec_plot

e1_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e1_rec_con_plot

# create dataframe excluding rec trials

e1_fd = e1_test_dat %>%
  filter(rkn != 'r')

# fit model for familiarity

e1_fam_large <- brm(said_fam~condition + (condition|participant) + (condition|words),
                     family = bernoulli(link="probit"),
                     data = e1_fd,
                     backend='cmdstanr',
                     prior = Prior,
                     control = list(adapt_delta = .9),
                     init = 0,
                     cores = 8, chains = 8, iter = 15000, warmup = 7500,
                     sample_prior = 'yes')

# plot familiarity 

e1_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  labs(title = 'E1a Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e1_fam_plot

e1_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.7) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e1_con_fam_plot

ggarrange(e1_conf_plot, e1_conf_con_plot, 
          e1_rec_plot, e1_rec_con_plot,
          e1_fam_plot, e1_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1))