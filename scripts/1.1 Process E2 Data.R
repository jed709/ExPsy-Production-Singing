
# Note: This is the first replication study conducted by Huff using 
# unmatched foils. Experiment 1b in manuscript.

# sing1 mixed up the confidence scale (foils > studied)
# sing16 and sing17 were being combined due to a faulty PID; sing17 changed to sing17a

# Load Data ---------------------------------------------------------------

# Reading both phases at once
e2_test_dat = NULL
for(l in list.files('data/E2', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E2', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = select(dat, testWord:frameRate, participant)
  dat = dat %>% 
    select(testN, words = testWord, condition=testType, conf=keyConf.keys, conf_rt=keyConf.rt, 
           rkn=keyRKN.keys, rkn_rt=keyRKN.rt, participant)
  dat$group = substr(l, 1,1)
  
  
  e2_test_dat = rbind(e2_test_dat, dat)
}

# Remove base participants, as they have 2 aloud conditions rather than aloud and sing
e2_test_dat = e2_test_dat %>%
  filter(group=='s') %>%
  filter(!is.na(as.numeric(as.character(testN)))) %>%
  filter(!(participant %in% c('sing1'))) %>%
  mutate(conf=as.numeric(as.character(conf)), condition=recode(condition, D_new='new', A_silent='read', B_aloud='speak', C_sing='sing'))


# basic summary stats, calc d, etc ----------------------------------------

# Test phase summary
e2_test_dat %>%
  group_by(participant, condition) %>%
  summarize(n=n(), m=mean(conf > 3), r=mean(rkn=='r'), f_raw=mean(rkn=='f'), recfam=mean(rkn=='r'|rkn=='f'), f=mean(rkn[rkn!='r']=='f')) -> e2_test_sum

# d' calculations
e2_test_sum %>%
  select(participant, condition, m) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'm') %>%
  mutate(read_d = calc_d(read, new, cor = .016), 
         sing_d = calc_d(sing, new, cor = .016), 
         speak_d = calc_d(speak, new, cor = .016)) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> e2_d_sum

#d' calculations for recollection
e2_test_sum %>%
  select(participant, condition, r) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'r') %>%
  mutate(read_d_rec = calc_d(read, new, cor = .016), 
         sing_d_rec = calc_d(sing, new, cor = .016),
         speak_d_rec = calc_d(speak, new, cor = .016)) %>%
  select(participant, read_d_rec:speak_d_rec) %>%
  pivot_longer(cols = c(read_d_rec:speak_d_rec), names_to = 'condition', values_to = 'd') -> e2_d_sum_rec

#d' calculations for familiarity
e2_test_sum %>%
  select(participant, condition, f) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'f') %>%
  mutate(read_d_fam = calc_d(read, new, cor = .016), 
         sing_d_fam = calc_d(sing, new, cor = .016),
         speak_d_fam = calc_d(speak, new, cor = .016)) %>%
  select(participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'condition', values_to = 'd') -> e2_d_sum_fam

#d' calculations for familiarity raw
e2_test_sum %>%
  select(participant, condition, f_raw) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'f_raw') %>%
  mutate(read_d_fam = calc_d(read, new, cor = .016), 
         sing_d_fam = calc_d(sing, new, cor = .016),
         speak_d_fam = calc_d(speak, new, cor = .016)) %>%
  select(participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'condition', values_to = 'd') -> e2_d_sum_fam_raw

#d cor
cor(e2_d_sum[e2_d_sum$condition=='sing_d',]$d, e2_d_sum[e2_d_sum$condition=='speak_d',]$d)

cor(e2_d_sum[e2_d_sum$condition=='read_d',]$d, e2_d_sum[e2_d_sum$condition=='speak_d',]$d)

cor(e2_d_sum[e2_d_sum$condition=='sing_d',]$d, e2_d_sum[e2_d_sum$condition=='read_d',]$d)

# # Probit models ---------------------------------------------------------

# make data suitable for models

e2_test_dat <- e2_test_dat %>%
  mutate(condition = factor(condition)) %>%
  mutate(condition = relevel(condition, 'new'),
         is_old = as.numeric(condition != 'new') - 0.5,
         said_old = as.numeric(conf > 3), 
         said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn == 'f'))

# set priors

Prior <- c(prior(normal(1, 1), class = "b", coef=conditionread),
           prior(normal(1, 1), class = "b", coef=conditionsing),
           prior(normal(1, 1), class = "b", coef=conditionspeak),
           prior(normal(0, 1), class = "sd"),
           prior(normal(-1, 1), class = "Intercept"),
           prior(lkj(4), class = "cor"))

# fit conf model

e2_conf_large <- brm(said_old~condition + (condition|participant) + (condition|words),
                     family = bernoulli(link="probit"),
                     data = e2_test_dat,
                     backend='cmdstanr',
                     prior = Prior,
                     control = list(adapt_delta = .9),
                     init = 0,
                     cores = 8, chains = 8, iter = 15000, warmup = 7500,
                     sample_prior = 'yes')

# plot

e2_conf_large %>%
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
  labs(title = 'E1b Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2.0, 0.5)) +
  theme_classic() -> e2_conf_plot

e2_conf_large %>%
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
  theme_classic() -> e2_conf_con_plot

# fit model for recollection

e2_rec_large <- brm(said_rec~condition + (condition|participant) + (condition|words),
                    family = bernoulli(link="probit"),
                    data = e2_test_dat,
                    backend='cmdstanr',
                    prior = Prior,
                    control = list(adapt_delta = .9),
                    init = 0,
                    cores = 8, chains = 8, iter = 15000, warmup = 7500,
                    sample_prior = 'yes')

# plot recollection 

e2_rec_large %>%
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
  labs(title = 'E1b Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2.0, 0.5)) +
  theme_classic() -> e2_rec_plot

e2_rec_large %>%
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
  theme_classic() -> e2_rec_con_plot

# create dataframe excluding rec trials

e2_fd = e2_test_dat %>%
  filter(rkn != 'r')

# fit model for familiarity

e2_fam_large <- brm(said_fam~condition + (condition|participant) + (condition|words),
                    family = bernoulli(link="probit"),
                    data = e2_fd,
                    backend='cmdstanr',
                    prior = Prior,
                    control = list(adapt_delta = .9),
                    init = 0,
                    cores = 8, chains = 8, iter = 15000, warmup = 7500,
                    sample_prior = 'yes')

# plot familiarity 

e2_fam_large %>%
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
  labs(title = 'E1b Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2.0, 0.5)) +
  theme_classic() -> e2_fam_plot

e2_fam_large %>%
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
  theme_classic() -> e2_con_fam_plot

ggarrange(e2_conf_plot, e2_conf_con_plot, 
          e2_rec_plot, e2_rec_con_plot,
          e2_fam_plot, e2_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1))
