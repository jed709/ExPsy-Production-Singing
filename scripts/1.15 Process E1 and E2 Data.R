
# load E1 data and summary stats ---------------------------------------------------------------

### E1

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

# Test phase summary
e1_test_dat %>%
  group_by(participant, condition) %>%
  summarize(n=n(), m=mean(conf > 3), r=mean(rkn=='r'), f_raw=mean(rkn=='f'), recfam=mean(rkn=='r'|rkn=='f'), f=mean(rkn[rkn!='r']=='f')) -> e1_test_sum

# d' calculations
e1_test_sum %>%
  select(participant, condition, m) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'm') %>%
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

### e2

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


# basic summary stuff, calc d, etc ----------------------------------------


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

# bind data together -----------------------


e12_test_sum = rbind(e1_test_sum %>% mutate(exp='1'), e2_test_sum %>% mutate(exp='2')) %>%
  mutate(condition = recode(condition, 'None' = 'new')) %>%
  mutate(participant = paste(exp, participant))
  
e12_d_sum = rbind(e1_d_sum %>% mutate(exp='1'), e2_d_sum %>% mutate(exp='2')) %>%
  mutate(participant = paste(exp, participant))

e12_d_sum_rec = rbind(e1_d_sum_rec %>% mutate(exp='1'), e2_d_sum_rec %>% mutate(exp='2')) %>%
  mutate(participant = paste(exp, participant))

e12_d_sum_fam = rbind(e1_d_sum_fam %>% mutate(exp='1'), e2_d_sum_fam %>% mutate(exp='2')) %>%
  mutate(participant = paste(exp, participant))

temp1 = e1_test_dat %>% select(participant, words, condition, conf, rkn) %>%
  mutate(condition = recode(condition, 'None' = 'new'), exp = '1')
temp2 = e2_test_dat %>% select(participant, words, condition, conf, rkn) %>%
  mutate(condition = recode(condition, 'None' = 'new',
                            'aloud' = 'speak',
                            'silent' = 'read'), exp = '2')
e12_test_dat = rbind(temp1, temp2) %>%
  mutate(participant = paste(exp, participant))

# probit models ---------------------------------------------------------

# modify data frame so it is appropriate for the models

e12_test_dat <- e12_test_dat %>%
  mutate(condition = factor(condition)) %>%
  mutate(condition = relevel(condition, 'new'),
         is_old = as.numeric(condition != 'new') - 0.5,
         said_old = as.numeric(conf > 3),
         said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn == 'f'))

e12_fd <- e12_test_dat %>%
  filter(said_rec != 1)

# specify Priors 
Prior <- c(prior(normal(1, 1), class = "b", coef=conditionread),
           prior(normal(1, 1), class = "b", coef=conditionsing),
           prior(normal(1, 1), class = "b", coef=conditionspeak),
           prior(normal(0, 1), class = "sd"),
           prior(normal(-1, 1), class = "Intercept"),
           prior(lkj(4), class = "cor"))


# fit model

e12_conf_large <- brm(said_old~condition + (condition|participant) + (condition|words),
                   family = bernoulli(link="probit"),
                   data = e12_test_dat,
                   backend='cmdstanr',
                   prior = Prior,
                   control = list(adapt_delta = .9),
                   cores = 8, chains = 8, iter = 15000, warmup = 7500,
                   sample_prior = 'yes')

# plot conf

e12_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E1a/b Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e12_conf_plot

# plot conf contrasts

e12_conf_large %>%
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
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e12_conf_con_plot 

# fit rec model


e12_rec_large <- brm(said_rec~condition + (condition|participant) + (condition|words),
                   family = bernoulli(link="probit"),
                   data = e12_test_dat,
                   backend='cmdstanr',
                   prior = Prior,
                   control = list(adapt_delta = .9),
                   cores = 8, chains = 8, iter = 15000, warmup = 7500,
                   sample_prior = 'yes')

# plot rec

e12_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E1a/b Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e12_rec_plot

# plot rec contrasts

e12_rec_large %>%
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
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e12_rec_con_plot

# fit fam model

e12_fam_large <- brm(said_fam~condition + (condition|participant) + (condition|words),
                  family = bernoulli(link="probit"),
                  data = e12_fd,
                  backend='cmdstanr',
                  prior = Prior,
                  control = list(adapt_delta = .9),
                  cores = 8, chains = 8, iter = 15000, warmup = 7500,
                  sample_prior = 'yes',
                  file = 'models/e12_fam_large')

# plot fam

e12_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_conditionspeak',
         Sing = 'b_conditionsing',
         Silent = 'b_conditionread') %>%
  select(Aloud:Silent) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Post') %>%
  mutate(Contrast = factor(Contrast, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Contrast, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E1a/b Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,1.5), breaks = seq(0, 1.5, 0.5)) +
  theme_classic() -> e12_fam_plot

# plot fam contrasts

e12_fam_large %>%
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
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e12_con_fam_plot

ggarrange(e12_conf_plot, e12_conf_con_plot, 
          e12_rec_plot, e12_rec_con_plot,
          e12_fam_plot, e12_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1))
