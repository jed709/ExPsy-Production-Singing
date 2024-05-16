
# Note: This is the second replication study conducted by Huff using 
# matched foils. Experiment 2 in manuscript.

# Load Data ---------------------------------------------------------------

# Reading both phases at once
e3_test_dat = NULL
for(l in list.files('data/E3', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E3', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = dat %>% 
    select(participant, testN, words = testWord, condition=testType, participant, conf=keyConf.keys, conf_rt=keyConf.rt, 
           rkn=keyRKN.keys, rkn_rt=keyRKN.rt)
  dat$group = substr(l, 1,1)
  
  
  e3_test_dat = rbind(e3_test_dat, dat)
}

# Create matched foils
e3_test_dat = e3_test_dat %>%
  filter(!is.na(as.numeric(as.character(testN)))) %>%
  mutate(instruction = str_replace(condition, 'new_', ''), is_old = as.numeric(condition %in% c('silent', 'aloud', 'sing')), is_old_cent = is_old - .5) %>%
  mutate(conf=as.numeric(as.character(conf)), condition=recode(condition, new_a='new_aloud', new_b='new_silent', new_c='new_sing')) %>%
  mutate(said_old = as.numeric(conf > 3), instruction = recode(instruction, a = 'aloud', b = 'silent', c = 'sing'))


# Summary stats -----------------------------------------------------------


e3_test_dat %>%
  mutate(group=tolower(group)) %>%
  group_by(group, participant, instruction, is_old) %>%
  summarize(n=n(), m=mean(conf > 3), r=mean(rkn=='r'), f_raw=mean(rkn=='f'), recfam=mean(rkn=='r'|rkn=='f'), f=mean(rkn[rkn!='r']=='f'), new=mean(conf < 4)) -> e3_test_sum

# d' calculations
e3_test_sum %>%
  select(group, participant, instruction, is_old, m) %>%
  mutate(condition = paste0(instruction, is_old)) %>%
  select(-instruction, -is_old) %>%
  pivot_wider(id_cols = c('participant', 'group'), names_from = 'condition', values_from = 'm') %>%
  mutate(read_d = calc_d(silent1, silent0, cor = .016), 
         sing_d = calc_d(sing1, sing0, cor = .016), 
         speak_d = calc_d(aloud1, aloud0, cor = .016)) %>%
  select(group, participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'instruction', values_to = 'd') -> e3_d_sum

# d' calculations for recollection
e3_test_sum %>%
  select(group, participant, instruction, is_old, r) %>%
  mutate(condition = paste0(instruction, is_old)) %>%
  select(-instruction, -is_old) %>%
  pivot_wider(id_cols = c('participant', 'group'), names_from = 'condition', values_from = 'r') %>%
  mutate(read_d_rec = calc_d(silent1, silent0, cor = .016), 
         sing_d_rec = calc_d(sing1, sing0, cor = .016), 
         speak_d_rec = calc_d(aloud1, aloud0, cor = .016)) %>%
  select(group, participant, read_d_rec:speak_d_rec) %>%
  pivot_longer(cols = c(read_d_rec:speak_d_rec), names_to = 'instruction', values_to = 'd') -> e3_d_sum_rec

# d' calculations for familiarity
e3_test_sum %>%
  select(group, participant, instruction, is_old, f) %>%
  mutate(condition = paste0(instruction, is_old)) %>%
  select(-instruction, -is_old) %>%
  pivot_wider(id_cols = c('participant', 'group'), names_from = 'condition', values_from = 'f') %>%
  mutate(read_d_fam = calc_d(silent1, silent0, cor = .016), 
         sing_d_fam = calc_d(sing1, sing0, cor = .016), 
         speak_d_fam = calc_d(aloud1, aloud0, cor = .016)) %>%
  select(group, participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'instruction', values_to = 'd') -> e3_d_sum_fam

# d' calculations for familiarity raw
e3_test_sum %>%
  select(group, participant, instruction, is_old, f_raw) %>%
  mutate(condition = paste0(instruction, is_old)) %>%
  select(-instruction, -is_old) %>%
  pivot_wider(id_cols = c('participant', 'group'), names_from = 'condition', values_from = 'f_raw') %>%
  mutate(read_d_fam = calc_d(silent1, silent0, cor = .016), 
         sing_d_fam = calc_d(sing1, sing0, cor = .016), 
         speak_d_fam = calc_d(aloud1, aloud0, cor = .016)) %>%
  select(group, participant, read_d_fam:speak_d_fam) %>%
  pivot_longer(cols = c(read_d_fam:speak_d_fam), names_to = 'instruction', values_to = 'd') -> e3_d_sum_fam_raw

#d cor matched
cor(e3_d_sum[e3_d_sum$instruction=='sing_d' & e3_d_sum$group =='c',]$d, 
    e3_d_sum[e3_d_sum$instruction=='speak_d' & e3_d_sum$group == 'c',]$d)
#d cor unmatched
cor(e3_d_sum[e3_d_sum$instruction=='sing_d' & e3_d_sum$group =='n',]$d, 
    e3_d_sum[e3_d_sum$instruction=='speak_d' & e3_d_sum$group == 'n',]$d)
#d cor comb
cor(e3_d_sum[e3_d_sum$instruction=='sing_d',]$d, 
    e3_d_sum[e3_d_sum$instruction=='speak_d',]$d)

#d cor matched
cor(e3_d_sum[e3_d_sum$instruction=='read_d' & e3_d_sum$group =='c',]$d, 
    e3_d_sum[e3_d_sum$instruction=='speak_d' & e3_d_sum$group == 'c',]$d)

cor(e3_d_sum[e3_d_sum$instruction=='read_d' & e3_d_sum$group =='c',]$d, 
    e3_d_sum[e3_d_sum$instruction=='sing_d' & e3_d_sum$group == 'c',]$d)
#d cor unmatched
cor(e3_d_sum[e3_d_sum$instruction=='read_d' & e3_d_sum$group =='n',]$d, 
    e3_d_sum[e3_d_sum$instruction=='speak_d' & e3_d_sum$group == 'n',]$d)

cor(e3_d_sum[e3_d_sum$instruction=='read_d' & e3_d_sum$group =='n',]$d, 
    e3_d_sum[e3_d_sum$instruction=='sing_d' & e3_d_sum$group == 'n',]$d)

# Probit Models -------------------------------------------------------------d

### make data suitable for models

e3_test_dat = e3_test_dat %>%
  mutate(group = tolower(group))

# formula for conf

e3m4 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

# specify priors

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(normal(0, 1), nlpar = "dprime", class = "sd"),
  prior(normal(0, 1), nlpar = "c"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),
  prior(lkj(4), class = "cor")
)

# fit model

e3_conf_large <- brm(
  e3m4,
  family = bernoulli(link = "identity"),
  data = e3_test_dat,
  prior = Priors,
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  backend = 'cmdstanr',
  sample_prior = 'yes'
)

# plot conf

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_conf_plot

# plot conf contrasts

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                               grepl('SS', Contrast) ~ 'Sing-Silent',
                               grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_conf_con_plot

# plots for c

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Confidence', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_conf_plot_c

# plot c conf contrasts

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_conf_con_plot_c

### rec and fam

# create binarized rec and fam data

e3_test_dat <- e3_test_dat %>%
  mutate(said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn == 'f'))

e3_fd <- e3_test_dat %>%
  filter(said_rec != 1)

# specify formulae

e3rm1 <- bf(
  said_rec ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

e3fm1 <- bf(
  said_fam ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

# fit models

e3_rec_large <- brm(
  e3rm1,
  family = bernoulli(link = "identity"),
  data = e3_test_dat,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes'
)

# plot rec

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_rec_plot

# plot rec contrasts

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_rec_con_plot

# plots for c rec

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Recollection', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_rec_plot_c

# plot c rec contrasts

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_rec_con_plot_c

# fit fam model

e3_fam_large <- brm(
  e3fm1,
  family = bernoulli(link = "identity"),
  data = e3_fd,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes'
)

# plot fam

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_fam_plot

# plot fam contrasts

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_con_fam_plot

# plot fam c

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Familiarity', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_fam_plot_c

# plot fam c contrasts

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_con_fam_plot_c

ggarrange(e3_conf_plot, e3_conf_con_plot, 
          e3_rec_plot, e3_rec_con_plot,
          e3_fam_plot, e3_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1),
          common.legend = T, legend = "bottom")

ggarrange(e3_conf_plot_c, e3_conf_con_plot_c, 
          e3_rec_plot_c, e3_rec_con_plot_c,
          e3_fam_plot_c, e3_con_fam_plot_c,
          nrow = 3, ncol = 2, widths = c(0.75,1),
          common.legend = T, legend = "bottom")
