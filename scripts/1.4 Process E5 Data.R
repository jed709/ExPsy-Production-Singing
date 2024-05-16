#Note: this is the exact replication conducted by Huff. Experiment 3 in
# manuscript

# Load Data ---------------------------------------------------------------

# Reading both phases at once
e5_test_dat = NULL
for(l in list.files('data/E5', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E5', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  #dat = select(dat, testWord:frameRate)
  dat = dat %>% 
    select(participant, test_order, test_num, test_word, old_new_test, Participant.Response, test_condition, participant, word_color_test)
  e5_test_dat = rbind(e5_test_dat, dat)
}

e5_test_dat = e5_test_dat %>%
  filter(!is.na(as.numeric(as.character(test_num)))) %>%
  mutate(group = recode(word_color_test, white = "c", red = "c", blue = "c", yellow = "nc"), 
         is_old = as.numeric(recode(old_new_test, old = '1', new = '0')),
         is_old_cent = is_old - .5,
         said_old = as.numeric(recode(Participant.Response, y = '1', n = '0')))%>%
  unite(condition, c(old_new_test, test_condition), remove = FALSE)


# summary stats -----------------------------------------------------------


e5_test_dat %>%
  group_by(group, participant, test_condition, is_old) %>%
  summarize(n=n(), m=mean(said_old == 1), new=mean(said_old == 0)) -> e5_test_sum

# d' calculations
e5_test_sum %>%
  select(group, participant, test_condition, is_old, m) %>%
  mutate(condition = paste0(test_condition, is_old)) %>%
  select(-test_condition, -is_old) %>%
  pivot_wider(id_cols = c('participant', 'group'), names_from = 'condition', values_from = 'm') %>%
  mutate(read_d = calc_d(silent1, silent0, cor = .016), 
         sing_d = calc_d(sing1, sing0, cor = .016), 
         speak_d = calc_d(aloud1, aloud0, cor = .016)) %>%
  select(group, participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'test_condition', values_to = 'd') -> e5_d_sum

#d cor matched
cor(e5_d_sum[e5_d_sum$test_condition=='sing_d' & e5_d_sum$group =='c',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='speak_d' & e5_d_sum$group == 'c',]$d)

cor(e5_d_sum[e5_d_sum$test_condition=='read_d' & e5_d_sum$group =='c',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='speak_d' & e5_d_sum$group == 'c',]$d)

cor(e5_d_sum[e5_d_sum$test_condition=='sing_d' & e5_d_sum$group =='c',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='read_d' & e5_d_sum$group == 'c',]$d)
#d cor unmatched
cor(e5_d_sum[e5_d_sum$test_condition=='sing_d' & e5_d_sum$group =='nc',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='speak_d' & e5_d_sum$group == 'nc',]$d)

cor(e5_d_sum[e5_d_sum$test_condition=='read_d' & e5_d_sum$group =='nc',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='speak_d' & e5_d_sum$group == 'nc',]$d)

cor(e5_d_sum[e5_d_sum$test_condition=='sing_d' & e5_d_sum$group =='nc',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='read_d' & e5_d_sum$group == 'nc',]$d)
#d cor comb
cor(e5_d_sum[e5_d_sum$test_condition=='sing_d',]$d, 
    e5_d_sum[e5_d_sum$test_condition=='speak_d',]$d)

# probit models ------------------------------------------------------------------

#y/n recog

# define formula

# Note: correlations between d' and c' are modeled here; this caused
# convergence issues in other models, but was able to be retained here

e5m1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ test_condition:group-1 + (test_condition-1|p|participant) + (test_condition:group-1|w|test_word), 
  c ~ test_condition:group-1 + (test_condition-1|p|participant) + (test_condition:group-1|w|test_word),
  nl = TRUE
)

# set prior

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(normal(0, 1), nlpar = "dprime", class = "sd"),
  prior(normal(0, 1), nlpar = "c"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),
  prior(lkj(4), class = "cor")
)

# fit model

e5_conf_large <- brm(
  e5m1,
  family = bernoulli(link = "identity"),
  data = e5_test_dat,
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

# plots

e5_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_test_conditionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_test_conditionaloud:groupnc',
         Sing_Matched = 'b_dprime_test_conditionsing:groupc',
         Sing_Unmatched = 'b_dprime_test_conditionsing:groupnc',
         Silent_Matched = 'b_dprime_test_conditionsilent:groupc',
         Silent_Unmatched = 'b_dprime_test_conditionsilent:groupnc') %>%
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
  labs(title = 'E3 Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0.5,2.5), breaks = seq(0.5, 2.5, 0.5)) +
  theme_classic() -> e5_conf_plot

# plot contrasts

e5_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_test_conditionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_test_conditionaloud:groupnc',
         Sing_Matched = 'b_dprime_test_conditionsing:groupc',
         Sing_Unmatched = 'b_dprime_test_conditionsing:groupnc',
         Silent_Matched = 'b_dprime_test_conditionsilent:groupc',
         Silent_Unmatched = 'b_dprime_test_conditionsilent:groupnc') %>%
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
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-0.5,1.5), breaks = seq(-0.5, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e5_conf_con_plot

# plot c

e5_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_test_conditionaloud:groupc',
         Aloud_Unmatched = 'b_c_test_conditionaloud:groupnc',
         Sing_Matched = 'b_c_test_conditionsing:groupc',
         Sing_Unmatched = 'b_c_test_conditionsing:groupnc',
         Silent_Matched = 'b_c_test_conditionsilent:groupc',
         Silent_Unmatched = 'b_c_test_conditionsilent:groupnc') %>%
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
  labs(title = 'E3 Confidence', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-.5,1), breaks = seq(-.5, 1, 0.5)) +
  theme_classic() -> e5_conf_plot_c

# plot c contrasts

e5_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_test_conditionaloud:groupc',
         Aloud_Unmatched = 'b_c_test_conditionaloud:groupnc',
         Sing_Matched = 'b_c_test_conditionsing:groupc',
         Sing_Unmatched = 'b_c_test_conditionsing:groupnc',
         Silent_Matched = 'b_c_test_conditionsilent:groupc',
         Silent_Unmatched = 'b_c_test_conditionsilent:groupnc') %>%
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
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, .5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e5_conf_con_plot_c

ggarrange(e5_conf_plot, e5_conf_con_plot, ncol = 2, widths = c(0.75,1), common.legend = T, legend = 'bottom')

ggarrange(e5_conf_plot_c, e5_conf_con_plot_c, ncol = 2, widths = c(0.75,1), common.legend = T, legend = 'bottom')
