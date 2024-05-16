# Experiment 1 ---------------------------------------------------------------
# note: unmatched studies conducted by Ozubko + Fawcett/Huff. 
# Experiments 1a and 1b in manuscript
### E1

# Study phase
e1_study_dat = read.csv('data/E1/sing_merged2.csv') %>%
  filter(!is.na(as.numeric(as.character(StudyTrials.thisTrialN)))) %>%
  filter(participant != 'sing_subj14real') %>%
  select(words, oldNew, condition, participant, date) %>%
  mutate(study_order = rep(c(1:90), times = 25))

# Test phase
e1_test_dat = read.csv('data/E1/sing_merged2.csv') %>%
  filter(!is.na(as.numeric(as.character(TestTrials.thisRepN)))) %>%
  select(words, oldNew, condition, participant, conf=keyConf.keys, conf_rt=keyConf.rt, 
         rkn=keyRKN.keys, rkn_rt=keyRKN.rt) %>%
  mutate(conf=as.numeric(as.character(conf))) %>%
  filter(participant != 'sing_subj14real') %>% 
  left_join(e1_study_dat %>% select(participant, words, condition, study_order)) %>%
  mutate(is_old = as.numeric(oldNew == 'old'))

# set up data frame with foils randomly split across SP

temp2 = e1_test_dat %>%
  filter(is_old == 0)

temp3 = NULL

set.seed(999)

for(i in 1:length(unique(e1_test_dat$participant)))
{
  temp4 <- sample(1:90)
  temp3 <- c(temp3, temp4)
}

temp4 = cbind(temp2, temp3)

temp4 = temp4 %>%
  mutate(study_order_2 = temp3)

temp_e1 <- left_join(e1_test_dat, temp4)

temp_e1 = temp_e1 %>%
  mutate(study_order = coalesce(study_order, study_order_2)) %>%
  mutate(said_old = as.numeric(conf>3),
         condition = factor(condition)) %>%
  mutate(condition = relevel(condition, 'None'),
         trials_scaled = scale(study_order)[,1])

### e2

# Reading test phase
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


# Read study phase
e2_study_dat = NULL
for(l in list.files('data/E2', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E2', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = dat %>% 
    select(studyN, participant, studyWord)
  dat$group = substr(l, 1,1)
  dat = dat %>%
    filter(!is.na(studyN)) %>%
    mutate(words = studyWord)
  dat$study_order = c(1:90)
  
  
  e2_study_dat = rbind(e2_study_dat, dat)
}

e2_study_dat = e2_study_dat %>% 
  filter(group=='s') %>%
  filter(!(participant %in% c('sing1')))

# set up data frame with foils randomly split across SP

temp2 = e2_test_dat %>%
  filter(condition == 'new')

temp3 = NULL

set.seed(999)

for(i in 1:length(unique(e2_test_dat$participant)))
{
  temp4 <- sample(1:90)
  temp3 <- c(temp3, temp4)
}

temp4 = cbind(temp2, temp3)

temp4 = temp4 %>%
  mutate(study_order_2 = temp3)

e2_merged <- left_join(e2_test_dat, e2_study_dat, by = c('participant', 'words'))

temp_e2 <- left_join(e2_merged, temp4)

temp_e2 = temp_e2 %>%
  mutate(study_order = coalesce(study_order, study_order_2)) %>%
  mutate(said_old = as.numeric(conf>3),
         condition = factor(condition)) %>%
  mutate(condition = relevel(condition, 'new'),
         trials_scaled = scale(study_order)[,1])

# bind data frames together

temp_e1 = temp_e1 %>%
  mutate(condition = recode_factor(condition, 'None' = 'new'),
         exp = 'E1',
         subj = paste0(participant, exp)) %>%
  select(subj, words, condition:rkn_rt, exp, study_order, trials_scaled) %>%
  select(-c(participant))

temp_e2 = temp_e2 %>%
  mutate(exp = 'E2',
         subj = paste0(participant, exp)) %>%
  select(subj, words, condition:rkn_rt, exp, study_order, trials_scaled)

temp_e12 = rbind(temp_e1 %>% 
                   select(subj, words, condition:rkn_rt, exp, study_order, trials_scaled), 
                 temp_e2 %>%
                   select(subj, words, condition:rkn_rt, exp, study_order, trials_scaled))

temp_e12 = temp_e12 %>%
  mutate(said_old = as.numeric(conf>3))

# define formula for nonlinear model

e12gm1 <- bf(said_old~condition + s(trials_scaled, by = condition) + 
               s(trials_scaled, subj, bs = 'fs', m = 1) + (condition|words))

# define formula for simple linear model

e12lm1 <- bf(said_old~condition + condition:trials_scaled + 
               (trials_scaled | subj) + (condition|words))

# define formula for complex linear model

e12lm2 <- bf(said_old~condition + condition:trials_scaled + 
               (condition:trials_scaled | subj) + (condition|words))

# set nonlinear priors

priors <- c(
  prior(normal(1, 1), class = "b", coef='conditionread'),
  prior(normal(1, 1), class = "b", coef='conditionsing'),
  prior(normal(1, 1), class = "b", coef='conditionspeak'),
  prior(normal(-1, 1), class = "Intercept"),
  prior(std_normal(), class = "sd"),
  prior(normal(0, 0.5), class = 'b', coef = 'strials_scaled:conditionnew_1'),
  prior(normal(0, 0.5), class = 'b', coef = 'strials_scaled:conditionread_1'),
  prior(normal(0, 0.5), class = 'b', coef = 'strials_scaled:conditionsing_1'),
  prior(normal(0, 0.5), class = 'b', coef = 'strials_scaled:conditionspeak_1'),
  prior(normal(0, 2), class = 'sds'),
  prior(lkj(3), class = "cor")
)

# set linear priors

lin_priors <- c(
  prior(normal(0, 0.5), class = "b", coef='conditionnew:trials_scaled'),
  prior(normal(0, 0.5), class = "b", coef='conditionread:trials_scaled'),
  prior(normal(0, 0.5), class = "b", coef='conditionsing:trials_scaled'),
  prior(normal(0, 0.5), class = "b", coef='conditionspeak:trials_scaled'),
  prior(normal(1, 1), class = "b"),
  prior(normal(-1, 1), class = "Intercept"),
  prior(std_normal(), class = "sd"),
  prior(lkj(3), class = "cor")
)

# fit NL model

e12_gam_v1 <- brm(e12gm1,
                  family = bernoulli(link="probit"),
                  data = temp_e12,
                  backend='cmdstanr',
                  prior = priors,
                  control = list(adapt_delta = .99),
                  cores = 6, 
                  chains = 6,
                  iter = 5000)

# fit simple linear model

e12_lm_v1 <- brm(e12lm1,
                 family = bernoulli(link="probit"),
                 data = temp_e12,
                 backend='cmdstanr',
                 prior = lin_priors,
                 control = list(adapt_delta = .99),
                 cores = 6, 
                 chains = 6,
                 iter = 5000)

# fit complex linear model

e12_lm_v2 <- brm(e12lm2,
                 family = bernoulli(link="probit"),
                 data = temp_e12,
                 backend='cmdstanr',
                 prior = lin_priors,
                 control = list(adapt_delta = .99),
                 cores = 6, 
                 chains = 6,
                 iter = 5000)

# generate predictions; due to the inclusion of a random smooth for participant
# - which brms does not recognize as a random effect for the purposes of predictions -
# we must generate participants using a random PID

gam_pred <- posterior_linpred(e12_gam_v1, 
                              newdata = expand.grid(condition = c('read', 'sing', 'speak', 'new'),
                                                    trials_scaled = unique(temp_e12$trials_scaled),
                                                    subj = '6E1'),
                              re_formula = NA)

# generate posterior smooths

psmooth<-posterior_smooths(e12_gam_v1, 
                           smooth = 's(trials_scaled, subj, bs = "fs", m = 1)',
                           newdata = expand.grid(condition = c('read', 'sing', 'speak', 'new'),
                                                 trials_scaled = unique(temp_e12$trials_scaled),
                                                 subj = '6E1'),
                           re_formula = NA)

### wrangling

# created transposed prediction matrix

# in order to exclude the random smooth, we isolate the term and subtract it from
# our predictions 

pred.1 <- t(gam_pred-as.matrix(psmooth))

# we then wrangle these predictions extensively for comparisons and plots

# bind new data

pred.2 = cbind(expand.grid(condition = c('read', 'sing', 'speak', 'new'),
                           trials_scaled = unique(temp_e12$trials_scaled),
                           subj = '6E1'), pred.1)

# pivot transposed prediction matrix

pred.2 %>%
  pivot_longer(cols = c(`1`:`15000`), names_to = 'names', values_to = 'values')-> pred.3

# pivot again to get what we want

pred.3 %>%
  pivot_wider(id_cols = c(trials_scaled:names), 
              names_from = condition, values_from = values) -> pred.4

pred.4 %>%
  mutate(silent = read-new,
         aloud = speak - new,
         sing = sing - new) %>%
  mutate(aloud_diff = aloud - silent,
         sing_diff = sing - silent,
         sse_diff = sing - aloud) -> pred.4

pred.4 %>%
  select(-c(read,speak,new,aloud_diff,sing_diff,sse_diff,names)) %>%
  pivot_longer(cols = c(sing,silent,aloud),
               names_to = 'condition',
               values_to = 'vals') ->pred.3

# compare differences between conditions at last 3 
# positions versus first 3 positions

# aloud vs silent

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) -> pred.5

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$aloud_diff - 
             pred.5[pred.5$sp <= 3,]$aloud_diff)

# compare middle to early

median_hdi(pred.5[pred.5$sp >= 44 & pred.5$sp <= 46,]$aloud_diff - 
             pred.5[pred.5$sp <= 3,]$aloud_diff)

# compare middle to late

median_hdi(pred.5[pred.5$sp >= 44 & pred.5$sp <= 46,]$aloud_diff - 
             pred.5[pred.5$sp >= 88,]$aloud_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$aloud_diff - 
             pred.5[pred.5$sp <= 5,]$aloud_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$aloud_diff - 
             pred.5[pred.5$sp <= 10,]$aloud_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$aloud_diff - 
             pred.5[pred.5$sp <= 20,]$aloud_diff)

# sing vs silent

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$sing_diff - 
             pred.5[pred.5$sp <= 3,]$sing_diff)

# compare middle to early

median_hdi(pred.5[pred.5$sp >= 44 & pred.5$sp <= 46,]$sing_diff - 
             pred.5[pred.5$sp <= 3,]$sing_diff)

# compare middle to late

median_hdi(pred.5[pred.5$sp >= 44 & pred.5$sp <= 46,]$sing_diff - 
             pred.5[pred.5$sp >= 88,]$sing_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$sing_diff - 
             pred.5[pred.5$sp <= 5,]$sing_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$sing_diff - 
             pred.5[pred.5$sp <= 10,]$sing_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$sing_diff - 
             pred.5[pred.5$sp <= 20,]$sing_diff)

# plot conditional predictions


pred.3 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0),
         Condition = recode(condition, 'aloud' = 'Aloud',
                            'silent' = 'Silent',
                            'sing' = 'Sing')) %>%
  group_by(sp, Condition) %>%
  mean_qi(vals) %>%
  ggplot(aes(x = sp, y = vals, fill = Condition)) +
  geom_ribbon(aes(fill = Condition, ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_line(aes(linetype = Condition)) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(0,2.5)) + 
  labs(y = expression(~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 1') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e12_cond

# plot contrasts

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) %>%
  select(-c(sing,aloud,silent)) %>%
  pivot_longer(cols = c(aloud_diff:sse_diff), 
               names_to = 'Contrast', values_to = 'estimate') %>%
  mutate(Contrast = recode(Contrast, 'aloud_diff' = 'Aloud - Silent',
                           'sing_diff' = 'Sing - Silent',
                           'sse_diff' = 'Sing - Aloud')) %>%
  group_by(sp, Contrast) %>%
  mean_qi(estimate) %>%
  ggplot(aes(x = sp, y = estimate, fill = Contrast)) +
  geom_line(aes(linetype = Contrast)) + 
  geom_ribbon(aes(fill = Contrast, ymin = .lower, ymax = .upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(-1,1.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(y = expression(Delta~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 1') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e12_diff

# Experiment 2 ----------------------------------------------------------------------
#Note: the first matched study conducted by Huff. Experiment 2 in manuscript.
# Reading both phases at once
e3_test_dat = NULL
for(l in list.files('data/E3', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E3', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  #dat = select(dat, testWord:frameRate)
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

# Reading both phases at once
e3_study_dat = NULL
for(l in list.files('data/E3', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E3', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  #dat = select(dat, testWord:frameRate)
  dat = dat %>% 
    select(participant, study_word, study_type)
  dat$group = substr(l, 1,1)
  
  dat = dat %>%
    filter(study_word!= '')
  dat$study_order = c(1:90)
  e3_study_dat = rbind(e3_study_dat, dat)
}

# set up data with foils randomly split across SP

e3_study_dat = e3_study_dat %>%
  mutate(words = study_word)

e3_merged <- merge(e3_test_dat, e3_study_dat, 
                   by = c('participant', 'words', 'group'))

temp2 = e3_test_dat %>%
  filter(is_old == 0)

temp3 = NULL

set.seed(999)

for(i in 1:length(unique(e3_test_dat$participant)))
{
  temp4 <- sample(1:90)
  temp3 <- c(temp3, temp4)
}

temp4 = cbind(temp2, temp3)

temp4 = temp4 %>%
  mutate(study_order = temp3)

temp_e3 <- rbind(e3_merged %>% select(participant, words, instruction, study_order, is_old_cent, said_old, group),
                 temp4 %>% select(participant, words, instruction, study_order, is_old_cent, said_old, group))

temp_e3 = temp_e3 %>%
  group_by(participant)

temp_e3 = temp_e3 %>%
  mutate(trials_scaled = scale(study_order)[,1])

temp_e3 = temp_e3 %>%
  mutate(group = toupper(group))

# define NL model formula

e3gm1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + s(trials_scaled,  by = instruction) +
    s(trials_scaled, participant, bs = 'fs', m =1) + 
    (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + 
    (instruction:group-1|words),
  nl = TRUE
)

# define simple linear model formula

e3lm1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group + instruction:trials_scaled-1 +
    (trials_scaled | participant) + 
    (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + 
    (instruction:group-1|words),
  nl = TRUE
)

# define complex linear model formula

e3lm2 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group + instruction:trials_scaled-1 +
    (instruction:trials_scaled-1 | participant) + 
    (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + 
    (instruction:group-1|words),
  nl = TRUE
)

# priors for NL model

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:instructionaloud_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:instructionsilent_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:instructionsing_1'),
  prior(normal(0, 2), nlpar = 'dprime', class = 'sds'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)

# priors for linear models

lin_priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'instructionaloud:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'instructionsilent:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'instructionsing:trials_scaled'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)

# fit models

e3_gam_v1 <- brm(
  e3gm1,
  family = bernoulli(link = "identity"),
  data = temp_e3,
  prior = Priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr'
)

e3_lm1 <- brm(
  e3lm1,
  family = bernoulli(link = "identity"),
  data = temp_e3,
  prior = lin_priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr'
)

e3_lm2 <- brm(
  e3lm2,
  family = bernoulli(link = "identity"),
  data = temp_e3,
  prior = lin_priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr'
)

# generate predictions across SP

gam_pred <- posterior_linpred(e3_gam_v1, 
                              newdata = expand.grid(instruction = c('aloud', 'silent', 'sing'),
                                                    trials_scaled = unique(temp_e3$trials_scaled),
                                                    group = c('C', 'N'),
                                                    is_old_cent = 0.5,
                                                    participant = 'NC29'
                              ),
                              re_formula = NA,
                              nlpar = 'dprime')

# generate posterior smooths

psmooth<-posterior_smooths(e3_gam_v1, 
                           smooth = 's(trials_scaled, participant, bs = "fs", m = 1)',
                           newdata = expand.grid(instruction = c('aloud', 'silent', 'sing'),
                                                 trials_scaled = unique(temp_e3$trials_scaled),
                                                 group = c('C', 'N'),
                                                 is_old_cent = 0.5,
                                                 participant = 'NC29'
                           ),
                           re_formula = NA,
                           nlpar = 'dprime')

### wrangling

# created transposed prediction matrix

pred.1 <- t(gam_pred-as.matrix(psmooth))

# bind new data

pred.2 = cbind(expand.grid(instruction = c('aloud', 'silent', 'sing'),
                           trials_scaled = unique(temp_e3$trials_scaled),
                           group = c('C', 'N'),
                           is_old_cent = 0.5,
                           participant = 'NC29'), pred.1)

# pivot transposed prediction matrix

pred.2 %>%
  pivot_longer(cols = c(`1`:`15000`), names_to = 'names', values_to = 'values')-> pred.3

# pivot again to get what we want

pred.3 %>%
  pivot_wider(id_cols = c(trials_scaled:names), 
              names_from = instruction, values_from = values) %>%
  select(-c(is_old_cent:names)) %>%
  
  # calculate differences between conditions
  
  mutate(aloud_diff = aloud-silent,
         sing_diff = sing - silent,
         sse_diff = sing - aloud) -> pred.4



# compare differences between conditions at last 3 
# positions versus first 3 positions

# aloud vs silent

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) -> pred.5

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$aloud_diff - 
             pred.5[pred.5$sp <= 3,]$aloud_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$aloud_diff - 
             pred.5[pred.5$sp <= 5,]$aloud_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$aloud_diff - 
             pred.5[pred.5$sp <= 10,]$aloud_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$aloud_diff - 
             pred.5[pred.5$sp <= 20,]$aloud_diff)

# sing vs silent

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$sing_diff - 
             pred.5[pred.5$sp <= 3,]$sing_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$sing_diff - 
             pred.5[pred.5$sp <= 5,]$sing_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$sing_diff - 
             pred.5[pred.5$sp <= 10,]$sing_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$sing_diff - 
             pred.5[pred.5$sp <= 20,]$sing_diff)


#plot

pred.3 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0),
         Condition = recode(instruction, 'aloud' = 'Aloud',
                            'silent' = 'Silent',
                            'sing' = 'Sing')) %>%
  group_by(sp, Condition) %>%
  mean_qi(values) %>%
  ggplot(aes(x = sp, y = values, fill = Condition)) +
  geom_ribbon(aes(fill = Condition, ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_line(aes(linetype = Condition)) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(0,2.5)) + 
  labs(y = expression(~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 2') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e3_cond

ggsave('figures/cond-smooth-e3.png')

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) %>%
  select(-c(sing,aloud,silent)) %>%
  pivot_longer(cols = c(aloud_diff:sse_diff), 
               names_to = 'Contrast', values_to = 'estimate') %>%
  mutate(Contrast = recode(Contrast, 'aloud_diff' = 'Aloud - Silent',
                           'sing_diff' = 'Sing - Silent',
                           'sse_diff' = 'Sing - Aloud')) %>%
  group_by(sp, Contrast) %>%
  mean_qi(estimate) %>%
  ggplot(aes(x = sp, y = estimate, fill = Contrast)) +
  geom_line(aes(linetype = Contrast)) + 
  geom_ribbon(aes(fill = Contrast, ymin = .lower, ymax = .upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(-1,1.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(y = expression(Delta~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 2') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e3_diff

ggsave('figures/diffsmooths-e3.png')  

# Experiment 3 ----------------------------------------------------------------------

#Note: exact replication conducted by Huff. Experiment 3 in manuscript

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

# Reading both phases at once
e5_study_dat = NULL
for(l in list.files('data/E5', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E5', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  #dat = select(dat, testWord:frameRate)
  dat = dat %>% 
    select(participant, study_order, study_word, study_type, key_colortest.keys)
  
  if ('y' %in% dat$key_colortest.keys) {
    dat = dat %>%
      mutate(group = 'c')
  } else {
    dat = dat %>%
      mutate(group = 'nc')
  }
  
  dat = dat %>%
    select(-key_colortest.keys) %>%
    filter(!is.na(study_order))
  e5_study_dat = rbind(e5_study_dat, dat)
}

e5_study_dat = e5_study_dat %>%
  filter(!is.na(as.numeric(as.character(study_order)))) %>%
  mutate(test_word = study_word)

# set up data with foils split across SP

e5_merged <- merge(e5_test_dat, e5_study_dat, 
                   by = c('participant', 'test_word', 'group'))

e5_merged = e5_merged %>%
  mutate(acc = as.numeric(is_old == said_old))

e5_merged %>%
  filter(group == 'nc') %>%
  group_by(test_condition, study_order) %>%
  summarise(mean(said_old)) %>%
  ggplot(aes(x = study_order, y = `mean(said_old)`), fill = test_condition) +
  geom_point(aes(color=test_condition)) +
  geom_smooth(aes(color = test_condition)) 

temp2 = e5_test_dat %>%
  filter(is_old == 0)

temp3 = NULL

set.seed(999)

for(i in 1:length(unique(e5_test_dat$participant)))
{
  temp4 <- sample(1:120)
  temp3 <- c(temp3, temp4)
}

temp4 = cbind(temp2, temp3)

temp4 = temp4 %>%
  mutate(study_order = temp3)

temp5 <- rbind(e5_merged %>% select(participant, test_word, test_condition, study_order, is_old_cent, said_old, group),
               temp4 %>% select(participant, test_word, test_condition, study_order, is_old_cent, said_old, group))

temp5 = temp5 %>%
  group_by(participant)

temp5 = temp5 %>%
  mutate(trials_scaled = scale(study_order)[,1])

# define nonlinear model formula

e5gm2 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ test_condition:group-1 + s(trials_scaled,  by = test_condition) +
    s(trials_scaled, participant, bs = 'fs', m =1) + 
    (test_condition:group-1|test_word), 
  c ~ test_condition:group-1 + (test_condition-1|participant) + 
    (test_condition:group-1|test_word),
  nl = TRUE
)

# define simple linear model formula

e5lm1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ test_condition:group + test_condition:trials_scaled-1 +
    (trials_scaled | participant) +
    (test_condition:group-1|test_word), 
  c ~ test_condition:group-1 + (test_condition-1|participant) + 
    (test_condition:group-1|test_word),
  nl = TRUE
)

# define complex linear model formula

e5lm2 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ test_condition:group + test_condition:trials_scaled-1 +
    (test_condition:trials_scaled-1 | participant) +
    (test_condition:group-1|test_word), 
  c ~ test_condition:group-1 + (test_condition-1|participant) + 
    (test_condition:group-1|test_word),
  nl = TRUE
)

# set priors for NL models

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:test_conditionaloud_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:test_conditionsilent_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:test_conditionsing_1'),
  prior(normal(0, 2), nlpar = 'dprime', class = 'sds'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)

# set priors for linear models

lin_priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'test_conditionaloud:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'test_conditionsilent:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'test_conditionsing:trials_scaled'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)

# fit models

e5_gam_v4 <- brm(
  e5gm2,
  family = bernoulli(link = "identity"),
  data = temp5,
  prior = Priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr')

e5_lm1 <- brm(
  e5lm1,
  family = bernoulli(link = "identity"),
  data = temp5,
  prior = lin_priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr')

e5_lm2 <- brm(
  e5lm2,
  family = bernoulli(link = "identity"),
  data = temp5,
  prior = lin_priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains = 6,
  cores = 6,
  init = 0,
  backend = 'cmdstanr'
)

# generate predictions across SP

gam_pred <- posterior_linpred(e5_gam_v4, 
                              newdata = expand.grid(test_condition = c('aloud', 'silent', 'sing'),
                                                    trials_scaled = unique(temp5$trials_scaled),
                                                    group = c('c', 'nc'),
                                                    is_old_cent = 0.5,
                                                    participant = '99NC'
                              ),
                              re_formula = NA,
                              nlpar = 'dprime')

# generate posterior smooths

psmooth<-posterior_smooths(e5_gam_v4, 
                           smooth = 's(trials_scaled, participant, bs = "fs", m = 1)',
                           newdata = expand.grid(test_condition = c('aloud', 'silent', 'sing'),
                                                 trials_scaled = unique(temp5$trials_scaled),
                                                 group = c('c', 'nc'),
                                                 is_old_cent = 0.5,
                                                 participant = '99NC'),
                           re_formula = NA,
                           nlpar = 'dprime')

### wrangling

# created transposed prediction matrix

pred.1 <- t(gam_pred-as.matrix(psmooth))

# bind new data

pred.2 = cbind(expand.grid(test_condition = c('aloud', 'silent', 'sing'),
                           trials_scaled = unique(temp5$trials_scaled),
                           is_old_cent = 0.5,
                           group = c('c', 'nc'),
                           participant = '99NC'), pred.1)

# pivot transposed prediction matrix

pred.2 %>%
  pivot_longer(cols = c(`1`:`15000`), names_to = 'names', values_to = 'values')-> pred.3

# pivot again to get what we want

pred.3 %>%
  pivot_wider(id_cols = c(trials_scaled:names), 
              names_from = test_condition, values_from = values) %>%
  select(-c(is_old_cent:names)) %>%
  
  # calculate differences between conditions
  
  mutate(aloud_diff = aloud-silent,
         sing_diff = sing - silent,
         sse_diff = sing - aloud) -> pred.4

# compare differences between conditions at last 3 
# positions versus first 3 positions

# aloud vs silent

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:120) + mean(1:120), digits = 0)) -> pred.5

# 3 positions

median_hdi(pred.5[pred.5$sp >= 118,]$aloud_diff - 
             pred.5[pred.5$sp <= 3,]$aloud_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 116,]$aloud_diff - 
             pred.5[pred.5$sp <= 5,]$aloud_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 111,]$aloud_diff - 
             pred.5[pred.5$sp <= 10,]$aloud_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 101,]$aloud_diff - 
             pred.5[pred.5$sp <= 20,]$aloud_diff)

# sing vs silent

# 3 positions

median_hdi(pred.5[pred.5$sp >= 118,]$sing_diff - 
             pred.5[pred.5$sp <= 3,]$sing_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 116,]$sing_diff - 
             pred.5[pred.5$sp <= 5,]$sing_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 111,]$sing_diff - 
             pred.5[pred.5$sp <= 10,]$sing_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 101,]$sing_diff - 
             pred.5[pred.5$sp <= 20,]$sing_diff)

#plot

pred.3 %>%
  mutate(sp = round(trials_scaled * sd(1:120) + mean(1:120), digits = 0),
         Condition = recode(test_condition, 'aloud' = 'Aloud',
                            'silent' = 'Silent',
                            'sing' = 'Sing')) %>%
  group_by(sp, Condition) %>%
  mean_qi(values) %>%
  ggplot(aes(x = sp, y = values, fill = Condition)) +
  geom_ribbon(aes(fill = Condition, ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_line(aes(linetype = Condition)) +
  scale_x_continuous(breaks = seq(0,120, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(0,2.5)) + 
  labs(y = expression(~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position', 
       title = 'Experiment 3') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e5_cond

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:120) + mean(1:120), digits = 0)) %>%
  select(-c(sing,aloud,silent)) %>%
  pivot_longer(cols = c(aloud_diff:sse_diff), 
               names_to = 'Contrast', values_to = 'estimate') %>%
  mutate(Contrast = recode(Contrast, 'aloud_diff' = 'Aloud - Silent',
                           'sing_diff' = 'Sing - Silent',
                           'sse_diff' = 'Sing - Aloud')) %>%
  group_by(sp, Contrast) %>%
  mean_qi(estimate) %>%
  ggplot(aes(x = sp, y = estimate, fill = Contrast)) +
  geom_line(aes(linetype = Contrast)) + 
  geom_ribbon(aes(fill = Contrast, ymin = .lower, ymax = .upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,120, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(-1,1.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(y = expression(Delta~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 3') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e5_diff

# Experiment 4 ----------------------------------------------------------------------

# note - between subjects experiment conducted by Huff; 
# experiment 4 in mansucript 

# read in and prepare test phase data
e4_test_dat = NULL
for(l in list.files('data/e4', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/e4', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = select(dat, test_word:frameRate)
  dat = dat %>% 
    select(participant, test_order, words = test_word, condition=test_condition, old_new_test, conf=keyConf.keys, conf_rt=keyConf.rt, 
           rkn=keyRKN.keys, rkn_rt=keyRKN.rt)
  
  e4_test_dat = rbind(e4_test_dat, dat)
}

e4_test_dat = e4_test_dat %>%
  filter(!is.na(as.numeric(as.character(test_order)))) %>%
  mutate(instruction = str_replace(condition, 'new_', ''), is_old = old_new_test == 'old', is_old_cent = is_old - .5) %>%
  mutate(conf=as.numeric(as.character(conf))) %>%
  mutate(said_old = as.numeric(conf > 3), instruction = recode(instruction, a = 'aloud', b = 'silent', c = 'sing'))

e4_test_dat <- e4_test_dat%>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) 

# read in and prepare study phase data
e4_study_dat = NULL
for(l in list.files('data/E4', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E4', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = dat %>% 
    select(participant, study_order, study_word, study_type)
  
  dat = dat %>%
    filter(!is.na(study_order))
  e4_study_dat = rbind(e4_study_dat, dat)
}

e4_study_dat <- e4_study_dat%>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) 

e4_study_dat = e4_study_dat %>%
  filter(!is.na(as.numeric(as.character(study_order)))) %>%
  mutate(words = study_word)

e4_merged <- merge(e4_test_dat, e4_study_dat, 
                   by = c('participant', 'words'))

# randomly assign study order values to test phase data

temp2 = e4_test_dat %>%
  filter(is_old == 0)

temp3 = NULL

set.seed(999)

for(i in 1:length(unique(e4_test_dat$participant)))
{
  temp4 <- sample(1:90)
  temp3 <- c(temp3, temp4)
}

temp4 = cbind(temp2, temp3)

temp4 = temp4 %>%
  mutate(study_order = temp3)

# bind everything together

temp_e4 <- rbind(e4_merged %>% select(participant, words, condition, study_order, is_old_cent, said_old),
                 temp4 %>% select(participant, words, condition, study_order, is_old_cent, said_old))

temp_e4 = temp_e4 %>%
  group_by(participant)

# scale serial position 

temp_e4 = temp_e4 %>%
  mutate(trials_scaled = scale(study_order)[,1]) 

# formula for the non-linear model

e4gm2 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ condition-1 + s(trials_scaled,  by = condition) +
    s(trials_scaled, participant, bs = 'fs', m = 1) + (condition-1|words), 
  c ~ condition-1 + (1|participant) + (condition-1|words),
  nl = TRUE
)

# linear formula

e4lm1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ condition + condition:trials_scaled-1 +
    (trials_scaled | participant) + (condition-1|words), 
  c ~ condition-1 + (1|participant) + (condition-1|words),
  nl = TRUE
)

# priors for non-linear model

Priors2 <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:conditionaloud_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:conditionsilent_1'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'strials_scaled:conditionsing_1'),
  prior(normal(0, 2), nlpar = 'dprime', class = 'sds'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)

# priors for linear model

lin_priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(std_normal(), nlpar = "dprime", class = "sd"),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'conditionaloud:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'conditionsilent:trials_scaled'),
  prior(normal(0, 0.5), nlpar = "dprime", coef = 'conditionsing:trials_scaled'),
  prior(std_normal(), nlpar = "c"),
  prior(std_normal(), nlpar = "c", class = "sd"),
  prior(lkj(3), class = "cor")
)


# fit non-linear model 

e4_gam <- brm(
  e4gm2,
  family = bernoulli(link = "identity"),
  data = temp_e4,
  prior = Priors2,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains= 6,
  cores = 6,
  backend = 'cmdstanr'
)

# fit linear model

e4_lm_1 <- brm(
  e4lm1,
  family = bernoulli(link = "identity"),
  data = temp_e4,
  prior = lin_priors,
  iter = 5000,
  control = list(adapt_delta = .99),
  chains= 6,
  cores = 6,
  backend = 'cmdstanr'
)

# generate predictions for GAM

gam_pred <- posterior_linpred(e4_gam, 
                              newdata = expand.grid(condition = c('aloud', 'silent', 'sing'),
                                                    trials_scaled = unique(temp_e4$trials_scaled),
                                                    is_old_cent = 0.5,
                                                    participant = '126Aloud'
                              ),
                              re_formula = NA,
                              nlpar = 'dprime')

# generate posterior smooth for random by-participant curves

psmooth<-posterior_smooths(e4_gam, 
                  smooth = 's(trials_scaled, participant, bs = "fs", m = 1)',
                  newdata = expand.grid(condition = c('aloud', 'silent', 'sing'),
                                        trials_scaled = unique(temp_e4$trials_scaled),
                                        is_old_cent = 0.5,
                                        participant = '126Aloud'),
                  re_formula = NA,
                  nlpar = 'dprime')

### wrangling

# created transposed prediction matrix

pred.1 <- t(gam_pred-as.matrix(psmooth))

# bind new data

pred.2 = cbind(expand.grid(condition = c('aloud', 'silent', 'sing'),
            trials_scaled = unique(temp_e4$trials_scaled),
            is_old_cent = 0.5,
            participant = '126Aloud'), pred.1)

# pivot transposed prediction matrix

pred.2 %>%
  pivot_longer(cols = c(`1`:`15000`), names_to = 'names', values_to = 'values')-> pred.3

# pivot again to get what we want

pred.3 %>%
  pivot_wider(id_cols = c(trials_scaled:names), 
              names_from = condition, values_from = values) %>%
  select(-c(is_old_cent:names)) %>%
  
  # calculate differences between conditions
  
  mutate(aloud_diff = aloud-silent,
         sing_diff = sing - silent,
         sse_diff = sing - aloud) -> pred.4

# compare differences between conditions at last 3 
# positions versus first 3 positions

# aloud vs silent

pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) -> pred.5

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$aloud_diff - 
             pred.5[pred.5$sp <= 3,]$aloud_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$aloud_diff - 
             pred.5[pred.5$sp <= 5,]$aloud_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$aloud_diff - 
             pred.5[pred.5$sp <= 10,]$aloud_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$aloud_diff - 
             pred.5[pred.5$sp <= 20,]$aloud_diff)

# sing vs silent

# 3 positions

median_hdi(pred.5[pred.5$sp >= 88,]$sing_diff - 
             pred.5[pred.5$sp <= 3,]$sing_diff)

# 5 positions

median_hdi(pred.5[pred.5$sp >= 86,]$sing_diff - 
             pred.5[pred.5$sp <= 5,]$sing_diff)

# 10 positions 

median_hdi(pred.5[pred.5$sp >= 81,]$sing_diff - 
             pred.5[pred.5$sp <= 10,]$sing_diff)

# 20 positions 

median_hdi(pred.5[pred.5$sp >= 71,]$sing_diff - 
             pred.5[pred.5$sp <= 20,]$sing_diff)

# plots

pred.3 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0),
         Condition = recode(condition, 'aloud' = 'Aloud',
                            'silent' = 'Silent',
                            'sing' = 'Sing')) %>%
  group_by(sp, Condition) %>%
  mean_qi(values) %>%
  ggplot(aes(x = sp, y = values, fill = Condition)) +
  geom_ribbon(aes(fill = Condition, ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_line(aes(linetype = Condition)) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(0,2.5)) + 
  labs(y = expression(~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 4') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e4_cond
  
pred.4 %>%
  mutate(sp = round(trials_scaled * sd(1:90) + mean(1:90), digits = 0)) %>%
  select(-c(sing,aloud,silent)) %>%
  pivot_longer(cols = c(aloud_diff:sse_diff), 
               names_to = 'Contrast', values_to = 'estimate') %>%
  mutate(Contrast = recode(Contrast, 'aloud_diff' = 'Aloud - Silent',
                            'sing_diff' = 'Sing - Silent',
                            'sse_diff' = 'Sing - Aloud')) %>%
  group_by(sp, Contrast) %>%
  mean_qi(estimate) %>%
  ggplot(aes(x = sp, y = estimate, fill = Contrast)) +
  geom_line(aes(linetype = Contrast)) + 
  geom_ribbon(aes(fill = Contrast, ymin = .lower, ymax = .upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,90, by = 10), expand = c(0.005,0)) + 
  scale_y_continuous(limits = c(-1,1.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(y = expression(Delta~italic("d'")~' (Model Estimate)'), 
       x = 'Serial Position',
       title = 'Experiment 4') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() -> e4_diff


# arrange -----------------------------------------------------------------

ggarrange(e12_cond, e3_cond,
          e5_cond, e4_cond,
          nrow = 2, ncol = 2, 
          #widths = c(0.75,1),
          common.legend = T, legend = "bottom")

ggarrange(e12_cond + rremove('ylab'), e3_cond + rremove('ylab'),
          e5_cond + rremove('ylab'), e4_cond + rremove('ylab'),
          nrow = 2, ncol = 2, 
          #widths = c(0.75,1),
          common.legend = T, legend = "bottom")

ggarrange(e12_cond + rremove('ylab'), e3_cond + rremove('ylab'),
          e5_cond + rremove('ylab'), e4_cond + rremove('ylab'),
          nrow = 2, ncol = 2, 
          common.legend = T, legend = "bottom") -> cond_arranged

annotate_figure(cond_arranged, 
                left = textGrob(expression(~italic("d'")~' (Model Estimate)'),
                                rot = 90))

ggarrange(e12_diff + rremove('ylab'), e3_diff + rremove('ylab'),
          e5_diff + rremove('ylab'), e4_diff + rremove('ylab'),
          nrow = 2, ncol = 2, 
          common.legend = T, legend = "bottom") -> diff_arranged

annotate_figure(diff_arranged, 
                left = textGrob(expression(Delta~italic("d'")~' (Model Estimate)'),
                                rot = 90))
