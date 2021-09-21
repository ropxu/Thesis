library(tidyverse)
library(RcppRoll)
library(modelr)
# Helper function to modify the table output
source('table_helper.R')

main_df <- read_csv('../data/main_data.csv')

main_df <- main_df %>% select(date, ric, co_sic, ret, vol, v30, everything())

df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se1 = lag(se_neg,1),
         lag_se2 = lag(se_neg,2),
         lag_se3 = lag(se_neg,3),
         lag_se4 = lag(se_neg,4),
         lag_se5 = lag(se_neg,5),
         ret1 = lag(ret,1),
         ret2 = lag(ret,2),
         ret3 = lag(ret,3),
         ret4 = lag(ret,4),
         ret5 = lag(ret,5),
         lag_vol1 = lag(vol,1),
         lag_vol2 = lag(vol,2),
         lag_vol3 = lag(vol,3),
         lag_vol4 = lag(vol,4),
         lag_vol5 = lag(vol,5),
         lag_ret1 = lag(ret,1),
         lag_ret2 = lag(ret,2),
         lag_ret3 = lag(ret,3),
         lag_ret4 = lag(ret,4),
         lag_ret5 = lag(ret,5),
         lag_iv301 = lag(iv30,1),
         lag_iv302 = lag(iv30,2),
         lag_iv303 = lag(iv30,3),
         lag_iv304 = lag(iv30,4),
         lag_iv305 = lag(iv30,5),
         sic = as.character(str_sub(co_sic, 1,2))) %>% 
  #sic = co_sic) %>% 
  select(date, ric, co_sic, sic, v30, ret,se_neg, lag_se1, 
         lag_se2, lag_se3, lag_se4, lag_se5, lag_vol1, 
         lag_vol2, lag_vol3, lag_vol4, lag_vol5, lag_ret1, 
         lag_ret2, lag_ret3, lag_ret4, lag_ret5,
         ret1, ret2, ret3, ret4, ret5, 
         lag_iv301, lag_iv302, lag_iv303, lag_iv304, lag_iv305) %>% ungroup()


# =========================== Controlled different periods ===========================

df1 <- df %>% arrange(date) %>% filter(row_number() <= nrow(df)/3)
df2 <- df %>% arrange(date) %>% filter(row_number() >= nrow(df)/3*1 & row_number() <= nrow(df)/3*2)
df3 <- df %>% arrange(date) %>% filter(row_number() >= nrow(df)/3*2 & row_number() <= nrow(df)/3*3)

co_period1 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + weekdays(date) + months(date) +
                  factor(sic)
                , data=df1)

co_period2 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                   log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + weekdays(date) + months(date) +
                   factor(sic)
                 , data=df2)

co_period3 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                   log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + weekdays(date) + months(date) +
                   factor(sic)
                 , data=df3)



iv_period1 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic)
                , data=df1)

iv_period2 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic)
                , data=df2)

iv_period3 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic)
                , data=df3)


stargazer(co_period1, co_period2, co_period3, iv_period1, iv_period2, iv_period3,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'implied_vol_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          column.labels = c('0-33\\%', '33-67\\%', '67-100\\%', '0-33\\%', '33-67\\%', '67-100\\%'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{Ivol_{t-1}}$',
                               '$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-3}}$',
                               '$\\mathbf{Ivol_{t-4}}$','$\\mathbf{Ivol_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/different_periods.tex')



# ==== Ret ====

se_model1 <- lm(se_neg ~ lag_ret1, data=df)
se_model2 <- lm(se_neg ~ lag_ret1 + lag_ret2, data=df)
se_model3 <- lm(se_neg ~ lag_ret1 + lag_ret2 + lag_ret3, data=df)
se_model4 <- lm(se_neg ~ lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4, data=df)
se_model5 <- lm(se_neg ~ lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5, data=df)


stargazer(se_model1, se_model2, se_model3, se_model4, se_model5,
          type = 'text',
          omit = c('Constant'),
          report = 'vct*',
          font.size = 'footnotesize',
          dep.var.caption = "",
          single.row = T,
          label = 'BadNews_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$'))



# ==== ret co ====

co_model1 <- lm(se_neg ~ lag_se1 + log(lag_vol1) + lag_ret1 + lag_se1 +  lag_iv301 + weekdays(date) + months(date), data=df)
co_model2 <- lm(se_neg ~ lag_se1 + lag_se2 + log(lag_vol1) + log(lag_vol2) + lag_ret1 + lag_ret2 +
                  lag_se1 + lag_se2 + weekdays(date) + months(date) + factor(sic), data=df)
co_model3 <- lm(se_neg ~ lag_se1 + lag_se2 + lag_se3 + 
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_se1 + lag_se2 + lag_se3 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

co_model4 <- lm(se_neg ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_se1 + lag_se2 + lag_se3 + lag_se4 + 
                  weekdays(date) + months(date) +
                  factor(sic), data=df)

co_model5 <- lm(se_neg ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 + 
                  weekdays(date) + months(date) +
                  factor(sic)
                , data=df)


stargazer(co_model1, co_model2, co_model3, co_model4, co_model5,
          type = 'text',
          omit = c('Constant', 'weekdays', 'months', 'log', 'se', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'controlled_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          #covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               # '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               # '$\\mathbf{BadNews_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))


df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se1 = lag(se_neg,1),
         lag_se2 = lag(se_neg,2),
         lag_se3 = lag(se_neg,3),
         lag_se4 = lag(se_neg,4),
         lag_se5 = lag(se_neg,5),
         ret1 = lag(ret,1),
         ret2 = lag(ret,2),
         ret3 = lag(ret,3),
         ret4 = lag(ret,4),
         ret5 = lag(ret,5),
         lag_vol1 = lag(vol,1),
         lag_vol2 = lag(vol,2),
         lag_vol3 = lag(vol,3),
         lag_vol4 = lag(vol,4),
         lag_vol5 = lag(vol,5),
         lag_ret1 = lag(ret,1),
         lag_ret2 = lag(ret,2),
         lag_ret3 = lag(ret,3),
         lag_ret4 = lag(ret,4),
         lag_ret5 = lag(ret,5),
         lag_iv301 = lag(iv30,1),
         lag_iv302 = lag(iv30,2),
         lag_iv303 = lag(iv30,3),
         lag_iv304 = lag(iv30,4),
         lag_iv305 = lag(iv30,5),
         sic = as.character(str_sub(co_sic, 1,2))) %>% 
  #sic = co_sic) %>% 
  select(date, ric, v30, co_sic, sic, v30, ret,se_neg, lag_se1, 
         lag_se2, lag_se3, lag_se4, lag_se5, lag_vol1, 
         lag_vol2, lag_vol3, lag_vol4, lag_vol5, lag_ret1, 
         lag_ret2, lag_ret3, lag_ret4, lag_ret5,
         ret1, ret2, ret3, ret4, ret5, 
         lag_iv301, lag_iv302, lag_iv303, lag_iv304, lag_iv305) %>% ungroup()


lm(v30 ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
     lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
     log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5)
   , data=df) %>% summary()


main_df %>% group_by(ric) %>% 
  summarise(mean = mean(iv30, na.rm = T)) %>% summarise(min = min(mean, na.rm = T), max = max(mean, na.rm = T))
