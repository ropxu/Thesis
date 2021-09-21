library(tidyverse)
library(RcppRoll)
library(modelr)
# Helper function to modify the table output
source('table_helper.R')

main_df <- read_csv('../data/main_data.csv')

main_df <- main_df %>% select(date, ric, co_sic, ret, vol, v10, everything())

df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se1 = lag(se_neg,1),
         lag_se2 = lag(se_neg,2),
         lag_se3 = lag(se_neg,3),
         lag_se4 = lag(se_neg,4),
         lag_se5 = lag(se_neg,5),
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
  select(date, ric, co_sic, sic, v30, ret,se_all, lag_se1, 
         lag_se2, lag_se3, lag_se4, lag_se5, lag_vol1, 
         lag_vol2, lag_vol3, lag_vol4, lag_vol5, lag_ret1, 
         lag_ret2, lag_ret3, lag_ret4, lag_ret5,
         lag_iv301, lag_iv302, lag_iv303, lag_iv304, lag_iv305) %>% ungroup()
  

# =========================== Only sentiment model ===========================
se_model1 <- lm(ret ~ lag_se1, data=df)
se_model2 <- lm(ret ~ lag_se1 + lag_se2, data=df)
se_model3 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3, data=df)
se_model4 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4, data=df)
se_model5 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5, data=df)


stargazer(se_model1, se_model2, se_model3, se_model4, se_model5,
          type = 'latex',
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
                               '$\\mathbf{BadNews_{t-5}}$'),
          out = '../thesis/tables/sentiment_model.tex')

# =========================== Controlled models ===========================
co_model1 <- lm(ret ~ lag_se1 + log(lag_vol1) + lag_ret1 + weekdays(date) + months(date), data=df)
co_model2 <- lm(ret ~ lag_se1 + lag_se2 + log(lag_vol1) + log(lag_vol2) + lag_ret1 + lag_ret2 +
                  weekdays(date) + months(date) + factor(sic), data=df)
co_model3 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + 
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) +
                  lag_ret1 + lag_ret2 + lag_ret3 + weekdays(date) + months(date) + factor(sic), data=df)

co_model4 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + weekdays(date) + months(date) +
                  factor(sic), data=df)

co_model5 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + weekdays(date) + months(date) +
                  factor(sic)
                , data=df)


stargazer(co_model1, co_model2, co_model3, co_model4, co_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'controlled_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/controlled_model.tex')

# =========================== Controlled + implied volatility models ===========================
iv_model1 <- lm(ret ~ 
                  lag_se1 + log(lag_vol1) + lag_ret1 +  lag_iv301 + weekdays(date) + months(date) + 
                  factor(sic)
                , data=df)

iv_model2 <- lm(ret ~ lag_se1 + lag_se2 + log(lag_vol1) + log(lag_vol2) + lag_ret1 + lag_ret2 +
                  lag_iv301 + lag_iv302 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model3 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + 
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_iv301 + lag_iv302 + lag_iv303 +
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model4 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_iv301 + lag_iv302 + lag_iv303 +
                  lag_iv304 + weekdays(date) + months(date) + factor(sic), data=df)

iv_model5 <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + log(lag_vol5) +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic)
                , data=df)


stargazer(iv_model1, iv_model2, iv_model3, iv_model4, iv_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'implied_vol_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{Ivol_{t-1}}$',
                               '$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-3}}$',
                               '$\\mathbf{Ivol_{t-4}}$','$\\mathbf{Ivol_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/implied_vola_model.tex')


# =========================== Implied voltility standalone ===========================


iv_standalone_model1 <- lm(ret ~ log(lag_vol1) + lag_ret1 +  lag_iv301 + weekdays(date) + months(date) +
                              factor(sic), data=df)

iv_standalone_model2 <- lm(ret ~ log(lag_vol1) + log(lag_vol2) + lag_ret1 + lag_ret2 +
                             lag_iv301 + lag_iv302 +
                             weekdays(date) + months(date) + factor(sic), data=df)

iv_standalone_model3 <- lm(ret ~
                             log(lag_vol1) + log(lag_vol2) + log(lag_vol3) +
                             lag_ret1 + lag_ret2 + lag_ret3 + lag_iv301 + lag_iv302 + lag_iv303 +
                             weekdays(date) + months(date) + factor(sic), data=df)

iv_standalone_model4 <- lm(ret ~
                             log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                             lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_iv301 + lag_iv302 + 
                             lag_iv303 + lag_iv304 + weekdays(date) + months(date) + 
                             factor(sic), data=df)

iv_standalone_model5 <- lm(ret ~ log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) + 
                             log(lag_vol5) + lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                             lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 +
                             weekdays(date) + months(date) + factor(sic)
                           , data = df)


stargazer(iv_standalone_model1, iv_standalone_model2, 
          iv_standalone_model3, iv_standalone_model4, iv_standalone_model5,
          type = 'latex',
          omit.stat = c('f', 'ser'),
          report = 'vct*',
          single.row = T,
          font.size = 'footnotesize',
          label = 'iv_standalone_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          dep.var.caption = '',
          covariate.labels = c('$\\mathbf{Ivol_{t-1}}$','$\\mathbf{Ivol_{t-2}}$',
                               '$\\mathbf{Ivol_{t-3}}$','$\\mathbf{Ivol_{t-4}}$',
                               '$\\mathbf{Ivol_{t-5}}$'),
          omit = c('Constant', 'log', 'ret', 'weekdays', 'months', 'factor'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/iv_standalone_model.tex')



# =========================== Encompassing IV ===========================

encompassing_iv_df <- df %>% 
  add_residuals(co_model1, var='mod1') %>% 
  add_residuals(co_model2, var='mod2') %>% 
  add_residuals(co_model3, var='mod3') %>% 
  add_residuals(co_model4, var='mod4') %>% 
  add_residuals(co_model5, var='mod5')
  

eiv_model1 <- lm(mod1 ~ lag_iv301, data=encompassing_iv_df)
eiv_model2 <- lm(mod2 ~ lag_iv301 + lag_iv302, data=encompassing_iv_df)
eiv_model3 <- lm(mod3 ~ lag_iv301 + lag_iv302 + lag_iv303, data=encompassing_iv_df)
eiv_model4 <- lm(mod4 ~ lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304, data=encompassing_iv_df)
eiv_model5 <- lm(mod5 ~ lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305, 
               data=encompassing_iv_df)

stargazer(eiv_model1, eiv_model2, eiv_model3, eiv_model4, eiv_model5,
          type = 'latex',
          report = 'vct*',
          dep.var.caption = "Residuals from sentiment model",
          omit = c('Constant'),
          font.size = 'footnotesize',
          label = 'encompassing_iv',
          single.row = T,
          dep.var.labels = c('','','','',''),
          covariate.labels = c('$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-2}}$',
                               '$\\mathbf{Ivol_{t-3}}$','$\\mathbf{Ivol_{t-4}}$',
                               '$\\mathbf{Ivol_{t-5}}$'),
          omit.stat = c('f', 'ser'),
          out = '../thesis/tables/encompassing_iv.tex')


# =========================== Encompassing Sentiment ===========================
encompassing_se_df <- df %>% 
  add_residuals(iv_standalone_model1, var='mod1') %>% 
  add_residuals(iv_standalone_model2, var='mod2') %>% 
  add_residuals(iv_standalone_model3, var='mod3') %>% 
  add_residuals(iv_standalone_model4, var='mod4') %>% 
  add_residuals(iv_standalone_model5, var='mod5')

ese_model1 <- lm(mod1 ~ lag_se1, data=encompassing_se_df)
ese_model2 <- lm(mod2 ~ lag_se1 + lag_se2, data=encompassing_se_df)
ese_model3 <- lm(mod3 ~ lag_se1 + lag_se2 + lag_se3, data=encompassing_se_df)
ese_model4 <- lm(mod4 ~ lag_se1 + lag_se2 + lag_se3 + lag_se4, data=encompassing_se_df)
ese_model5 <- lm(mod5 ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5, 
                 data=encompassing_se_df)


stargazer(ese_model1, ese_model2, ese_model3, ese_model4, ese_model5,
          type = 'latex',
          report = 'vct*',
          dep.var.caption = "Residuals from implied volatility model",
          omit = c('Constant'),
          font.size = 'footnotesize',
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$'),
          dep.var.labels = c('', '', '', '', ''),
          single.row = T,
          label = 'encompassing_se',
          omit.stat = c('f', 'ser'),
          out = '../thesis/tables/encompassing_se.tex')

# =========================== Model performance ===========================

benchmark_model <- lm(ret ~ log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + 
                      log(lag_vol4) + log(lag_vol5) +
                      lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                      weekdays(date) + months(date) + factor(sic), data=df)

BadNews_model <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                      log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                      log(lag_vol5) + lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                      weekdays(date) + months(date) + factor(sic), data=df)

implied_volatility_model <- lm(ret ~ log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                               log(lag_vol5) + lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                               lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 +
                               weekdays(date) + months(date) + factor(sic), data = df)

BadNews_vola_model <- lm(ret ~ lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                           log(lag_vol1) + log(lag_vol2) + log(lag_vol3) + log(lag_vol4) +
                           log(lag_vol5) + lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                           lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 +
                           weekdays(date) + months(date) + factor(sic), data = df)


benchmark <- rmse(benchmark_model, df) %>% enframe(name = 'model') %>% mutate(model = 'Bechmark model')
sentiment <- rmse(BadNews_model, df) %>% enframe(name = 'model') %>% mutate(model = 'Sentiment model')
iv <- rmse(implied_volatility_model, df) %>% 
  enframe(name = 'model') %>% mutate(model = 'Implied volatility model')
combined <- rmse(BadNews_vola_model, df) %>% enframe(name = 'model') %>% mutate(model = 'Combined model')

benchmark %>% bind_rows(sentiment) %>% 
  bind_rows(iv) %>% bind_rows(combined)
