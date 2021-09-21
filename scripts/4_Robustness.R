library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)
library(pracma)
library(RcppRoll)
library(modelr)
library(lubridate)


main_df <- read_csv('../data/main_data_no_after_hours.csv')

main_df <- main_df %>% select(date, ric, co_sic, ret, vol, everything())

df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se1 = lag(se_neg,1),
         lag_se2 = lag(se_neg,2),
         lag_se3 = lag(se_neg,3),
         lag_se4 = lag(se_neg,4),
         lag_se5 = lag(se_neg,5),
         lag_v1 = lag(v30,1),
         vol = log(vol),
         iv30 = v30 - iv30,
         lag_v2 = lag(v30,2),
         lag_v3 = lag(v30,3),
         lag_v4 = lag(v30,4),
         lag_v5 = lag(v30,5),
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
  select(date, ric, co_sic, sic, v30, ret,se_neg, lag_se1, vol, iv30,
         lag_se2, lag_se3, lag_se4, lag_se5, lag_vol1, 
         lag_vol2, lag_vol3, lag_vol4, lag_vol5, lag_ret1, 
         lag_ret2, lag_ret3, lag_ret4, lag_ret5,
         lag_v1, lag_v2, lag_v3, lag_v4, lag_v5, 
         lag_iv301, lag_iv302, lag_iv303, lag_iv304, lag_iv305) %>% ungroup()


df <- df %>% filter_at(vars(contains('iv')), all_vars(. > -200))

# ==== badnews ====

co_model1 <- lm(ret ~ 
                  lag_se1 + 
                  lag_vol1 + 
                  lag_ret1 + 
                  lag_v1 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

co_model2 <- lm(ret ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

co_model3 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + 
                  lag_vol1 + lag_vol2 + lag_vol3 +
                  lag_ret1 + lag_ret2 + lag_ret3 + 
                  lag_v1 + lag_v2 + lag_v3 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

co_model4 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 +
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

co_model5 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  weekdays(date) + months(date) + factor(sic), data=df)


nw_c1 <- coeftest(co_model1, vcovPL(co_model1, cluster = ~ ric, lag=1))
nw_c2 <- coeftest(co_model2, vcovPL(co_model2, cluster = ~ ric, lag=2))
nw_c3 <- coeftest(co_model3, vcovPL(co_model3, cluster = ~ ric, lag=3))
nw_c4 <- coeftest(co_model4, vcovPL(co_model4, cluster = ~ ric, lag=4))
nw_c5 <- coeftest(co_model5, vcovPL(co_model5, cluster = ~ ric, lag=5))

stargazer(co_model1, co_model2, co_model3, co_model4, co_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor' , 'lag_v'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          label = 'ret_sent',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_c1[,2], nw_c2[,2], nw_c3[,2], nw_c4[,2], nw_c5[,2]),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/bdnws_newey.tex')


# ==== Returns ====


ret_model1 <- lm(se_neg ~ 
                  lag_se1 + 
                  lag_vol1 + 
                  lag_ret1 + 
                  lag_v1 + 
                   weekdays(date) + months(date) + factor(sic), data=df)

ret_model2 <- lm(se_neg ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

ret_model3 <- lm(se_neg ~ 
                  lag_se1 + lag_se2 + lag_se3 + 
                  lag_vol1 + lag_vol2 + lag_vol3 +
                  lag_ret1 + lag_ret2 + lag_ret3 + 
                  lag_v1 + lag_v2 + lag_v3 + 
                   weekdays(date) + months(date) + factor(sic), data=df)

ret_model4 <- lm(se_neg ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

ret_model5 <- lm(se_neg ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

nw_r1 <- coeftest(ret_model1, vcovPL(ret_model1, cluster = ~ ric, lag=1))
nw_r2 <- coeftest(ret_model2, vcovPL(ret_model2, cluster = ~ ric, lag=2))
nw_r3 <- coeftest(ret_model3, vcovPL(ret_model3, cluster = ~ ric, lag=3))
nw_r4 <- coeftest(ret_model4, vcovPL(ret_model4, cluster = ~ ric, lag=4))
nw_r5 <- coeftest(ret_model5, vcovPL(ret_model5, cluster = ~ ric, lag=5))

stargazer(ret_model1, ret_model2, ret_model3, ret_model4, ret_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'lag_se', 'factor' , 'lag_v'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          label = 'sent_ret',
          dep.var.labels = '$\\mathbf{Badnews_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_r1[,2], nw_r2[,2], nw_r3[,2], nw_r4[,2], nw_r5[,2]),
          covariate.labels = c('$\\mathbf{Return_{t-1}}$', '$\\mathbf{Return_{t-2}}$',
                               '$\\mathbf{Return_{t-3}}$', '$\\mathbf{Return_{t-4}}$',
                               '$\\mathbf{Return_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/return_newey.tex')

# ==== Volume ====

vlm_model1 <- lm(vol ~ 
                   lag_se1 + 
                   lag_vol1 + 
                   lag_ret1 + 
                   lag_v1 + 
                   weekdays(date) + months(date) + factor(sic), data=df)

vlm_model2 <- lm(vol ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

vlm_model3 <- lm(vol ~ lag_se1 + lag_se2 + lag_se3 + 
                   lag_vol1 + lag_vol2 + lag_vol3 +
                   lag_ret1 + lag_ret2 + lag_ret3 + 
                   lag_v1 + lag_v2 + lag_v3 + 
                   weekdays(date) + months(date) + factor(sic), data=df)

vlm_model4 <- lm(vol ~ 
                   lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                   lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                   lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                   weekdays(date) + months(date) + factor(sic), data=df)

vlm_model5 <- lm(vol ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

nw_v1 <- coeftest(vlm_model1, vcovPL(vlm_model1, cluster = ~ ric, lag=1))
nw_v2 <- coeftest(vlm_model2, vcovPL(vlm_model2, cluster = ~ ric, lag=2))
nw_v3 <- coeftest(vlm_model3, vcovPL(vlm_model3, cluster = ~ ric, lag=3))
nw_v4 <- coeftest(vlm_model4, vcovPL(vlm_model4, cluster = ~ ric, lag=4))
nw_v5 <- coeftest(vlm_model5, vcovPL(vlm_model5, cluster = ~ ric, lag=5))

stargazer(vlm_model1, vlm_model2, vlm_model3, vlm_model4, vlm_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor' , 'lag_v'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          label = 'vol_sent',
          dep.var.labels = '$\\mathbf{Volume_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_v1[,2], nw_v2[,2], nw_v3[,2], nw_v4[,2], nw_v5[,2]),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/volume_newey.tex')

# ==== badnews + iv ====

iv_model1 <- lm(ret ~ 
                  lag_se1 + 
                  lag_vol1 + 
                  lag_ret1 + 
                  lag_v1 + 
                  lag_iv301 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model2 <- lm(ret ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  lag_iv301 + lag_iv302 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model3 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + 
                  lag_vol1 + lag_vol2 + lag_vol3 +
                  lag_ret1 + lag_ret2 + lag_ret3 + 
                  lag_v1 + lag_v2 + lag_v3 + 
                  lag_iv301 + lag_iv302 + lag_iv303 +
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model4 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

iv_model5 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 +
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic), data=df)


nw_iv1 <- coeftest(iv_model1, vcovPL(iv_model1, cluster = ~ ric, lag=1))
nw_iv2 <- coeftest(iv_model2, vcovPL(iv_model2, cluster = ~ ric, lag=2))
nw_iv3 <- coeftest(iv_model3, vcovPL(iv_model3, cluster = ~ ric, lag=3))
nw_iv4 <- coeftest(iv_model4, vcovPL(iv_model4, cluster = ~ ric, lag=4))
nw_iv5 <- coeftest(iv_model5, vcovPL(iv_model5, cluster = ~ ric, lag=5))

stargazer(iv_model1, iv_model2, iv_model3, iv_model4, iv_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'lag_v', 'factor'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          se = list(nw_iv1[,2], nw_iv2[,2], nw_iv3[,2], nw_iv4[,2], nw_iv5[,2]),
          label = 'ret_iv_sent',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{RVIV_{t-1}}$',
                               '$\\mathbf{RVIV_{t-2}}$','$\\mathbf{RVIV_{t-3}}$',
                               '$\\mathbf{RVIV_{t-4}}$','$\\mathbf{RVIV_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/badnews_iv_newey.tex')

# ==== badnews <- iv + badnews ====
          
bd_iv_model1 <- lm(se_neg ~ 
                    lag_se1 + 
                    lag_vol1 + 
                    lag_ret1 + 
                    lag_v1 + 
                    lag_iv301 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

bd_iv_model2 <- lm(se_neg ~ 
                    lag_se1 + lag_se2 + 
                    lag_vol1 + lag_vol2 + 
                    lag_ret1 + lag_ret2 +
                    lag_v1 + lag_v2 + 
                    lag_iv301 + lag_iv302 + 
                    weekdays(date) + months(date) + factor(sic), data=df)

bd_iv_model3 <- lm(se_neg ~ 
                    lag_se1 + lag_se2 + lag_se3 + 
                    lag_vol1 + lag_vol2 + lag_vol3 +
                    lag_ret1 + lag_ret2 + lag_ret3 + 
                    lag_v1 + lag_v2 + lag_v3 + 
                    lag_iv301 + lag_iv302 + lag_iv303 +
                    weekdays(date) + months(date) + factor(sic), data=df)

bd_iv_model4 <- lm(se_neg ~ 
                     lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                    lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                    lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                    lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                    lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                    weekdays(date) + months(date) + factor(sic), data=df)

bd_iv_model5 <- lm(se_neg ~ 
                    lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                    lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                    lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                    lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                    lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                    weekdays(date) + months(date) + factor(sic), data=df)


nw_bd_iv1 <- coeftest(bd_iv_model1, vcovPL(bd_iv_model1, cluster = ~ ric, lag=1))
nw_bd_iv2 <- coeftest(bd_iv_model2, vcovPL(bd_iv_model2, cluster = ~ ric, lag=2))
nw_bd_iv3 <- coeftest(bd_iv_model3, vcovPL(bd_iv_model3, cluster = ~ ric, lag=3))
nw_bd_iv4 <- coeftest(bd_iv_model4, vcovPL(bd_iv_model4, cluster = ~ ric, lag=4))
nw_bd_iv5 <- coeftest(bd_iv_model5, vcovPL(bd_iv_model5, cluster = ~ ric, lag=5))



stargazer(bd_iv_model1, bd_iv_model2, bd_iv_model3, bd_iv_model4, bd_iv_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'lag_v', 'factor'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          se = list(nw_bd_iv1[,2], nw_bd_iv2[,2], nw_bd_iv3[,2], nw_bd_iv4[,2], nw_bd_iv5[,2]),
          label = 'sent_iv_sent',
          dep.var.labels = '$\\mathbf{BadNews_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{RVIV_{t-1}}$',
                               '$\\mathbf{RVIV_{t-2}}$','$\\mathbf{RVIV_{t-3}}$',
                               '$\\mathbf{RVIV_{t-4}}$','$\\mathbf{RVIV_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/bd_ivbd_newey.tex')


# ==== iv <- iv + badnews ====

iv_bd_model1 <- lm(iv30 ~ 
                    lag_se1 + 
                    lag_vol1 + 
                    lag_ret1 + 
                    lag_v1 + 
                    lag_iv301 + 
                    weekdays(date) + months(date) + factor(sic), data=df)

iv_bd_model2 <- lm(iv30 ~ 
                    lag_se1 + lag_se2 + 
                    lag_vol1 + lag_vol2 + 
                    lag_ret1 + lag_ret2 +
                    lag_v1 + lag_v2 + 
                    lag_iv301 + lag_iv302 + 
                    weekdays(date) + months(date) + factor(sic), data=df)

iv_bd_model3 <- lm(iv30 ~ 
                    lag_se1 + lag_se2 + lag_se3 + 
                    lag_vol1 + lag_vol2 + lag_vol3 +
                    lag_ret1 + lag_ret2 + lag_ret3 + 
                    lag_v1 + lag_v2 + lag_v3 + 
                    lag_iv301 + lag_iv302 + lag_iv303 +
                    weekdays(date) + months(date) + factor(sic), data=df)

iv_bd_model4 <- lm(iv30 ~ 
                    lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                    lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                    lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                    lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 +
                    lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                    weekdays(date) + months(date) + factor(sic), data=df)

iv_bd_model5 <- lm(iv30 ~ 
                    lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                    lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                    lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                    lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                    lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                    weekdays(date) + months(date) + factor(sic), data=df)


nw_iv_bd1 <- coeftest(iv_bd_model1, vcovPL(iv_bd_model1, cluster = ~ ric, lag=1))
nw_iv_bd2 <- coeftest(iv_bd_model2, vcovPL(iv_bd_model2, cluster = ~ ric, lag=2))
nw_iv_bd3 <- coeftest(iv_bd_model3, vcovPL(iv_bd_model3, cluster = ~ ric, lag=3))
nw_iv_bd4 <- coeftest(iv_bd_model4, vcovPL(iv_bd_model4, cluster = ~ ric, lag=4))
nw_iv_bd5 <- coeftest(iv_bd_model5, vcovPL(iv_bd_model5, cluster = ~ ric, lag=5))



stargazer(iv_bd_model1, iv_bd_model2, iv_bd_model3, iv_bd_model4, iv_bd_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'lag_v', 'factor'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          # single.row = T,
          se = list(nw_iv_bd1[,2], nw_iv_bd2[,2], nw_iv_bd3[,2], nw_iv_bd4[,2], nw_iv_bd5[,2]),
          label = 'iv_iv_sent',
          dep.var.labels = '$\\mathbf{Implied volatility_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{RVIV_{t-1}}$',
                               '$\\mathbf{RVIV_{t-2}}$','$\\mathbf{RVIV_{t-3}}$',
                               '$\\mathbf{RVIV_{t-4}}$','$\\mathbf{RVIV_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/iv_ivbd_newey.tex')


# ==== realized-iv spread only ====

iv_standalone_model1 <- lm(ret ~ 
                            lag_vol1 +
                            lag_ret1 + 
                            lag_iv301 + 
                            lag_v1 + 
                            weekdays(date) + months(date) + factor(sic) , data=df)

iv_standalone_model2 <- lm(ret ~ 
                             lag_vol1 + lag_vol2 +
                             lag_ret1 + lag_ret2 +
                             lag_v1 + lag_v2 + 
                             lag_iv301 + lag_iv302 + 
                             weekdays(date) + months(date) + factor(sic), data=df)

iv_standalone_model3 <- lm(ret ~ 
                            lag_vol1 + lag_vol2 + lag_vol3 +
                            lag_v1 + lag_v2 + lag_v3 + 
                            lag_ret1 + lag_ret2 + lag_ret3 + 
                            lag_iv301 + lag_iv302 + lag_iv303 +
                            weekdays(date) + months(date) + factor(sic), data=df)

iv_standalone_model4 <- lm(ret ~ 
                            lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                            lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                            lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                            lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 +
                            weekdays(date) + months(date) + factor(sic), data=df)

iv_standalone_model5 <- lm(ret ~ 
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + months(date) + factor(sic), data=df)

nw_iv_only1 <- coeftest(iv_standalone_model1, vcovPL(iv_standalone_model1, cluster = ~ ric, lag=1))
nw_iv_only2 <- coeftest(iv_standalone_model2, vcovPL(iv_standalone_model2, cluster = ~ ric, lag=2))
nw_iv_only3 <- coeftest(iv_standalone_model3, vcovPL(iv_standalone_model3, cluster = ~ ric, lag=3))
nw_iv_only4 <- coeftest(iv_standalone_model4, vcovPL(iv_standalone_model4, cluster = ~ ric, lag=4))
nw_iv_only5 <- coeftest(iv_standalone_model5, vcovPL(iv_standalone_model5, cluster = ~ ric, lag=5))

stargazer(iv_standalone_model1, iv_standalone_model2, iv_standalone_model3, iv_standalone_model4,
          iv_standalone_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'months', 'log', 'ret', 'factor' , 'lag_v'),
          # report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          covariate.labels = c('$\\mathbf{RVIV_{t-1}}$', '$\\mathbf{RVIV_{t-2}}$',
                               '$\\mathbf{RVIV_{t-3}}$', '$\\mathbf{RVIV_{t-4}}$',
                               '$\\mathbf{RVIV_{t-5}}$'),
          # single.row = T,
          label = 'rviv_only',
          se = list(nw_iv_only1[,2], nw_iv_only2[,2], nw_iv_only3[,2], nw_iv_only4[,2], nw_iv_only5[,2]),
          dep.var.labels = '$\\mathbf{Return_t}$',
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Fixed effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          omit.stat = c('f', 'ser'))
          # out = '../thesis/tables/rviv_only.tex')


# ==== Encompassing ====

encompassing_df <- df %>% 
  # Add residuals
  add_residuals(iv_standalone_model1, var='iv_mod1') %>% 
  add_residuals(iv_standalone_model2, var='iv_mod2') %>% 
  add_residuals(iv_standalone_model3, var='iv_mod3') %>% 
  add_residuals(iv_standalone_model4, var='iv_mod4') %>% 
  add_residuals(iv_standalone_model5, var='iv_mod5') %>% 
  add_residuals(co_model1, var='se_mod1') %>% 
  add_residuals(co_model2, var='se_mod2') %>% 
  add_residuals(co_model3, var='se_mod3') %>% 
  add_residuals(co_model4, var='se_mod4') %>% 
  add_residuals(co_model5, var='se_mod5') %>% 
  # Add predictions
  add_predictions(iv_standalone_model1, var='iv_mod1_pred') %>% 
  add_predictions(iv_standalone_model2, var='iv_mod2_pred') %>% 
  add_predictions(iv_standalone_model3, var='iv_mod3_pred') %>% 
  add_predictions(iv_standalone_model4, var='iv_mod4_pred') %>% 
  add_predictions(iv_standalone_model5, var='iv_mod5_pred') %>% 
  add_predictions(co_model1, var='se_mod1_pred') %>% 
  add_predictions(co_model2, var='se_mod2_pred') %>% 
  add_predictions(co_model3, var='se_mod3_pred') %>% 
  add_predictions(co_model4, var='se_mod4_pred') %>% 
  add_predictions(co_model5, var='se_mod5_pred')

m1 <- lm(se_mod1 ~ iv_mod1_pred, data=encompassing_df)
m2 <- lm(iv_mod1 ~ se_mod1_pred, data=encompassing_df)
m3 <- lm(se_mod2 ~ iv_mod2_pred, data=encompassing_df)
m4 <- lm(iv_mod2 ~ se_mod2_pred, data=encompassing_df)
m5 <- lm(se_mod3 ~ iv_mod3_pred, data=encompassing_df)
m6 <- lm(iv_mod3 ~ se_mod3_pred, data=encompassing_df)
m7 <- lm(se_mod4 ~ iv_mod4_pred, data=encompassing_df)
m8 <- lm(iv_mod4 ~ se_mod4_pred, data=encompassing_df)
m9 <- lm(se_mod5 ~ iv_mod5_pred, data=encompassing_df)
m10 <- lm(iv_mod5 ~ se_mod5_pred, data=encompassing_df)



stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
          type='text',
          column.labels = c('1 lag', '2 lags', '3 lags', '4 lags', '5 lags'),
          column.separate = c(2, 2, 2, 2, 2),
          font.size = 'footnotesize',
          omit = c('Constant'),
          omit.stat = c('f', 'ser'))


# ==== Plots ====


plot_width <- 5.5
plot_height <- 3



df %>% group_by(date) %>% tally() %>% filter(!weekdays(date) %in% c('Sunday', 'Saturday')) %>% 
  ggplot(aes(date, n)) + 
  geom_line() +
  expand_limits(y = 0) + 
  theme_linedraw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(x = 'Date', y = 'Number of news') + 
  ggsave('../thesis/figures/news_timeseries.pdf',
       device = 'pdf', dpi = 'retina', width = plot_width, height = plot_height)


df %>% mutate(ret = ret / 100) %>% ggplot(aes(se_neg, ret)) + 
  geom_point(color = '#0f0f0f', alpha = 0.2, size=2) +
  scale_y_continuous(labels = scales::percent, limits = c(NA, NA)) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_linedraw() + 
  theme(panel.grid = element_blank()) +
  labs(x = 'Negative sentiment', y = 'Return')  +
  ggsave('../thesis/figures/sent_scatter.png',
         device = 'png', dpi = 'retina', width = plot_width, height = plot_height)


df %>%  mutate(ret = ret / 100, iv30 = iv30 / 100) %>%
  ggplot(aes(iv30, ret)) + 
  geom_point(color = '#0f0f0f', alpha = 0.2, size=2) +
  scale_y_continuous(labels = scales::percent, limits = c(NA, 0.4)) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_linedraw() + 
  labs(x = 'Implied volatility', y='Return') + 
  theme(panel.grid = element_blank())
  # ggsave('../thesis/figures/iv_scatter.pdf',
  #        device = 'pdf', dpi = 'retina', width = plot_width, height = plot_height)


main_df %>% filter(se_neg != 0) %>% pull(se_neg) %>% mean()
main_df %>% pull(iv30) %>% sd(na.rm = T) * 0.011 * 100


library(pander)
myModel <- function(df) {
  lm(ret ~ 
          lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
          lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
          lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
          lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 +
          # lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 +
          weekdays(date) + factor(sic)
     , data=df)
}

df %>% drop_na() %>% group_by(month = quarter(date), month_name = quarter(date), year = year(date)) %>% 
  nest() %>%  
  mutate(model = map(data, myModel),
         tidy = map(model, broom::tidy),
         glance = map(model, broom::glance)) %>% 
  unnest(tidy) %>% filter(term %in% c('lag_se1', 'lag_se2','lag_se3','lag_se4','lag_se5',
                                      'lag_iv301', 'lag_iv302', 'lag_iv303', 'lag_iv304', 'lag_iv305')) %>% 
  ungroup() %>% 
  
  mutate(coef = paste(round(estimate,3), 
                      pander::add.significance.stars(p.value, cutoffs = c(0.1, 0.05, 0.01)), sep=''),
         coef = str_replace_all(coef, ' ', '')) %>% 
  select(-p.value, -statistic, -estimate) %>% select(month_name, year, term, coef, std.error, glance) %>% 
  select(-std.error) %>%
  pivot_wider(names_from = term, values_from = c('coef')) %>% 
  unnest(glance) %>% select(-r.squared, -sigma, -statistic, -p.value,
                            -df, -logLik, -AIC, -BIC, -deviance, -df.residual) %>% 
  select(month_name, year, lag_se1:lag_se5, adj.r.squared) %>%
  rename(Quarter = month_name) %>% 
  knitr::kable(format = 'latex',
               digits = 3,
               booktabs=T, caption = '') %>%
  kableExtra::kable_styling(position = 'center',) %>% kableExtra::landscape()
  

df %>% group_by(month = month(date), year = year(date)) %>% 
  summarise(mean = mean(iv30, na.rm = T),
            median = median(iv30, na.rm = T))


df %>% group_by(date) %>% 
  summarise(mean = mean(iv30)) %>% ggplot(aes(date, mean)) + geom_line()


df %>% 
  group_by(
    months = month(date),
    month = months(date),
    year = year(date),
  ) %>% add_tally() %>% group_by(months, month, year, n) %>% nest() %>% 
  mutate(
    mean = map_dbl(data, ~mean(.$iv30, na.rm = T)),
    median = map_dbl(data, ~median(.$iv30, na.rm = T)),
    sd = map_dbl(data, ~sd(.$iv30, na.rm = T)),
    min = map_dbl(data, ~min(.$iv30, na.rm = T)),
    max = map_dbl(data, ~max(.$iv30, na.rm = T)))




  
ggplot(df, aes(lag_iv301, ret)) + geom_point() + geom_smooth(method = 'lm')


df %>% 
  group_by(date) %>% 
  summarise(RVIV = mean(iv30, na.rm = T),
            Return = mean(ret, na.rm = T),
            BadNews = mean(se_neg, na.rm = T)) %>% 
  mutate(`Cumulative return` = cumprod(Return/ 100 + 1) -1 ,
         RVIV = RVIV / 100,
         BadNews = BadNews - mean(BadNews, na.rm = T)) %>% 
  add_row(date = '2018-09-10', RVIV = NA, `Cumulative return` = 0, BadNews = NA, .before = 1) %>% 
  pivot_longer(names_to = 'name', values_to = 'value', -date) %>% 
  filter(name != c('BadNews', 'Return')) %>%
  # filter(!name %in% c('Return', 'RVIV')) %>%
  ggplot(aes(date, value), color = '#112222')  + 
  geom_line(aes(linetype=name)) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.margin=margin(-15, 0, 0, 0)) +
  labs(y = '', x = '') +
  ggsave('../thesis/figures/rviv_ret.pdf',
         device = 'pdf', dpi = 'retina', width = plot_width, height = plot_height)
    



df %>% mutate(se = if_else(se_neg != 0,'yes','no')) %>% 
  select(date, ret, se) %>% 
  filter(ret != 0) %>% 
  ggplot(aes(x = ret, fill=se)) + geom_histogram(bins = 40)
  

lm(ret ~ se_neg, data=df) %>% summary()
lm(ret ~ iv30, data=df) %>% summary()


