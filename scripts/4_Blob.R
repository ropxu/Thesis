library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)

main_df <- read_csv('../textblob.csv')


df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se1 = lag(polarity,1),
         lag_se2 = lag(polarity,2),
         lag_se3 = lag(polarity,3),
         lag_se4 = lag(polarity,4),
         lag_se5 = lag(polarity,5),
         lag_v1 = lag(var,1),
         lag_v2 = lag(var,2),
         lag_v3 = lag(var,3),
         lag_v4 = lag(var,4),
         lag_v5 = lag(var,5),
         lag_vol1 = lag(volume,1),
         lag_vol2 = lag(volume,2),
         lag_vol3 = lag(volume,3),
         lag_vol4 = lag(volume,4),
         lag_vol5 = lag(volume,5),
         lag_ret1 = lag(ret,1),
         lag_ret2 = lag(ret,2),
         lag_ret3 = lag(ret,3),
         lag_ret4 = lag(ret,4),
         lag_ret5 = lag(ret,5),
         lag_iv301 = lag(ivol_30,1),
         lag_iv302 = lag(ivol_30,2),
         lag_iv303 = lag(ivol_30,3),
         lag_iv304 = lag(ivol_30,4),
         lag_iv305 = lag(ivol_30,5),
         # sic = as.character(str_sub(co_sic, 1,2))) %>%
         sic = sic) %>% 
  select(date, ric, co_sic, sic, var, ret, polarity, lag_se1, volume, ivol_30, lag_no_news, jan,
         lag_se2, lag_se3, lag_se4, lag_se5, lag_vol1, 
         lag_vol2, lag_vol3, lag_vol4, lag_vol5, lag_ret1, 
         lag_ret2, lag_ret3, lag_ret4, lag_ret5,
         lag_v1, lag_v2, lag_v3, lag_v4, lag_v5, 
         lag_iv301, lag_iv302, lag_iv303, lag_iv304, lag_iv305) %>% ungroup()



# ==== Predict returns with sentiment ====

co_model1 <- lm(ret ~ 
                  lag_se1 + 
                  lag_vol1 + 
                  lag_ret1 + 
                  lag_v1 + 
                  weekdays(date) + jan + factor(sic), data=df)

co_model2 <- lm(ret ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  weekdays(date) + jan + factor(sic), data=df)

co_model3 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + 
                  lag_vol1 + lag_vol2 + lag_vol3 +
                  lag_ret1 + lag_ret2 + lag_ret3 + 
                  lag_v1 + lag_v2 + lag_v3 + 
                  weekdays(date) + jan + factor(sic), data=df)

co_model4 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 +
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                  weekdays(date) + jan + factor(sic), data=df)

co_model5 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 +
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  weekdays(date) + jan + factor(sic), data=df)


nw_c1 <- coeftest(co_model1, NeweyWest(co_model1, lag=1, prewhite = F))
nw_c2 <- coeftest(co_model2, NeweyWest(co_model2, lag=2, prewhite = F))
nw_c3 <- coeftest(co_model3, NeweyWest(co_model3, lag=3, prewhite = F))
nw_c4 <- coeftest(co_model4, NeweyWest(co_model4, lag=4, prewhite = F))
nw_c5 <- coeftest(co_model5, NeweyWest(co_model5, lag=5, prewhite = F))

stargazer(co_model1, co_model2, co_model3, co_model4, co_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'jan', 'log', 'ret', 'factor' , 'lag_v'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'return_newey',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_c1[,2], nw_c2[,2], nw_c3[,2], nw_c4[,2], nw_c5[,2]),
          covariate.labels = c('$\\mathbf{Sentiment_{t-1}}$', '$\\mathbf{Sentiment_{t-2}}$',
                               '$\\mathbf{Sentiment_{t-3}}$', '$\\mathbf{Sentiment_{t-4}}$',
                               '$\\mathbf{Sentiment_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Fixed effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out='../thesis/tables/blob_ret.tex')


# ==== Predict sentiment with returns ====
ret_model1 <- lm(polarity ~ 
                   lag_se1 + 
                   lag_vol1 + 
                   lag_ret1 + 
                   lag_v1 + 
                   weekdays(date) + jan + factor(sic), data=df)

ret_model2 <- lm(polarity ~ 
                   lag_se1 + lag_se2 + 
                   lag_vol1 + lag_vol2 + 
                   lag_ret1 + lag_ret2 +
                   lag_v1 + lag_v2 + 
                   weekdays(date) + jan + factor(sic), data=df)

ret_model3 <- lm(polarity ~ 
                   lag_se1 + lag_se2 + lag_se3 + 
                   lag_vol1 + lag_vol2 + lag_vol3 +
                   lag_ret1 + lag_ret2 + lag_ret3 + 
                   lag_v1 + lag_v2 + lag_v3 + 
                   weekdays(date) + jan + factor(sic), data=df)

ret_model4 <- lm(polarity ~ 
                   lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                   lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                   lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                   weekdays(date) + jan + factor(sic), data=df)

ret_model5 <- lm(polarity ~ 
                   lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                   lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                   lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                   weekdays(date) + jan + factor(sic), data=df)

nw_r1 <- coeftest(ret_model1, NeweyWest(ret_model1, lag=1, prewhite = F))
nw_r2 <- coeftest(ret_model2, NeweyWest(ret_model2, lag=2, prewhite = F))
nw_r3 <- coeftest(ret_model3, NeweyWest(ret_model3, lag=3, prewhite = F))
nw_r4 <- coeftest(ret_model4, NeweyWest(ret_model4, lag=4, prewhite = F))
nw_r5 <- coeftest(ret_model5, NeweyWest(ret_model5, lag=5, prewhite = F))

stargazer(ret_model1, ret_model2, ret_model3, ret_model4, ret_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'jan', 'lag_no_news',  'log', 'lag_se', 'factor' , 'lag_v'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'controlled_model',
          dep.var.labels = '$\\mathbf{Sentiment_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_r1[,2], nw_r2[,2], nw_r3[,2], nw_r4[,2], nw_r5[,2]),
          covariate.labels = c('$\\mathbf{Return_{t-1}}$', '$\\mathbf{Return_{t-2}}$',
                               '$\\mathbf{Return_{t-3}}$', '$\\mathbf{Return_{t-4}}$',
                               '$\\mathbf{Return_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Fixed effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/blob_sentiment.tex')

# ==== Predict volume with sentiment ====

vlm_model1 <- lm(volume ~ 
                   lag_se1 + 
                   lag_vol1 +
                   abs(lag_se1) +
                   lag_ret1 + 
                   lag_v1 + 
                   weekdays(date) + jan + factor(sic), data=df)

vlm_model2 <- lm(volume ~ 
                   lag_se1 + lag_se2 + 
                   lag_vol1 + lag_vol2 + 
                   abs(lag_se1) + abs(lag_se2) +
                   lag_ret1 + lag_ret2 +
                   lag_v1 + lag_v2 + 
                   weekdays(date) + jan + factor(sic), data=df)

vlm_model3 <- lm(volume ~ lag_se1 + lag_se2 + lag_se3 + 
                   lag_vol1 + lag_vol2 + lag_vol3 +
                   abs(lag_se1) + abs(lag_se2) + abs(lag_se3) +
                   lag_ret1 + lag_ret2 + lag_ret3 + 
                   lag_v1 + lag_v2 + lag_v3 + 
                   weekdays(date) + jan + factor(sic), data=df)

vlm_model4 <- lm(volume ~ 
                   lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                   lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                   abs(lag_se1) + abs(lag_se2) + abs(lag_se3) + abs(lag_se4) +
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                   lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                   weekdays(date) + jan + factor(sic), data=df)

vlm_model5 <- lm(volume ~ 
                   lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                   lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                   abs(lag_se1) + abs(lag_se2) + abs(lag_se3) + abs(lag_se4) + abs(lag_se5) + 
                   lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                   lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                   weekdays(date) + jan + factor(sic), data=df)

nw_v1 <- coeftest(vlm_model1, NeweyWest(vlm_model1, lag=1, prewhite = F))
nw_v2 <- coeftest(vlm_model2, NeweyWest(vlm_model2, lag=2, prewhite = F))
nw_v3 <- coeftest(vlm_model3, NeweyWest(vlm_model3, lag=3, prewhite = F))
nw_v4 <- coeftest(vlm_model4, NeweyWest(vlm_model4, lag=4, prewhite = F))
nw_v5 <- coeftest(vlm_model5, NeweyWest(vlm_model5, lag=5, prewhite = F))

stargazer(vlm_model1, vlm_model2, vlm_model3, vlm_model4, vlm_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'jan', 'log', 'ret', 'factor' , 'lag_v'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          label = 'controlled_model',
          dep.var.labels = '$\\mathbf{Volume_t}$',
          omit.stat = c('f', 'ser'),
          se = list(nw_v1[,2], nw_v2[,2], nw_v3[,2], nw_v4[,2], nw_v5[,2]),
          # covariate.labels = c('$\\mathbf{Sentiment_{t-1}}$', '$\\mathbf{Sentiment_{t-2}}$',
          #                      '$\\mathbf{Sentiment_{t-3}}$', '$\\mathbf{Sentiment_{t-4}}$',
          #                      '$\\mathbf{Sentiment_{t-5}}$','$\\left|\\mathbf{Sentiment_{t-1}}\\right|$',
          #                        '$\\left|\\mathbf{Sentiment_{t-2}}\\right|$', 
          #                        '$\\left|\\mathbf{Sentiment_{t-3}}\\right|$',
          #                        '$\\left|\\mathbf{Sentiment_{t-4}}\\right|$',
          #                      '$\\left|\\mathbf{Sentiment_{t-5}}\\right|$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Fidex effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          out = '../thesis/tables/blob_volume.tex')


# ==== Predict returns with sentiment and implied volatility ====

iv_model1 <- lm(ret ~ 
                  lag_se1 + 
                  lag_vol1 + 
                  lag_ret1 + 
                  lag_v1 + 
                  lag_iv301 + 
                  weekdays(date) + jan + factor(sic), data=df)

iv_model2 <- lm(ret ~ 
                  lag_se1 + lag_se2 + 
                  lag_vol1 + lag_vol2 + 
                  lag_ret1 + lag_ret2 +
                  lag_v1 + lag_v2 + 
                  lag_iv301 + lag_iv302 + 
                  weekdays(date) + jan + factor(sic), data=df)

iv_model3 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + 
                  lag_vol1 + lag_vol2 + lag_vol3 +
                  lag_ret1 + lag_ret2 + lag_ret3 + 
                  lag_v1 + lag_v2 + lag_v3 + 
                  lag_iv301 + lag_iv302 + lag_iv303 +
                  weekdays(date) + jan + factor(sic), data=df)

iv_model4 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                  weekdays(date) + jan + factor(sic), data=df)

iv_model5 <- lm(ret ~ 
                  lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                  lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                  lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                  lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                  lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                  weekdays(date) + jan + factor(sic), data=df)


nw_iv1 <- coeftest(iv_model1, NeweyWest(iv_model1, lag=1, prewhite = F))
nw_iv2 <- coeftest(iv_model2, NeweyWest(iv_model2, lag=2, prewhite = F))
nw_iv3 <- coeftest(iv_model3, NeweyWest(iv_model3, lag=3, prewhite = F))
nw_iv4 <- coeftest(iv_model4, NeweyWest(iv_model4, lag=4, prewhite = F))
nw_iv5 <- coeftest(iv_model5, NeweyWest(iv_model5, lag=5, prewhite = F))

stargazer(iv_model1, iv_model2, iv_model3, iv_model4, iv_model5,
          type = 'latex',
          omit = c('Constant', 'weekdays', 'jan', 'log', 'ret', 'lag_v', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          se = list(nw_iv1[,2], nw_iv2[,2], nw_iv3[,2], nw_iv4[,2], nw_iv5[,2]),
          label = 'implied_vol_model',
          dep.var.labels = '$\\mathbf{Return_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{Sentiment_{t-1}}$', '$\\mathbf{Sentiment_{t-2}}$',
                               '$\\mathbf{Sentiment_{t-3}}$', '$\\mathbf{Sentiment_{t-4}}$',
                               '$\\mathbf{Sentiment_{t-5}}$', '$\\mathbf{Ivol_{t-1}}$',
                               '$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-3}}$',
                               '$\\mathbf{Ivol_{t-4}}$','$\\mathbf{Ivol_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Fixed effect', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
         out = '../thesis/tables/blob_iv.tex')

# ==== badnews <- iv + badnews ====

bd_iv_model1 <- lm(polarity ~ 
                     lag_se1 + 
                     lag_vol1 + 
                     lag_ret1 + 
                     lag_v1 + 
                     lag_iv301 + 
                     weekdays(date) + jan + factor(sic), data=df)

bd_iv_model2 <- lm(polarity ~ 
                     lag_se1 + lag_se2 + 
                     lag_vol1 + lag_vol2 + 
                     lag_ret1 + lag_ret2 +
                     lag_v1 + lag_v2 + 
                     lag_iv301 + lag_iv302 + 
                     weekdays(date) + jan + factor(sic), data=df)

bd_iv_model3 <- lm(polarity ~ 
                     lag_se1 + lag_se2 + lag_se3 + 
                     lag_vol1 + lag_vol2 + lag_vol3 +
                     lag_ret1 + lag_ret2 + lag_ret3 + 
                     lag_v1 + lag_v2 + lag_v3 + 
                     lag_iv301 + lag_iv302 + lag_iv303 +
                     weekdays(date) + jan + factor(sic), data=df)

bd_iv_model4 <- lm(polarity ~ 
                     lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                     lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                     lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + 
                     lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                     lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                     weekdays(date) + jan + factor(sic), data=df)

bd_iv_model5 <- lm(polarity ~ 
                     lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                     lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                     lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                     lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                     lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                     weekdays(date) + jan + factor(sic), data=df)


nw_bd_iv1 <- coeftest(bd_iv_model1, NeweyWest(bd_iv_model1, lag=1, prewhite = F))
nw_bd_iv2 <- coeftest(bd_iv_model2, NeweyWest(bd_iv_model2, lag=2, prewhite = F))
nw_bd_iv3 <- coeftest(bd_iv_model3, NeweyWest(bd_iv_model3, lag=3, prewhite = F))
nw_bd_iv4 <- coeftest(bd_iv_model4, NeweyWest(bd_iv_model4, lag=4, prewhite = F))
nw_bd_iv5 <- coeftest(bd_iv_model5, NeweyWest(bd_iv_model5, lag=5, prewhite = F))



stargazer(bd_iv_model1, bd_iv_model2, bd_iv_model3, bd_iv_model4, bd_iv_model5,
          type = 'text',
          omit = c('Constant', 'weekdays', 'jan', 'log', 'ret', 'lag_v', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          se = list(nw_bd_iv1[,2], nw_bd_iv2[,2], nw_bd_iv3[,2], nw_bd_iv4[,2], nw_bd_iv5[,2]),
          label = 'implied_vol_model',
          dep.var.labels = '$\\mathbf{BadNews_t}$',
          omit.stat = c('f', 'ser'),
          covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
                               '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
                               '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{Ivol_{t-1}}$',
                               '$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-3}}$',
                               '$\\mathbf{Ivol_{t-4}}$','$\\mathbf{Ivol_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
# out = '../thesis/tables/blob_pred_se_ivse.tex')



# ==== iv <- iv + badnews ====

iv_bd_model1 <- lm(ivol_30 ~ 
                     lag_se1 + 
                     # abs(lag_se1) + 
                     lag_vol1 + 
                     lag_ret1 + 
                     lag_v1 + 
                     lag_iv301 + 
                     weekdays(date) + jan + factor(sic), data=df)

iv_bd_model2 <- lm(ivol_30 ~ 
                     lag_se1 + lag_se2 + 
                     # abs(lag_se1) + abs(lag_se2) +
                     lag_vol1 + lag_vol2 + 
                     lag_ret1 + lag_ret2 +
                     lag_v1 + lag_v2 + 
                     lag_iv301 + lag_iv302 + 
                     weekdays(date) + jan + factor(sic), data=df)

iv_bd_model3 <- lm(ivol_30 ~ 
                     lag_se1 + lag_se2 + lag_se3 + 
                     # abs(lag_se1) + abs(lag_se2) + abs(lag_se3) + 
                     lag_vol1 + lag_vol2 + lag_vol3 +
                     lag_ret1 + lag_ret2 + lag_ret3 + 
                     lag_v1 + lag_v2 + lag_v3 + 
                     lag_iv301 + lag_iv302 + lag_iv303 +
                     weekdays(date) + jan + factor(sic), data=df)

iv_bd_model4 <- lm(ivol_30 ~ 
                     lag_se1 + lag_se2 + lag_se3 + lag_se4 +
                     # abs(lag_se1) + abs(lag_se2) + abs(lag_se3) + abs(lag_se4) + 
                     lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 +
                     lag_v1 + lag_v2 + lag_v3 + lag_v4 + 
                     lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 +
                     lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + 
                     weekdays(date) + jan + factor(sic), data=df)

iv_bd_model5 <- lm(ivol_30 ~ 
                     lag_se1 + lag_se2 + lag_se3 + lag_se4 + lag_se5 +
                     lag_vol1 + lag_vol2 + lag_vol3 + lag_vol4 + lag_vol5 +
                     # abs(lag_se1) + abs(lag_se2) + abs(lag_se3) + abs(lag_se4) + abs(lag_se5) +
                     lag_ret1 + lag_ret2 + lag_ret3 + lag_ret4 + lag_ret5 + 
                     lag_v1 + lag_v2 + lag_v3 + lag_v4 + lag_v5 + 
                     lag_iv301 + lag_iv302 + lag_iv303 + lag_iv304 + lag_iv305 + 
                     weekdays(date) + jan + factor(sic), data=df)


nw_iv_bd1 <- coeftest(iv_bd_model1, NeweyWest(iv_bd_model1, lag=1, prewhite = F))
nw_iv_bd2 <- coeftest(iv_bd_model2, NeweyWest(iv_bd_model2, lag=2, prewhite = F))
nw_iv_bd3 <- coeftest(iv_bd_model3, NeweyWest(iv_bd_model3, lag=3, prewhite = F))
nw_iv_bd4 <- coeftest(iv_bd_model4, NeweyWest(iv_bd_model4, lag=4, prewhite = F))
nw_iv_bd5 <- coeftest(iv_bd_model5, NeweyWest(iv_bd_model5, lag=5, prewhite = F))



stargazer(iv_bd_model1, iv_bd_model2, iv_bd_model3, iv_bd_model4, iv_bd_model5,
          type = 'text',
          omit = c('Constant', 'jan', 'months', 'log', 'ret', 'lag_v', 'factor'),
          report = 'vct*',
          dep.var.caption = "",
          font.size = 'footnotesize',
          single.row = T,
          se = list(nw_iv_bd1[,2], nw_iv_bd2[,2], nw_iv_bd3[,2], nw_iv_bd4[,2], nw_iv_bd5[,2]),
          label = 'implied_vol_model',
          dep.var.labels = '$\\mathbf{Implied volatility_t}$',
          omit.stat = c('f', 'ser'),
          # covariate.labels = c('$\\mathbf{BadNews_{t-1}}$', '$\\mathbf{BadNews_{t-2}}$',
          #                      '$\\mathbf{BadNews_{t-3}}$', '$\\mathbf{BadNews_{t-4}}$',
          #                      '$\\mathbf{BadNews_{t-5}}$', '$\\mathbf{Ivol_{t-1}}$',
          #                      '$\\mathbf{Ivol_{t-2}}$','$\\mathbf{Ivol_{t-3}}$',
          #                      '$\\mathbf{Ivol_{t-4}}$','$\\mathbf{Ivol_{t-5}}$'),
          add.lines = list(
            c('Return', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Log(Volume)', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Volatility', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Weekday effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Month effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
            c('Industry', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')))
          # out = '../thesis/tables/blob_pred_iv_ivse.text')



# ==== Plots ====
ggplot(main_df, aes(polarity, ret)) + 
  geom_point(color = 'black', fill = 'lightblue', alpha = 0.3) +
  theme_stata(scheme = 's1color')


main_df %>% #filter(se_neg != 0) %>% 
  group_by(
    months = month(date),
    month = months(date),
    year = year(date),
  ) %>% add_tally() %>% group_by(months, month, year, n) %>% nest() %>% 
  mutate(
    ngg = map_dbl(data, ~sum(if_else(.$polarity==0,0,1))),
    mean = map_dbl(data, ~mean(.$polarity, na.rm = T)),
    sd = map_dbl(data, ~sd(.$polarity, na.rm = T)),
    min = map_dbl(data, ~min(.$polarity, na.rm = T)),
    max = map_dbl(data, ~max(.$polarity, na.rm = T))) %>% ungroup() %>% 
  arrange(year, months) %>% select(-months, -data) %>% 
  select(month, year, n, ngg, mean, sd, min, max) %>% 
  rename(Month = month, Year = year, N = n, Mean = mean, `Std.dev.` = sd, Min = min, Max = max) %>% 
  knitr::kable(format = 'latex',
               digits = 3,
               booktabs=T, caption = '') %>% 
  kableExtra::kable_styling(position = 'center', full_width = T) %>% 
  cat(., file = '../thesis/tables/sentiment_descriptives.tex')



