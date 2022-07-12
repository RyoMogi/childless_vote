# direct childlessness ---------------------------------------------------------
ESS_all_analy_39 <- ESS_all_analy %>% 
  filter(essround %in% c(3, 9)) %>% 
  mutate(livechild = ifelse(child0 == 0, 1, 0),
         biochild = ifelse(nobiochild == 0, 1, 0),
         live_biochild = ifelse(livechild == 1 | biochild == 1, 1, 0),
         nolive_nobiochild = ifelse(live_biochild == 1, 0, 1))

ESS_all_analy_39 <- subset(ESS_all_analy_39, !is.na(child0) & !is.na(nobiochild) & !is.na(age_ele5) &
                             !is.na(cntry) & !is.na(gndr) & !is.na(edu_cate) & edu_cate %out% "uncategorised")

table(ESS_all_analy_39$livechild, ESS_all_analy_39$biochild, useNA = "ifany")

#table(ESS_all_analy_39$live_biochild, useNA = "always")
#table(ESS_all_analy_39$livechild, useNA = "always")
#table(ESS_all_analy_39$biochild, useNA = "always")
#
#table(ESS_all_analy$child0, ESS_all_analy$vote_dum)
#table(ESS_all_analy_39$nolive_nobiochild, ESS_all_analy_39$vote_dum)

ESS_all_analy_39$essround <- as.factor(ESS_all_analy_39$essround)
ESS_all_analy_39$age_ele5 <- as.factor(ESS_all_analy_39$age_ele5)
ESS_all_analy_39$age_ele5 <- relevel(ESS_all_analy_39$age_ele5, ref = "30-34")
ESS_all_analy_39$edu_cate <- as.factor(ESS_all_analy_39$edu_cate)
ESS_all_analy_39$edu_cate <- relevel(ESS_all_analy_39$edu_cate, ref = "secondary")

# model 1
m1_sensi_1 <- glm(vote_dum ~ nolive_nobiochild + age_ele5 + gndr + cntry + essround + edu_cate, data = ESS_all_analy_39, family = binomial("logit"))
m2_sensi_1 <- glm(vote_dum ~ child0 + age_ele5 + gndr + cntry + essround + edu_cate, data = ESS_all_analy_39, family = binomial("logit"))

m1_sensi_1rse <- robustse(m1_sensi_1, coef = "logit")
m2_sensi_1rse <- robustse(m2_sensi_1, coef = "logit")
stargazer(m1_sensi_1rse, m2_sensi_1rse,
          title = "", align = T, no.space = T, star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+p < 0.1; *p < 0.05; **p < 0.01; ***p < 0.001"), 
          notes.append = F)

# remove some countries --------------------------------------------------------
Sensi <- ESS_all_analy %>% 
  filter(cntry %out% c("AL", "CY", "IL", "XK", "ME", "RU", "TR"))

# model 1
m1_sensi_2 <- glm(vote_dum ~ child0 + age_ele5 + gndr + cntry + essround + edu_cate, data = Sensi, family = binomial("logit"))
robustse(m1_sensi_2, coef = "logit")
