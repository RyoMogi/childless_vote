load("out/ESS_all_analy.rda")

# Y = vote or not
# X = childless or not
# covariates:
# age, sex, education, country, essround

ESS_all_analy$essround <- as.factor(ESS_all_analy$essround)
ESS_all_analy$age_ele5 <- as.factor(ESS_all_analy$age_ele5)
ESS_all_analy$age_ele5 <- relevel(ESS_all_analy$age_ele5, ref = "30-34")
ESS_all_analy$edu_cate <- as.factor(ESS_all_analy$edu_cate)
ESS_all_analy$edu_cate <- relevel(ESS_all_analy$edu_cate, ref = "secondary")
ESS_all_analy$cntry <- as.factor(ESS_all_analy$cntry)
ESS_all_analy$cntry <- relevel(ESS_all_analy$cntry, ref = "AT")

# model 1
m1 <- glm(vote_dum ~ child0 + age_ele5 + gndr + cntry + essround + edu_cate, data = ESS_all_analy, family = binomial("logit"))

# model 2 (interaction x age)
m2 <- glm(vote_dum ~ child0 * age_ele5 + gndr + cntry + essround + edu_cate, data = ESS_all_analy, family = binomial("logit"))

# model 3 (interaction x sex)
m3 <- glm(vote_dum ~ child0 * gndr + age_ele5 + cntry + essround + edu_cate, data = ESS_all_analy, family = binomial("logit"))

# model 4 (interaction x edu)
m4 <- glm(vote_dum ~ child0 * edu_cate + age_ele5 + gndr + cntry + essround, data = ESS_all_analy, family = binomial("logit"))

# model 5 (interaction x country)
m5 <- glm(vote_dum ~ child0 * cntry + age_ele5 + gndr + essround + edu_cate, data = ESS_all_analy, family = binomial("logit"))

stargazer(m1, m2, m3, m4, 
          title = "", align = T, no.space = T, star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+p < 0.1; *p < 0.05; **p < 0.01; ***p < 0.001"), 
          notes.append = F)

# robust standard errors
# from this website: https://cimentadaj.github.io/blog/2016-09-19-obtaining-robust-standard-errors-and-odds-ratios/obtaining-robust-standard-errors-and-odds-ratios-for-logistic-regression-in-r/
robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
  suppressMessages(suppressWarnings(library(lmtest)))
  suppressMessages(suppressWarnings(library(sandwich)))
  
  sandwich1 <- function(object, ...) sandwich(object) *
    nobs(object) / (nobs(object) - 1)
  # Function calculates SE's
  mod1 <- coeftest(x, vcov = sandwich1) 
  # apply the function over the variance-covariance matrix
  
  if (coef == "logit") {
    return(mod1) # return logit with robust SE's
  } else if (coef == "odd.ratio") {
    mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
    mod1[, 2] <- mod1[, 1] * mod1[, 2]
    return(mod1)
  } else {
    mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
    mod1[, 2] <- mod1[, 2]/4
    return(mod1)
  }
}

robustse(m1, coef = "logit")
