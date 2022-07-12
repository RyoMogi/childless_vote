# read the data: all rounds of ESS ---------------------------------------------

# read ESS data from their website. Now essurvey package does not work due to the
# changes of the ESS website.

ESS1 <- ESS1 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rship2, rship3, rship4, rship5, rship6,
         rship7, rship8, rship9, rship10, rship11,
         rship12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvla, eisced, essround) %>%
  rename(rshipa2 = rship2, rshipa3 = rship3, rshipa4 = rship4, rshipa5 = rship5,
         rshipa6 = rship6, rshipa7 = rship7, rshipa8 = rship8, rshipa9 = rship9,
         rshipa10 = rship10, rshipa11 = rship11, rshipa12 = rship12, edulvlc = edulvla) %>% 
  mutate(bthcld = NA)

ESS2 <- ESS2 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvla, eisced, essround) %>% 
  rename(edulvlc = edulvla)

ESS3 <- ESS3 %>%
  select(agea, vote, chldhm, chldhhe, bthcld, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvla, eisced, essround) %>% 
  rename(edulvlc = edulvla)

ESS4 <- ESS4 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvla, eisced, essround) %>% 
  rename(edulvlc = edulvla)

ESS5 <- ESS5 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvlb, eisced, essround) %>% 
  rename(edulvlc = edulvlb)

ESS6 <- ESS6 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvlb, eisced, essround) %>% 
  rename(edulvlc = edulvlb)

ESS7 <- ESS7 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvlb, eisced, essround) %>% 
  rename(edulvlc = edulvlb)

ESS8 <- ESS8 %>%
  select(agea, vote, chldhm, chldhhe, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvlb, eisced, essround) %>% 
  rename(edulvlc = edulvlb)

ESS9 <- ESS9 %>%
  select(agea, vote, chldhhe, bthcld, pspwght, pweight,
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6,
         rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, yrbrn2, yrbrn3, yrbrn4, yrbrn5, yrbrn6,
         yrbrn7, yrbrn8, yrbrn9, yrbrn10, yrbrn11, yrbrn12,
         cntry, gndr, yrbrn, edulvlb, eisced, essround) %>% 
  rename(edulvlc = edulvlb)

# Combine all rounds -----------------------------------------------------------
ESS_all <- bind_rows(ESS1, ESS2) %>%
  bind_rows(ESS3) %>%
  bind_rows(ESS4) %>%
  bind_rows(ESS5) %>%
  bind_rows(ESS6) %>%
  bind_rows(ESS7) %>%
  bind_rows(ESS8) %>%
  bind_rows(ESS9) %>%
  mutate(agea = as.numeric(as.character(agea)))

save(ESS_all, file = "out/ESS_all.rda")
#write.dta(ESS_all, "out/ESS_all.dta")