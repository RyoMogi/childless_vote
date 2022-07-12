load("out/ESS_all.rda")

ESS_all <- ESS_all %>% 
  mutate(vote_dum = case_when(vote == 1 ~ 1,
                              vote == 2 ~ 0),
         chldhhe = ifelse(is.na(chldhhe), 9, chldhhe)) %>% 
  mutate_at(c("yrbrn2", "yrbrn3", "yrbrn4", "yrbrn5", "yrbrn6", "yrbrn7", "yrbrn8",
              "yrbrn9", "yrbrn10", "yrbrn11", "yrbrn12"),
            funs(as.numeric(as.character(.)))) %>% 
  mutate_at(c("yrbrn2", "yrbrn3", "yrbrn4", "yrbrn5", "yrbrn6", "yrbrn7", "yrbrn8",
              "yrbrn9", "yrbrn10", "yrbrn11", "yrbrn12"),
            funs(ifelse(is.na(.), 9999, .))) %>% 
  mutate_at(c("rshipa2", "rshipa3", "rshipa4", "rshipa5", "rshipa6", "rshipa7", "rshipa8",
              "rshipa9", "rshipa10", "rshipa11", "rshipa12"),
            funs(ifelse(is.na(.), 99, .)))

# birth year of the oldest son
ESS_all$by_oldkids[ESS_all$rshipa2 == 2] <- ESS_all$yrbrn2[ESS_all$rshipa2 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 == 2] <- ESS_all$yrbrn3[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 == 2] <- ESS_all$yrbrn4[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 == 2] <- ESS_all$yrbrn5[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 != 2 &
                                                               ESS_all$rshipa5 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 == 2] <- ESS_all$yrbrn6[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 != 2 &
                                                               ESS_all$rshipa5 != 2 &
                                                               ESS_all$rshipa6 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 == 2] <- ESS_all$yrbrn7[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 != 2 &
                                                               ESS_all$rshipa5 != 2 &
                                                               ESS_all$rshipa6 != 2 &
                                                               ESS_all$rshipa7 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 == 2] <- ESS_all$yrbrn8[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 != 2 &
                                                               ESS_all$rshipa5 != 2 &
                                                               ESS_all$rshipa6 != 2 &
                                                               ESS_all$rshipa7 != 2 &
                                                               ESS_all$rshipa8 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 != 2 &
                     ESS_all$rshipa9 == 2] <- ESS_all$yrbrn9[ESS_all$rshipa2 != 2 &
                                                               ESS_all$rshipa3 != 2 &
                                                               ESS_all$rshipa4 != 2 &
                                                               ESS_all$rshipa5 != 2 &
                                                               ESS_all$rshipa6 != 2 &
                                                               ESS_all$rshipa7 != 2 &
                                                               ESS_all$rshipa8 != 2 &
                                                               ESS_all$rshipa9 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 != 2 &
                     ESS_all$rshipa9 != 2 &
                     ESS_all$rshipa10 == 2] <- ESS_all$yrbrn10[ESS_all$rshipa2 != 2 &
                                                                 ESS_all$rshipa3 != 2 &
                                                                 ESS_all$rshipa4 != 2 &
                                                                 ESS_all$rshipa5 != 2 &
                                                                 ESS_all$rshipa6 != 2 &
                                                                 ESS_all$rshipa7 != 2 &
                                                                 ESS_all$rshipa8 != 2 &
                                                                 ESS_all$rshipa9 != 2 &
                                                                 ESS_all$rshipa10 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 != 2 &
                     ESS_all$rshipa9 != 2 &
                     ESS_all$rshipa10 != 2 &
                     ESS_all$rshipa11 == 2] <- ESS_all$yrbrn11[ESS_all$rshipa2 != 2 &
                                                                 ESS_all$rshipa3 != 2 &
                                                                 ESS_all$rshipa4 != 2 &
                                                                 ESS_all$rshipa5 != 2 &
                                                                 ESS_all$rshipa6 != 2 &
                                                                 ESS_all$rshipa7 != 2 &
                                                                 ESS_all$rshipa8 != 2 &
                                                                 ESS_all$rshipa9 != 2 &
                                                                 ESS_all$rshipa10 != 2 &
                                                                 ESS_all$rshipa11 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 != 2 &
                     ESS_all$rshipa9 != 2 &
                     ESS_all$rshipa10 != 2 &
                     ESS_all$rshipa11 != 2 &
                     ESS_all$rshipa12 == 2] <- ESS_all$yrbrn12[ESS_all$rshipa2 != 2 &
                                                                 ESS_all$rshipa3 != 2 &
                                                                 ESS_all$rshipa4 != 2 &
                                                                 ESS_all$rshipa5 != 2 &
                                                                 ESS_all$rshipa6 != 2 &
                                                                 ESS_all$rshipa7 != 2 &
                                                                 ESS_all$rshipa8 != 2 &
                                                                 ESS_all$rshipa9 != 2 &
                                                                 ESS_all$rshipa10 != 2 &
                                                                 ESS_all$rshipa11 != 2 &
                                                                 ESS_all$rshipa12 == 2]
ESS_all$by_oldkids[ESS_all$rshipa2 != 2 &
                     ESS_all$rshipa3 != 2 &
                     ESS_all$rshipa4 != 2 &
                     ESS_all$rshipa5 != 2 &
                     ESS_all$rshipa6 != 2 &
                     ESS_all$rshipa7 != 2 &
                     ESS_all$rshipa8 != 2 &
                     ESS_all$rshipa9 != 2 &
                     ESS_all$rshipa10 != 2 &
                     ESS_all$rshipa11 != 2 &
                     ESS_all$rshipa12 != 2] <- 0

ESS_all$by_oldkids <- ifelse(ESS_all$by_oldkids == 9999, 0, ESS_all$by_oldkids)

# last election year
D_ESS_all <- ESS_all %>% 
  mutate(y_ele = case_when(cntry == "AT" & essround == 9 ~ 2017,
                           cntry == "BE" & essround == 9 ~ 2014,
                           cntry == "BG" & essround == 9 ~ 2017,
                           cntry == "HR" & essround == 9 ~ 2016,
                           cntry == "CY" & essround == 9 ~ 2016,
                           cntry == "CZ" & essround == 9 ~ 2017,
                           cntry == "DK" & essround == 9 ~ 2015,
                           cntry == "EE" & essround == 9 ~ 2015,
                           cntry == "FI" & essround == 9 ~ 2015,
                           cntry == "FR" & essround == 9 ~ 2017,
                           cntry == "DE" & essround == 9 ~ 2017,
                           cntry == "HU" & essround == 9 ~ 2018,
                           cntry == "IS" & essround == 9 ~ 2017,
                           cntry == "IE" & essround == 9 ~ 2016,
                           cntry == "IT" & essround == 9 ~ 2018,
                           cntry == "LT" & essround == 9 ~ 2016,
                           cntry == "NL" & essround == 9 ~ 2017,
                           cntry == "NO" & essround == 9 ~ 2017,
                           cntry == "PL" & essround == 9 ~ 2015,
                           cntry == "PT" & essround == 9 ~ 2015,
                           cntry == "SK" & essround == 9 ~ 2016,
                           cntry == "SI" & essround == 9 ~ 2018,
                           cntry == "ES" & essround == 9 ~ 2019,
                           cntry == "SE" & essround == 9 ~ 2018,
                           cntry == "CH" & essround == 9 ~ 2019,
                           cntry == "GB" & essround == 9 ~ 2017,
                           cntry == "LV" & essround == 9 ~ 2018,
                           cntry == "ME" & essround == 9 ~ 2016,
                           cntry == "RS" & essround == 9 ~ 2016,
                           cntry == "AT" & essround == 8 ~ 2013,
                           cntry == "BE" & essround == 8 ~ 2014,
                           cntry == "CH" & essround == 8 ~ 2015,
                           cntry == "CZ" & essround == 8 ~ 2013,
                           cntry == "DE" & essround == 8 ~ 2013,
                           cntry == "EE" & essround == 8 ~ 2015,
                           cntry == "ES" & essround == 8 ~ 2016,
                           cntry == "FI" & essround == 8 ~ 2015,
                           cntry == "FR" & essround == 8 ~ 2012,
                           cntry == "GB" & essround == 8 ~ 2015,
                           cntry == "HU" & essround == 8 ~ 2014,
                           cntry == "IE" & essround == 8 ~ 2016,
                           cntry == "IL" & essround == 8 ~ 2015,
                           cntry == "IS" & essround == 8 ~ 2013,
                           cntry == "IT" & essround == 8 ~ 2013,
                           cntry == "LT" & essround == 8 ~ 2016,
                           cntry == "NL" & essround == 8 ~ 2012,
                           cntry == "NO" & essround == 8 ~ 2013,
                           cntry == "PL" & essround == 8 ~ 2015,
                           cntry == "PT" & essround == 8 ~ 2015,
                           cntry == "RU" & essround == 8 ~ 2016,
                           cntry == "SE" & essround == 8 ~ 2014,
                           cntry == "SI" & essround == 8 ~ 2014,
                           cntry == "AT" & essround == 7 ~ 2013,
                           cntry == "BE" & essround == 7 ~ 2014,
                           cntry == "CH" & essround == 7 ~ 2011,
                           cntry == "CZ" & essround == 7 ~ 2013,
                           cntry == "DE" & essround == 7 ~ 2013,
                           cntry == "DK" & essround == 7 ~ 2011,
                           cntry == "EE" & essround == 7 ~ 2015,
                           cntry == "ES" & essround == 7 ~ 2011,
                           cntry == "FI" & essround == 7 ~ 2011,
                           cntry == "FR" & essround == 7 ~ 2012,
                           cntry == "GB" & essround == 7 ~ 2015,
                           cntry == "HU" & essround == 7 ~ 2014,
                           cntry == "IE" & essround == 7 ~ 2011,
                           cntry == "IL" & essround == 7 ~ 2015,
                           cntry == "LT" & essround == 7 ~ 2012,
                           cntry == "NL" & essround == 7 ~ 2012,
                           cntry == "NO" & essround == 7 ~ 2013,
                           cntry == "PL" & essround == 7 ~ 2011,
                           cntry == "PT" & essround == 7 ~ 2015,
                           cntry == "SE" & essround == 7 ~ 2014,
                           cntry == "SI" & essround == 7 ~ 2014,
                           cntry == "AL" & essround == 6 ~ 2009,
                           cntry == "BE" & essround == 6 ~ 2010,
                           cntry == "BG" & essround == 6 ~ 2011,
                           cntry == "CH" & essround == 6 ~ 2011,
                           cntry == "CY" & essround == 6 ~ 2011,
                           cntry == "CZ" & essround == 6 ~ 2010,
                           cntry == "DE" & essround == 6 ~ 2009,
                           cntry == "DK" & essround == 6 ~ 2011,
                           cntry == "EE" & essround == 6 ~ 2011,
                           cntry == "ES" & essround == 6 ~ 2011,
                           cntry == "FI" & essround == 6 ~ 2011,
                           cntry == "FR" & essround == 6 ~ 2012,
                           cntry == "GB" & essround == 6 ~ 2010,
                           cntry == "HU" & essround == 6 ~ 2010,
                           cntry == "IE" & essround == 6 ~ 2011,
                           cntry == "IL" & essround == 6 ~ 2009,
                           cntry == "IS" & essround == 6 ~ 2009,
                           cntry == "IT" & essround == 6 ~ 2013,
                           cntry == "LT" & essround == 6 ~ 2012,
                           cntry == "NL" & essround == 6 ~ 2012,
                           cntry == "NO" & essround == 6 ~ 2009,
                           cntry == "PL" & essround == 6 ~ 2011,
                           cntry == "PT" & essround == 6 ~ 2011,
                           cntry == "RU" & essround == 6 ~ 2011,
                           cntry == "SE" & essround == 6 ~ 2010,
                           cntry == "SI" & essround == 6 ~ 2011,
                           cntry == "SK" & essround == 6 ~ 2012,
                           cntry == "UA" & essround == 6 ~ 2012,
                           cntry == "XK" & essround == 6 ~ 2010,
                           cntry == "AT" & essround == 5 ~ 2008,
                           cntry == "BE" & essround == 5 ~ 2010,
                           cntry == "BG" & essround == 5 ~ 2009,
                           cntry == "CH" & essround == 5 ~ 2007,
                           cntry == "CY" & essround == 5 ~ 2011,
                           cntry == "CZ" & essround == 5 ~ 2010,
                           cntry == "DE" & essround == 5 ~ 2009,
                           cntry == "DK" & essround == 5 ~ 2007,
                           cntry == "EE" & essround == 5 ~ 2011,
                           cntry == "ES" & essround == 5 ~ 2008,
                           cntry == "FI" & essround == 5 ~ 2007,
                           cntry == "FR" & essround == 5 ~ 2007,
                           cntry == "GB" & essround == 5 ~ 2010,
                           cntry == "GR" & essround == 5 ~ 2009,
                           cntry == "HR" & essround == 5 ~ 2007,
                           cntry == "HU" & essround == 5 ~ 2010,
                           cntry == "IE" & essround == 5 ~ 2011,
                           cntry == "IL" & essround == 5 ~ 2009,
                           cntry == "LT" & essround == 5 ~ 2008,
                           cntry == "NL" & essround == 5 ~ 2010,
                           cntry == "NO" & essround == 5 ~ 2009,
                           cntry == "PL" & essround == 5 ~ 2007,
                           cntry == "PT" & essround == 5 ~ 2009,
                           cntry == "RU" & essround == 5 ~ 2007,
                           cntry == "SE" & essround == 5 ~ 2010,
                           cntry == "SI" & essround == 5 ~ 2008,
                           cntry == "SK" & essround == 5 ~ 2010,
                           cntry == "UA" & essround == 5 ~ 2007,
                           cntry == "AT" & essround == 4 ~ 2008,
                           cntry == "BE" & essround == 4 ~ 2007,
                           cntry == "BG" & essround == 4 ~ 2005,
                           cntry == "CH" & essround == 4 ~ 2007,
                           cntry == "CY" & essround == 4 ~ 2006,
                           cntry == "CZ" & essround == 4 ~ 2006,
                           cntry == "DE" & essround == 4 ~ 2005,
                           cntry == "DK" & essround == 4 ~ 2007,
                           cntry == "EE" & essround == 4 ~ 2007,
                           cntry == "ES" & essround == 4 ~ 2008,
                           cntry == "FI" & essround == 4 ~ 2007,
                           cntry == "FR" & essround == 4 ~ 2007,
                           cntry == "GB" & essround == 4 ~ 2005,
                           cntry == "GR" & essround == 4 ~ 2007,
                           cntry == "HR" & essround == 4 ~ 2007,
                           cntry == "HU" & essround == 4 ~ 2006,
                           cntry == "IE" & essround == 4 ~ 2008,
                           cntry == "IL" & essround == 4 ~ 2006,
                           cntry == "LT" & essround == 4 ~ 2008,
                           cntry == "LV" & essround == 4 ~ 2006,
                           cntry == "NL" & essround == 4 ~ 2006,
                           cntry == "NO" & essround == 4 ~ 2005,
                           cntry == "PL" & essround == 4 ~ 2007,
                           cntry == "PT" & essround == 4 ~ 2005,
                           cntry == "RO" & essround == 4 ~ 2008,
                           cntry == "RU" & essround == 4 ~ 2007,
                           cntry == "SE" & essround == 4 ~ 2006,
                           cntry == "SI" & essround == 4 ~ 2008,
                           cntry == "SK" & essround == 4 ~ 2006,
                           cntry == "TR" & essround == 4 ~ 2007,
                           cntry == "UA" & essround == 4 ~ 2007,
                           cntry == "AT" & essround == 3 ~ 2006,
                           cntry == "BE" & essround == 3 ~ 2003,
                           cntry == "BG" & essround == 3 ~ 2005,
                           cntry == "CH" & essround == 3 ~ 2003,
                           cntry == "CY" & essround == 3 ~ 2006,
                           cntry == "DE" & essround == 3 ~ 2005,
                           cntry == "DK" & essround == 3 ~ 2005,
                           cntry == "EE" & essround == 3 ~ 2003,
                           cntry == "ES" & essround == 3 ~ 2004,
                           cntry == "FI" & essround == 3 ~ 2003,
                           cntry == "FR" & essround == 3 ~ 2002,
                           cntry == "GB" & essround == 3 ~ 2005,
                           cntry == "HU" & essround == 3 ~ 2006,
                           cntry == "IE" & essround == 3 ~ 2002,
                           cntry == "NL" & essround == 3 ~ 2003,
                           cntry == "NO" & essround == 3 ~ 2005,
                           cntry == "PL" & essround == 3 ~ 2005,
                           cntry == "PT" & essround == 3 ~ 2005,
                           cntry == "RU" & essround == 3 ~ 2003,
                           cntry == "SE" & essround == 3 ~ 2006,
                           cntry == "SI" & essround == 3 ~ 2004,
                           cntry == "SK" & essround == 3 ~ 2006,
                           cntry == "UA" & essround == 3 ~ 2006,
                           cntry == "AT" & essround == 2 ~ 2002,
                           cntry == "BE" & essround == 2 ~ 2003,
                           cntry == "CH" & essround == 2 ~ 2003,
                           cntry == "CZ" & essround == 2 ~ 2002,
                           cntry == "DE" & essround == 2 ~ 2002,
                           cntry == "DK" & essround == 2 ~ 2001,
                           cntry == "EE" & essround == 2 ~ 2003,
                           cntry == "ES" & essround == 2 ~ 2004,
                           cntry == "FI" & essround == 2 ~ 2003,
                           cntry == "FR" & essround == 2 ~ 2002,
                           cntry == "GB" & essround == 2 ~ 2001,
                           cntry == "GR" & essround == 2 ~ 2004,
                           cntry == "HU" & essround == 2 ~ 2002,
                           cntry == "IE" & essround == 2 ~ 2002,
                           cntry == "IS" & essround == 2 ~ 2003,
                           cntry == "IT" & essround == 2 ~ 2001,
                           cntry == "LU" & essround == 2 ~ 2004,
                           cntry == "NL" & essround == 2 ~ 2003,
                           cntry == "NO" & essround == 2 ~ 2001,
                           cntry == "PL" & essround == 2 ~ 2001,
                           cntry == "PT" & essround == 2 ~ 2002,
                           cntry == "SE" & essround == 2 ~ 2002,
                           cntry == "SI" & essround == 2 ~ 2004,
                           cntry == "SK" & essround == 2 ~ 2002,
                           cntry == "TR" & essround == 2 ~ 2002,
                           cntry == "UA" & essround == 2 ~ 2002,
                           cntry == "AT" & essround == 1 ~ 2002,
                           cntry == "BE" & essround == 1 ~ 1999,
                           cntry == "CH" & essround == 1 ~ 1999,
                           cntry == "CZ" & essround == 1 ~ 2002,
                           cntry == "DE" & essround == 1 ~ 2002,
                           cntry == "DK" & essround == 1 ~ 2001,
                           cntry == "ES" & essround == 1 ~ 2000,
                           cntry == "FI" & essround == 1 ~ 1999,
                           cntry == "FR" & essround == 1 ~ 2002,
                           cntry == "GB" & essround == 1 ~ 2001,
                           cntry == "GR" & essround == 1 ~ 2000,
                           cntry == "HU" & essround == 1 ~ 2002,
                           cntry == "IE" & essround == 1 ~ 2002,
                           cntry == "IL" & essround == 1 ~ 1999,
                           cntry == "IT" & essround == 1 ~ 2001,
                           cntry == "LU" & essround == 1 ~ 1999,
                           cntry == "NL" & essround == 1 ~ 2002,
                           cntry == "NO" & essround == 1 ~ 2001,
                           cntry == "PL" & essround == 1 ~ 2001,
                           cntry == "PT" & essround == 1 ~ 2002,
                           cntry == "SE" & essround == 1 ~ 2002,
                           cntry == "SI" & essround == 1 ~ 2000),
         # the respondents gave birth the oldest child after the last election
         timing_kidvote = ifelse(by_oldkids >= y_ele, 1, 0),
         # childless respondent at the year of the last election (child0 == 1)
         NeverLived = case_when(essround == 9 & chldhhe == 2 ~ 1,
                                essround != 9 & (chldhm == 2 & chldhhe == 2) ~ 1,
                                TRUE ~ 0),
         child0 = ifelse(NeverLived == 1 | timing_kidvote == 1, 1, 0),
         # sex
         gndr = as.numeric(as.character(gndr)),
         gndr = ifelse(gndr == 2, 0, gndr),
         # age at the last election
         age_ele = y_ele - yrbrn,
         age_ele5 = case_when(age_ele <= 24 ~ "-24",
                              age_ele >= 25 & age_ele <= 29 ~ "25-29",
                              age_ele >= 30 & age_ele <= 34 ~ "30-34",
                              age_ele >= 35 & age_ele <= 39 ~ "35-39",
                              age_ele >= 40 & age_ele <= 44 ~ "40-44",
                              age_ele >= 45 & age_ele <= 49 ~ "45-49",
                              age_ele >= 50 & age_ele <= 54 ~ "50-54",
                              age_ele >= 55 & age_ele <= 59 ~ "55-59",
                              age_ele >= 60 & age_ele <= 64 ~ "60-64",
                              age_ele >= 65 & age_ele <= 69 ~ "65-69",
                              age_ele >= 70 & age_ele <= 74 ~ "70-74",
                              age_ele >= 75 & age_ele <= 79 ~ "75-79",
                              age_ele >= 80 & age_ele <= 84 ~ "80-84",
                              age_ele >= 85 & age_ele <= 89 ~ "85-89",
                              age_ele >= 90 ~ "90-"),
         # educational level
         edulvlc2 = case_when(edulvlc %in% c(0, 1, 113, 129) ~ 1,
                              edulvlc %in% c(2, 212, 213, 221, 222, 223, 229) ~ 2,
                              edulvlc %in% c(3, 311, 312, 313, 321, 322, 323) ~ 3,
                              edulvlc %in% c(4, 412, 413, 421, 422, 423) ~ 4,
                              edulvlc %in% c(5, 510, 520, 610, 620, 710, 720, 800) ~ 5,
                              edulvlc %in% c(55, 555) ~ 0),
         edu_cate = case_when(edulvlc2 == 0 ~ "uncategorised",
                              edulvlc2 == 1 ~ "less than low",
                              edulvlc2 == 2 ~ "low",
                              edulvlc2 %in% c(3, 4) ~ "secondary",
                              edulvlc2 == 5 ~ "tertiary"),
         nobiochild = ifelse(bthcld == 2, 1, 0)
  )


# select the dataset
D_ESS_all_sel <- D_ESS_all %>% 
  filter(vote_dum %in% c(0, 1),
         age_ele >= 18 & agea <= 99)

ESS_all_analy <- subset(D_ESS_all_sel, !is.na(child0) & !is.na(age_ele5) &
                          !is.na(cntry) & !is.na(gndr) & !is.na(edu_cate) & edu_cate %out% "uncategorised")

# Number of missing cases
nrow(D_ESS_all_sel) - nrow(ESS_all_analy)
(nrow(D_ESS_all_sel) - nrow(ESS_all_analy)) / nrow(D_ESS_all_sel)

save(ESS_all_analy, file = "out/ESS_all_analy.rda")
#write.dta(ESS_all_analy, "out/ESS_all_analy.dta")
