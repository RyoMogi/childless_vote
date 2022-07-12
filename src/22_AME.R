# model 2
m2_AME_db <- read.csv("out/AME_age.csv", sep = ",", skip = 50, nrows = 30)
m2_AME_db <- m2_AME_db %>% 
  as.data.frame() %>% 
  mutate(Age = rep(1:15, each = 2),
         Age = case_when(Age == 1 ~ "<=24",
                         Age == 2 ~ "25-29",
                         Age == 3 ~ "30-34",
                         Age == 4 ~ "35-39",
                         Age == 5 ~ "40-44",
                         Age == 6 ~ "45-49",
                         Age == 7 ~ "50-54",
                         Age == 8 ~ "55-59",
                         Age == 9 ~ "60-64",
                         Age == 10 ~ "65-69",
                         Age == 11 ~ "70-74",
                         Age == 12 ~ "75-79",
                         Age == 13 ~ "80-84",
                         Age == 14 ~ "85-89",
                         Age == 15 ~ ">=90"),
         Age = factor(Age, levels = c("<=24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                      "80-84", "85-89", ">=90")),
         Cate = rep(c("AME", "SD"), 15),
         Value = readr::parse_number(X.)) %>% 
  select(Age, Cate, Value) %>% 
  spread(key = Cate, value = Value) %>% 
  mutate(lowCI95 = AME - (1.96 * SD),
         highCI95 = AME + (1.96 * SD),
         lowCI83 = AME - (1.39 * SD),
         highCI83 = AME + (1.39 * SD))

Fig_m2_AME_95 <- m2_AME_db %>% 
  ggplot(aes(x = Age, y = AME)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = lowCI95, ymax = highCI95), width = 0.1, position = position_dodge(0.1)) +
  geom_point() +
  ylim(-0.1, 0.1) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        axis.title.y = element_blank())

# model 3
m3_AME_db <- read.csv("out/AME_gndr.csv", sep = ",", skip = 11, nrows = 4)
m3_AME_db <- m3_AME_db %>% 
  as.data.frame() %>% 
  mutate(Sex = c("Female", "Female", "Male", "Male"),
         Cate = rep(c("AME", "SD"), 2),
         Value = readr::parse_number(X.)) %>% 
  select(Sex, Cate, Value) %>% 
  spread(key = Cate, value = Value) %>% 
  mutate(lowCI95 = AME - (1.96 * SD),
         highCI95 = AME + (1.96 * SD),
         lowCI83 = AME - (1.39 * SD),
         highCI83 = AME + (1.39 * SD))

Fig_m3_AME_95 <- m3_AME_db %>% 
  ggplot(aes(x = Sex, y = AME)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = lowCI95, ymax = highCI95), width = 0.1, position = position_dodge(0.1)) +
  geom_point() +
  ylim(-0.1, 0.1) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        axis.title.y = element_blank())

# model 4
m4_AME_db <- read.csv("out/AME_edu.csv", sep = ",", skip = 17, nrows = 8)
m4_AME_db <- m4_AME_db %>% 
  as.data.frame() %>% 
  mutate(Edu = rep(1:4, each = 2),
         Edu = case_when(Edu == 1 ~ "Less than low",
                         Edu == 2 ~ "Low",
                         Edu == 3 ~ "Secondary",
                         Edu == 4 ~ "Tertiary"),
         Edu = factor(Edu, levels = c("Less than low", "Low", "Secondary", "Tertiary")),
         Cate = rep(c("AME", "SD"), 4),
         Value = readr::parse_number(X.)) %>% 
  select(Edu, Cate, Value) %>% 
  spread(key = Cate, value = Value) %>% 
  mutate(lowCI95 = AME - (1.96 * SD),
         highCI95 = AME + (1.96 * SD),
         lowCI83 = AME - (1.39 * SD),
         highCI83 = AME + (1.39 * SD))

Fig_m4_AME_95 <- m4_AME_db %>% 
  ggplot(aes(x = Edu, y = AME)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = lowCI95, ymax = highCI95), width = 0.1, position = position_dodge(0.1)) +
  geom_point() +
  ylim(-0.1, 0.1) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        axis.title.y = element_blank()) +
  labs(x = "Education")


Fig_m234_AME_95 <- ggarrange(Fig_m2_AME_95, Fig_m3_AME_95, Fig_m4_AME_95,
                             ncol = 1, nrow = 3)
Fig_m234_AME_95 <- annotate_figure(Fig_m234_AME_95, left = text_grob("Average Marginal Effect", rot = 90,
                                                                     size = 13))
ggsave("out/Fig_m234_AME_95.pdf", Fig_m234_AME_95, width = 8, height = 11)

# model 5
m5_AME_db <- read.csv("out/AME_country.csv", sep = ",", skip = 119, nrows = 76)
m5_AME_db <- m5_AME_db %>% 
  as.data.frame() %>% 
  mutate(Country = rep(1:38, each = 2),
         Country = case_when(Country == 1 ~ "Albania",
                             Country == 2 ~ "Austria", 
                             Country == 3 ~ "Belgium",
                             Country == 4 ~ "Bulgaria",
                             Country == 5 ~ "Switzerland",
                             Country == 6 ~ "Cyprus",
                             Country == 7 ~ "Czechia",
                             Country == 8 ~ "Germany",
                             Country == 9 ~ "Denmark",
                             Country == 10 ~ "Estonia",
                             Country == 11 ~ "Spain",
                             Country == 12 ~ "Finland",
                             Country == 13 ~ "France",
                             Country == 14 ~ "the UK",
                             Country == 15 ~ "Greece",
                             Country == 16 ~ "Croatia",
                             Country == 17 ~ "Hungary",
                             Country == 18 ~ "Ireland",
                             Country == 19 ~ "Israel",
                             Country == 20 ~ "Iceland",
                             Country == 21 ~ "Italy",
                             Country == 22 ~ "Lithuania",
                             Country == 23 ~ "Luxembourg",
                             Country == 24 ~ "Latvia",
                             Country == 25 ~ "Montenegro",
                             Country == 26 ~ "the Netherlands",
                             Country == 27 ~ "Norway",
                             Country == 28 ~ "Poland",
                             Country == 29 ~ "Portugal",
                             Country == 30 ~ "Romania",
                             Country == 31 ~ "Serbia",
                             Country == 32 ~ "Russia",
                             Country == 33 ~ "Sweden",
                             Country == 34 ~ "Slovenia",
                             Country == 35 ~ "Slovakia",
                             Country == 36 ~ "Turkey",
                             Country == 37 ~ "Ukraine",
                             Country == 38 ~ "Kosovo"),
         Cate = rep(c("AME", "SD"), 38),
         Value = readr::parse_number(X.)) %>% 
  select(Country, Cate, Value) %>% 
  spread(key = Cate, value = Value) %>% 
  mutate(lowCI95 = AME - (1.96 * SD),
         highCI95 = AME + (1.96 * SD),
         lowCI83 = AME - (1.39 * SD),
         highCI83 = AME + (1.39 * SD),
         Country_group1 = ifelse(Country %in% c("Albania", "Belarus", "Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary", "Kosovo", 
                                                "Latvia", "Lithuania", "Montenegro", "Poland", "Romania", "Russia", "Serbia", "Slovakia",
                                                "Slovenia", "Ukraine"), "Post-communist", "Established democracies"),
         Country_group2 = case_when(Country %in% c("Belarus", "Bulgaria", "Czechia", "Hungary", "Poland", "Romania", "Russia", "Slovakia", "Ukraine") ~ "East",
                                    Country %in% c("Denmark", "Estonia", "Iceland", "Ireland", "Latvia", "Lithuania", "Norway", "Sweden", "the UK") ~ "North",
                                    Country %in% c("Albania", "Croatia", "Greece", "Italy", "Montenegro", "Portugal", "Serbia", "Slovenia", "Spain") ~ "South",
                                    Country %in% c("Austria", "Belgium", "France", "Germany", "Luxembourg", "the Netherlands", "Switzerland") ~ "West",
                                    TRUE ~ "Others"))

level_country <- m5_AME_db %>% 
  arrange(AME)

Fig_m5_AME_95 <- m5_AME_db %>% 
  mutate(Country = factor(Country, levels = level_country$Country)) %>%
  ggplot(aes(x = AME, y = Country)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_errorbar(aes(xmin = lowCI95, xmax = highCI95), width = 0.1, position = position_dodge(0.1)) +
  geom_point() +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        axis.title.y = element_blank()) +
  labs(x = "Average Marginal Effect")
ggsave("out/Fig_m5_AME_95.pdf", Fig_m5_AME_95, width = 8, height = 7.5)
