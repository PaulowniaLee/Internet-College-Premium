#### Environment ####
library("tidyverse")
'%nin%' <- Negate("%in%")
library("ranger")

setwd("/Users/mac/Desktop/UCL\ Publish/Data")
getwd()
#####

#### Import ####
cfps_2016 <- readRDS(file = "cfps_2016_income.rds")

#####

#### Adjust ####
cfps_2016 <- cfps_2016 %>%
  mutate(income = case_when(
    INCOME == -8 ~ incomeb_imp,
    TRUE ~ INCOME
  )) %>%
  subset(select = -c(INCOME, incomeb_imp))

# rename
colnames(cfps_2016) <- c("edu", "hukou", "marr", 
                         "minzu", "gender", "age",
                         "prov", "employer",
                         "indu", "lang", "comp",
                         "size", "iq", "income")

# estimate prob from available data 
{
  comp_prob <- cfps_2016 %>% 
    filter(indu != "7") %>%
    filter(comp != "-8") %>%
    subset(select = comp) %>%
    unlist() %>%
    mean()
  
  lang_prob <- cfps_2016 %>%
    filter(lang != "-8") %>%
    subset(select = lang) %>%
    unlist() %>%
    mean()
} 

# clean -8 
{
  cfps_2016 <- cfps_2016 %>%
    mutate(comp = case_when(
      (indu == "7" & comp == "-8") ~ 1,
      TRUE ~ comp
    )) %>% #替换indu里的-8comp
    mutate(comp = case_when(
      comp == "-8" ~ NA,
      TRUE ~ comp
    )) %>% 
    mutate (lang = case_when(
      lang == "-8" ~ NA,
      TRUE ~ lang
    )) %>%
    mutate(income = case_when(
      income == -8 ~0,
      TRUE ~ income
    ))
}

# Replace NA with estimated probability 
{
  cfps_2016$comp[is.na(cfps_2016$comp)] <- rbinom(
    n = sum(is.na(cfps_2016$comp)), 
    prob = comp_prob, 
    size = 1
  )
  
  cfps_2016$lang[is.na(cfps_2016$lang)] <- rbinom(
    n = sum(is.na(cfps_2016$lang)), 
    prob = lang_prob, 
    size = 1
  )
}

saveRDS(cfps_2016, file = "cfps_2016.rds")
#####