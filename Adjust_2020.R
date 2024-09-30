#### Environment ####
library("tidyverse")
'%nin%' <- Negate("%in%")
library("ranger")

setwd("/Users/mac/Desktop/UCL\ Publish/Data")
getwd()
#####

#### Import ####
cfps_2020 <- readRDS(file = "cfps_2020_income.rds")

#####

#### Adjust ####
# rename
colnames(cfps_2020) <- c("edu", "hukou", "marr", 
                         "minzu", "gender", "age",
                         "prov", "income", "employer",
                         "indu", "lang", "comp",
                         "size", "iq")

# estimate prob from available data 
{
  comp_prob <- cfps_2020 %>% 
    filter(indu != "7") %>%
    filter(comp != "-8") %>%
    subset(select = comp) %>%
    unlist() %>%
    mean()
  
  lang_prob <- cfps_2020 %>%
    filter(lang != "-8") %>%
    subset(select = lang) %>%
    unlist() %>%
    mean()
} 

# clean -8 
{
  cfps_2020 <- cfps_2020 %>%
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
  cfps_2020$comp[is.na(cfps_2020$comp)] <- rbinom(
    n = sum(is.na(cfps_2020$comp)), 
    prob = comp_prob, 
    size = 1
  )
  
  cfps_2020$lang[is.na(cfps_2020$lang)] <- rbinom(
    n = sum(is.na(cfps_2020$lang)), 
    prob = lang_prob, 
    size = 1
  )
}

mean(cfps_2020$lang)
mean(cfps_2020$comp)

saveRDS(cfps_2020, file = "cfps_2020.rds")
#####