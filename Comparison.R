# descriptive statistics 

# set up 
{
Inter_mean <- matrix(, nrow = 4, ncol = 6)
rownames(Inter_mean) <- c("2014", "2016", "2018", "2020")
colnames(Inter_mean) <- c("income", "edu", "gender", "comp", "lang", "prov")

cfps_mean <- matrix(, nrow = 4, ncol = 6)
rownames(cfps_mean) <- c("2014", "2016", "2018", "2020")
colnames(cfps_mean) <- c("income", "edu", "gender", "comp", "lang", "prov")
}

#### 2014 ####
# Internet Firm Data set
{
  Internet_2014 <- cfps_2014 %>%
    filter(indu == "7" & comp == "1" & edu %nin% c("1", "2")) %>%
    subset(select = income)
  
  Internet_2014 <- data_2014 %>%
    filter(indu == 1) %>%
    subset(select = -income) %>%
    cbind(Internet_2014)
  
  compare <- filter(cfps_2014, 
          (indu == "7" & comp == "1" & edu %nin% c("1", "2")))
  setequal(compare$income, Internet_2014$income)
  
  remove(compare)
}

# age 看不出来什么

# income 确实收入更高
Inter_mean["2014", "income"] <- mean(Internet_2014$income)
quantile(Internet_2014$income)

cfps_mean["2014", "income"] <- mean(cfps_2014$income)
quantile(cfps_2014$income)


# Graph 4
# 方案二：
{
income_vec <- Internet_2014$income
Distr1_df <- data.frame(income_vec)
income_vec <- cfps_2014$income
Distr2_df <- data.frame(income_vec)
remove(income_vec)
}

{
ggplot() +
  geom_density(data = Distr1_df,
               aes(x = income_vec, 
                   fill = "Inter"),
               alpha = 0.7,
               colour = "black") +
  scale_x_continuous(name = "Income",
                     labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) + #使坐标轴为正常数字
  labs(y = NULL) + #y轴不要标签
  geom_vline(aes(xintercept = 
                   mean(Distr1_df$income_vec),
                 colour = "Inter"),
             linetype = 2) +
  geom_density(data = Distr2_df,
               aes(x = income_vec,
                   fill = "Total"),
               alpha = 0.7,
               colour = "black",
               adjust = 2) + #第二个density
  geom_vline(aes(xintercept = 
                   mean(Distr2_df$income_vec),
                 colour = "Total"),
             linetype = 2) + #第二个mean line
    scale_colour_manual(values = c(
      "Inter"="#CC79A7",
      "Total"="#56B4E9"),
      aesthetics = c("fill")) +
    scale_colour_manual(values = c(
      "Inter" = "#D55E00",
      "Total" = "#0072B2"),
      aesthetics = c("colour")) + #按color和fill的map生成图例
  guides(fill = guide_legend(
        title = "Distrubtion")) + 
  guides(colour = guide_legend(
        title = "Mean")) + #给图例改名
  ggtitle("Graph 1: Internet Industry and Total Sample, 2014")
    
}

remove(Distr1_df, Distr2_df)


# edu
Inter_mean["2014", "edu"] <- mean(Internet_2014$edu)
cfps_mean["2014", "edu"] <- mean(data_2014$edu) # higher percentage for college above

# gender 差距不大
Inter_mean["2014", "gender"] <- mean(Internet_2014$gender)
cfps_mean["2014", "gender"] <- mean(data_2014$gender)

# comp 
Inter_mean["2014", "comp"] <- 1 #显著性地用comp更多
cfps_mean["2014", "comp"] <- mean(data_2014$comp)

# lang
Inter_mean["2014", "lang"] <- mean(Internet_2014$lang) #显著性地用lang更多
cfps_mean["2014", "lang"] <- mean(data_2014$lang)

# prov 差别不大
Inter_mean["2014", "prov"] <- mean(Internet_2014$prov) 
cfps_mean["2014", "prov"] <- mean(data_2014$prov)



#####

#### 2016 ####
# Internet Firm Data set
{
  Internet_2016 <- cfps_2016 %>%
    filter(indu == "7" & comp == "1" & edu %nin% c("1", "2")) %>%
    subset(select = income)
  
  Internet_2016 <- data_2016 %>%
    filter(indu == 1) %>%
    subset(select = -income) %>%
    cbind(Internet_2016)
  
  compare <- filter(cfps_2016, 
                    (indu == "7" & comp == "1" & edu %nin% c("1", "2")))
  setequal(compare$income, Internet_2016$income)
  
  remove(compare)
}

# age 看不出来什么

# income 确实收入更高
Inter_mean["2016", "income"] <- mean(Internet_2016$income)
quantile(Internet_2016$income)

cfps_mean["2016", "income"] <- mean(cfps_2016$income)
quantile(cfps_2016$income)


# Graph 4
# 方案二：
{
  income_vec <- Internet_2016$income
  Distr1_df <- data.frame(income_vec)
  income_vec <- cfps_2016$income
  Distr2_df <- data.frame(income_vec)
  remove(income_vec)
}

{
  ggplot() +
    geom_density(data = Distr1_df,
                 aes(x = income_vec, 
                     fill = "Inter"),
                 alpha = 0.7,
                 colour = "black") +
    scale_x_continuous(name = "Income",
                       labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) + #使坐标轴为正常数字
    labs(y = NULL) + #y轴不要标签
    geom_vline(aes(xintercept = 
                     mean(Distr1_df$income_vec),
                   colour = "Inter"),
               linetype = 2) +
    geom_density(data = Distr2_df,
                 aes(x = income_vec,
                     fill = "Total"),
                 alpha = 0.7,
                 colour = "black",
                 adjust = 2) + #第二个density
    geom_vline(aes(xintercept = 
                     mean(Distr2_df$income_vec),
                   colour = "Total"),
               linetype = 2) + #第二个mean line
    scale_colour_manual(values = c(
      "Inter"="#CC79A7",
      "Total"="#56B4E9"),
      aesthetics = c("fill")) +
    scale_colour_manual(values = c(
      "Inter" = "#D55E00",
      "Total" = "#0072B2"),
      aesthetics = c("colour")) + #按color和fill的map生成图例
    guides(fill = guide_legend(
      title = "Distrubtion")) + 
    guides(colour = guide_legend(
      title = "Mean")) + #给图例改名
    ggtitle("Graph 2: Internet Industry and Total Sample, 2016")
  
}

remove(Distr1_df, Distr2_df)


# edu
Inter_mean["2016", "edu"] <- mean(Internet_2016$edu)
cfps_mean["2016", "edu"] <- mean(data_2016$edu) # higher percentage for college above

# gender 差距不大
Inter_mean["2016", "gender"] <- mean(Internet_2016$gender)
cfps_mean["2016", "gender"] <- mean(data_2016$gender)

# comp 
Inter_mean["2016", "comp"] <- 1 #显著性地用comp更多
cfps_mean["2016", "comp"] <- mean(data_2016$comp)

# lang
Inter_mean["2016", "lang"] <- mean(Internet_2016$lang) #显著性地用lang更多
cfps_mean["2016", "lang"] <- mean(data_2016$lang)

# prov 差别不大
Inter_mean["2016", "prov"] <- mean(Internet_2016$prov) 
cfps_mean["2016", "prov"] <- mean(data_2016$prov)



#####
#### 2018 ####
# Internet Firm Data set
{
  Internet_2018 <- cfps_2018 %>%
    filter(indu == "7" & comp == "1" & edu %nin% c("1", "2")) %>%
    subset(select = income)
  
  Internet_2018 <- data_2018 %>%
    filter(indu == 1) %>%
    subset(select = -income) %>%
    cbind(Internet_2018)
  
  compare <- filter(cfps_2018, 
                    (indu == "7" & comp == "1" & edu %nin% c("1", "2")))
  setequal(compare$income, Internet_2018$income)
  
  remove(compare)
}

# age 看不出来什么

# income 确实收入更高
Inter_mean["2018", "income"] <- mean(Internet_2018$income)
quantile(Internet_2018$income)

cfps_mean["2018", "income"] <- mean(cfps_2018$income)
quantile(cfps_2018$income)


# Graph 4
# 方案二：
{
  income_vec <- Internet_2018$income
  Distr1_df <- data.frame(income_vec)
  income_vec <- cfps_2018$income
  Distr2_df <- data.frame(income_vec)
  remove(income_vec)
}

{
  ggplot() +
    geom_density(data = Distr1_df,
                 aes(x = income_vec, 
                     fill = "Inter"),
                 alpha = 0.7,
                 colour = "black") +
    scale_x_continuous(name = "Income",
                       labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) + #使坐标轴为正常数字
    labs(y = NULL) + #y轴不要标签
    geom_vline(aes(xintercept = 
                     mean(Distr1_df$income_vec),
                   colour = "Inter"),
               linetype = 2) +
    geom_density(data = Distr2_df,
                 aes(x = income_vec,
                     fill = "Total"),
                 alpha = 0.7,
                 colour = "black",
                 adjust = 2) + #第二个density
    geom_vline(aes(xintercept = 
                     mean(Distr2_df$income_vec),
                   colour = "Total"),
               linetype = 2) + #第二个mean line
    scale_colour_manual(values = c(
      "Inter"="#CC79A7",
      "Total"="#56B4E9"),
      aesthetics = c("fill")) +
    scale_colour_manual(values = c(
      "Inter" = "#D55E00",
      "Total" = "#0072B2"),
      aesthetics = c("colour")) + #按color和fill的map生成图例
    guides(fill = guide_legend(
      title = "Distrubtion")) + 
    guides(colour = guide_legend(
      title = "Mean")) + #给图例改名
    ggtitle("Graph 3: Internet Industry and Total Sample, 2018")
  
}

remove(Distr1_df, Distr2_df)


# edu
Inter_mean["2018", "edu"] <- mean(Internet_2018$edu)
cfps_mean["2018", "edu"] <- mean(data_2018$edu) # higher percentage for college above

# gender 差距不大
Inter_mean["2018", "gender"] <- mean(Internet_2018$gender)
cfps_mean["2018", "gender"] <- mean(data_2018$gender)

# comp 
Inter_mean["2018", "comp"] <- 1 #显著性地用comp更多
cfps_mean["2018", "comp"] <- mean(data_2018$comp)

# lang
Inter_mean["2018", "lang"] <- mean(Internet_2018$lang) #显著性地用lang更多
cfps_mean["2018", "lang"] <- mean(data_2018$lang)

# prov 差别不大
Inter_mean["2018", "prov"] <- mean(Internet_2018$prov) 
cfps_mean["2018", "prov"] <- mean(data_2018$prov)



#####
#### 2020 ####
# Internet Firm Data set
{
  Internet_2020 <- cfps_2020 %>%
    filter(indu == "7" & comp == "1" & edu %nin% c("1", "2")) %>%
    subset(select = income)
  
  Internet_2020 <- data_2020 %>%
    filter(indu == 1) %>%
    subset(select = -income) %>%
    cbind(Internet_2020)
  
  compare <- filter(cfps_2020, 
                    (indu == "7" & comp == "1" & edu %nin% c("1", "2")))
  setequal(compare$income, Internet_2020$income)
  
  remove(compare)
}

# age 看不出来什么

# income 确实收入更高
Inter_mean["2020", "income"] <- mean(Internet_2020$income)
quantile(Internet_2020$income)

cfps_mean["2020", "income"] <- mean(cfps_2020$income)
quantile(cfps_2020$income)


# Graph 4
# 方案二：
{
  income_vec <- Internet_2020$income
  Distr1_df <- data.frame(income_vec)
  income_vec <- cfps_2020$income
  Distr2_df <- data.frame(income_vec)
  remove(income_vec)
}

{
  ggplot() +
    geom_density(data = Distr1_df,
                 aes(x = income_vec, 
                     fill = "Inter"),
                 alpha = 0.7,
                 colour = "black") +
    scale_x_continuous(name = "Income",
                       labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) + #使坐标轴为正常数字
    labs(y = NULL) + #y轴不要标签
    geom_vline(aes(xintercept = 
                     mean(Distr1_df$income_vec),
                   colour = "Inter"),
               linetype = 2) +
    geom_density(data = Distr2_df,
                 aes(x = income_vec,
                     fill = "Total"),
                 alpha = 0.7,
                 colour = "black",
                 adjust = 2) + #第二个density
    geom_vline(aes(xintercept = 
                     mean(Distr2_df$income_vec),
                   colour = "Total"),
               linetype = 2) + #第二个mean line
    scale_colour_manual(values = c(
      "Inter"="#CC79A7",
      "Total"="#56B4E9"),
      aesthetics = c("fill")) +
    scale_colour_manual(values = c(
      "Inter" = "#D55E00",
      "Total" = "#0072B2"),
      aesthetics = c("colour")) + #按color和fill的map生成图例
    guides(fill = guide_legend(
      title = "Distrubtion")) + 
    guides(colour = guide_legend(
      title = "Mean")) + #给图例改名
    ggtitle("Graph 4: Internet Industry and Total Sample, 2020")
  
}

remove(Distr1_df, Distr2_df)


# edu
Inter_mean["2020", "edu"] <- mean(Internet_2020$edu)
cfps_mean["2020", "edu"] <- mean(data_2020$edu) # higher percentage for college above

# gender 差距不大
Inter_mean["2020", "gender"] <- mean(Internet_2020$gender)
cfps_mean["2020", "gender"] <- mean(data_2020$gender)

# comp 
Inter_mean["2020", "comp"] <- 1 #显著性地用comp更多
cfps_mean["2020", "comp"] <- mean(data_2020$comp)

# lang
Inter_mean["2020", "lang"] <- mean(Internet_2020$lang) #显著性地用lang更多
cfps_mean["2020", "lang"] <- mean(data_2020$lang)

# prov 差别不大
Inter_mean["2020", "prov"] <- mean(Internet_2020$prov) 
cfps_mean["2020", "prov"] <- mean(data_2020$prov)



#####



#### Check ####

check <- vector(mode = "double", length = ncol(data_2014))
for (i in c(1:ncol(data_2014))){
  check[i] <- sum(str_detect(data_2014[i], pattern = "^-8$"))
}
check

check <- vector(mode = "double", length = ncol(data_2016))
for (i in c(1:ncol(data_2016))){
  check[i] <- sum(str_detect(data_2016[i], pattern = "^-8$"))
}
check


check <- vector(mode = "double", length = ncol(data_2018))
for (i in c(1:ncol(data_2018))){
  check[i] <- sum(str_detect(data_2018[i], pattern = "^-8$"))
}
check


check <- vector(mode = "double", length = ncol(data_2020))
for (i in c(1:ncol(data_2020))){
  check[i] <- sum(str_detect(data_2020[i], pattern = "^-8$"))
}
check
#####

