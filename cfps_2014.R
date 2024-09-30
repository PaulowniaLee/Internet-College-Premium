#### Environment ####
library("tidyverse")
if(F){ #方程重名，不能一起用
detach("package:haven", unload = TRUE)
library("readstata13")
}
library("foreign")
library("haven")
detach("package:readstata13", unload = TRUE)


rstudioapi::writeRStudioPreference("data_viewer_max_columns",
                                   1500L)
options(max.print = 1500)
setwd("/Users/mac")
getwd()

#####

#### Import ####
route <- paste0(
  "/Users/mac/cfps2014adult_201906.sas7bdat"
)
dat_income <- read_sas(route)
cfps_2014_income <- data.frame(dat_income)


remove(route)
remove(dat_income)
#####

#### Cleaning ####

# 生成variable对照表
names <- colnames(cfps_2014_income)
footnote <- c(1:length(names))
for (i in c(1:length(names))){
  name_list <- c("cfps_2014_income$", names[i])
  footnote[i] <- attr(
    eval(parse(text = 
                 paste(name_list, collapse = "")
    )), 
    "label")
  # eval() 和 parse() 两个一起
  # 让 paste() 生成的字符串被识别为variable name
}
remove(i, name_list)

variable_names <- data.frame(names, footnote)
write_csv(variable_names,
          file = "var_names_2014.csv")
remove(footnote, names, variable_names)

# Variable Test Block 

# 学历选 cfps2014edu
{
cfps_2014_income$CFPS_LATEST_EDU
cfps_2014_income$PW1R
cfps_2014_income$cfps2014edu
sum(is.na(cfps_2014_income$PW1R))
sum(is.na(cfps_2014_income$cfps2014edu))


v1 <- cfps_2014_income$CFPS_LATEST_EDU
v1 <- lapply(v1,
       function(x) replace(x, x==-8, NA))
sum(is.na(v1))

v2 <-cfps_2014_income$cfps2014edu
v1 <- lapply(v2,
             function(x) replace(x, x==-8, NA))
sum(is.na(v2))

remove(v1, v2)
}

cfps_2014_income_cut$PA301
cfps_2014_income_cut$QEA0
cfps_2014_income$EMPLOY2014

cfps_2014_income_cut$p_wage
cfps_2014_income_cut$p_income
(str_count(cfps_2014_income_cut$p_income - cfps_2014_income_cut$p_wage,
          pattern = fixed("0")))
cfps_2014_income_cut$p_income - cfps_2014_income_cut$p_wage


v1 <- cfps_2014_income_cut$INCOME
length(v1) #5569
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #1148 缺失
sum(str_detect(v1, pattern = "^0$")) #1079 无工资
#缺2227

v1 <- cfps_2014_income_cut$p_wage
length(v1) #5569
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #0 缺失
sum(str_detect(v1, pattern = "^0$")) #2084 无工资
#缺2084

v1 <- cfps_2014_income_cut$INCOMEB
length(v1) #5569
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #1800 缺失
sum(str_detect(v1, pattern = "^0$")) #103 无工资
#缺1903

v1 <- cfps_2014_income_cut$INCOMEA
length(v1) #5569
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #6 缺失
sum(str_detect(v1, pattern = "^0$")) #4988 无工资



# 就业的
cfps_2014_income_cut <- filter(
  cfps_2014_income, EMPLOY2014 == 1) 


# 精简数据
{
cfps_2014_income_final <- subset(cfps_2014_income_cut,
                                 select = c("cfps2014edu", #学历
                                            "QA301", #户口(农/城)
                                            "QEA0", #婚姻
                                            "QA701CODE",#民族
                                            "CFPS_GENDER", 
                                            "CFPS2014_AGE",
                                            "provcd14",
                                            "INCOME",#工资性收入
                                            "QG2", #雇主性质
                                            "QG302CODE", #行业
                                            "QG18", #外语
                                            "QG19", #计算机
                                            "QG16", #单位规模
                                            "QZ207" #智力
                                            )) %>%
  filter(!INCOME %in% c("0", "-8"))  %>%
    filter(!QG302CODE == "-8")
  }
# 7394

# Save 
setwd("/Users/mac/Desktop/UCL\ Publish/Data")
saveRDS(cfps_2014_income_final, file = "cfps_2014_income.rds")
setwd("/Users/mac")
#####




               
  

