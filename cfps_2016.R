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
  "/Users/mac/cfps2016adult_201906.sas7bdat"
)
dat_income <- read_sas(route)
cfps_2016_income <- data.frame(dat_income)


remove(route)
remove(dat_income)
#####

#### Cleaning ####

# 生成variable对照表
names <- colnames(cfps_2016_income)
footnote <- c(1:length(names))
for (i in c(1:length(names))){
  name_list <- c("cfps_2016_income$", names[i])
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
          file = "var_names_2016.csv")
remove(footnote, names, variable_names)

# Variable Test Block 

# 学历选 cfps2016edu
{
cfps_2016_income$CFPS_LATEST_EDU
cfps_2016_income$PW1R
cfps_2016_income$cfps2016edu
sum(is.na(cfps_2016_income$PW1R))
sum(is.na(cfps_2016_income$cfps2016edu))


v1 <- cfps_2016_income$CFPS_LATEST_EDU
v1 <- lapply(v1,
       function(x) replace(x, x==-8, NA))
sum(is.na(v1))

v2 <-cfps_2016_income$cfps2016edu
v1 <- lapply(v2,
             function(x) replace(x, x==-8, NA))
sum(is.na(v2))

remove(v1, v2)
}

#行业
# QG302CODE 最优
v1 <- cfps_2016_income_cut$QG302CODE
length(v1) #5634
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #733 缺失

# QGA4CODE
v1 <- cfps_2016_income_cut$QGA4CODE
length(v1) #5634
sum(is.na(v1)) #0 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #5634 缺失

# 收入
v1 <- cfps_2016_income_cut$INCOME
length(v1) #5634
sum(is.na(v1)) # no NA
sum(str_detect(v1, pattern = "-8")) #3063 缺失
sum(str_detect(v1, pattern = "^0$")) #483 无工资

v1 <- cfps_2016_income_cut$INCOMEA
length(v1) #5634
sum(is.na(v1)) # no NA
sum(str_detect(v1, pattern = "-8")) #109 缺失
sum(str_detect(v1, pattern = "^0$")) #4912 无income A

v1 <- cfps_2016_income_cut$INCOMEB
length(v1) #5634
sum(is.na(v1)) # no NA
sum(str_detect(v1, pattern = "-8")) #3601 缺失
sum(str_detect(v1, pattern = "^0$")) #59 无income B

v1 <- cfps_2016_income_cut$incomeb_imp
length(v1) #5634
sum(is.na(v1)) # 929 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #2578 缺失
sum(str_detect(v1, pattern = "^0$")) #2 无income imp
#这里差补值有3054，前面收入缺失有3063


# 学历不做要求，要求就业
cfps_2016_income_cut <- filter(
  cfps_2016_income, EMPLOY == 1) 


# 精简数据
{
cfps_2016_income_final <- subset(cfps_2016_income_cut,
                                 select = c("cfps2016edu", #学历
                                            "PA301", #户口(农/城)
                                            "QEA0", #婚姻
                                            "PA701CODE",#民族
                                            "CFPS_GENDER", 
                                            "CFPS_AGE",
                                            "provcd16",
                                            "INCOME", #收入
                                            "incomeb_imp",#收入插补
                                            "QG2", #雇主性质
                                            "QG302CODE", #行业
                                            "QG18", #外语
                                            "QG19", #计算机
                                            "QG16", #单位规模
                                            "QZ207" #智力
                                            )) %>%
  filter(!INCOME %in% c("0")) %>%
    filter(!QG302CODE == "-8") 
  }

cfps_2016_income_final <- na.omit(cfps_2016_income_final)
#total 8949 observation 

# Save 
setwd("/Users/mac/Desktop/UCL\ Publish/Data")
saveRDS(cfps_2016_income_final, file = "cfps_2016_income.rds")
setwd("/Users/mac")
#####




               
  

