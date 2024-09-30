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
getwd()

#####

#### Import ####
route <- paste0(
  "/Users/mac/cfps2012adult_201906.sas7bdat"
)
dat_income <- read_sas(route)
cfps_2012_income <- data.frame(dat_income)


remove(route)
remove(dat_income)
#####

#### Cleaning ####

# 生成variable对照表
names <- colnames(cfps_2012_income)
footnote <- c(1:length(names))
for (i in c(1:length(names))){
  name_list <- c("cfps_2012_income$", names[i])
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
          file = "var_names_2012.csv")
remove(footnote, names, variable_names)

# Variable Test Block 

# 学历选 edu2012
{
v1 <- cfps_2012_income$CFPS2011_LATEST_EDU
sum(str_detect(v1, pattern = "-8"))
v1 <- cfps_2012_income$edu2012
sum(is.na(v1))
v2 <- cfps_2012_income$sch2012

all.equal.numeric(target = v1, current = v2)
help("all.equal")

v1 <- na.omit(cfps_2012_income$edu2012)
range(v1)
remove(v1, v2)
}

v1 <- cfps_2012_income$edu2012
length(v1) #35719
sum(is.na(v1)) #43 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #0 缺失
#缺43

v1 <- cfps_2012_income$sch2012
length(v1) #35719
sum(is.na(v1)) #2361 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #0 缺失

#草稿
{
v1 <- cfps_2012_income$EMPLOY
sum(str_detect(v1, pattern = "1"))
sum(str_detect(v1, pattern = "0"))
sum(str_detect(v1, pattern = "8"))

cfps_2012_income_cut$income_adj #去除NA的版本



cfps_2012_income_cut$PA301
cfps_2012_income_cut$QEA0
cfps_2012_income$EMPLOY2012
}

# 收入
v1 <- cfps_2012_income_cut$INCOME
length(v1) #4980
sum(is.na(v1)) #168 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #0 缺失
sum(str_detect(v1, pattern = "^0$")) #1139 无收入
# 1307

v1 <- cfps_2012_income_cut$income_adj
length(v1) #4980
sum(is.na(v1)) #2 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #0 缺失
sum(str_detect(v1, pattern = "^0$")) #1139 无收入
# 1141


#行业
v1 <- cfps_2012_income_cut$sg410code
length(v1) #4980
sum(is.na(v1)) #2 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #4642 缺失
sum(str_detect(v1, pattern = "^0$")) #0 无收入

v1 <- cfps_2012_income_cut$sg411code_best
length(v1) #4980
sum(is.na(v1)) #2 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #4642 缺失
sum(str_detect(v1, pattern = "^0$")) #0 无收入

v1 <- cfps_2012_income_cut$job2012mn_occu #当前主要职业代码
length(v1) #4980
sum(is.na(v1)) #2 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #2083 缺失
sum(str_detect(v1, pattern = "^0$")) #0 无收入

# 只要高中以上学历，且就业的
cfps_2012_income_cut <- filter(
  cfps_2012_income, EMPLOY == 1) %>%
  filter(edu2012 %in% c("4","5", "6", "7", "8"))
# 2 小学 3 初中 4 高中 5 大专 6 本科 7 硕士 8 博士 9 没上学


# 精简数据
{
cfps_2012_income_final <- subset(cfps_2012_income_cut,
                                 select = c("edu2012", #学历
                                            "QA301", #户口(农/城)
                                            "QE104", #婚姻
                                            "QA701CODE",#民族
                                            "CFPS2012_GENDER", 
                                            "CFPS2012_AGE",
                                            "provcd",
                                            "income_adj",#收入
                                            "QG703", #雇主性质
                                            "QG302CODE", #行业
                                            "QG18", #外语
                                            "QG19", #计算机
                                            "QG705", #单位规模
                                            "QZ207" #智力
                                            )) %>%
  filter(!p_wage == 0)  %>%
    filter(!QG302CODE == "-8")
  }

# 补充income缺失情况
# 最后汇总的时候再说


# Save 
saveRDS(cfps_2012_income_final, file = "cfps_2012_income.rds")

#####




               
  

