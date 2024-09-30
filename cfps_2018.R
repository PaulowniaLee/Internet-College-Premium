#### Environment ####
library("tidyverse")
if(F){ #方程重名，不能一起用
library("readstata13")
}
detach("package:readstata13", unload = TRUE)
library("foreign")
library("haven")


rstudioapi::writeRStudioPreference("data_viewer_max_columns",
                                   1500L)
options(max.print = 1500)
getwd()

#####

#### Import ####

route <- paste0(
  "/Users/mac/cfps2018person_202012.sas7bdat"
)
dat_income <- read_sas(route)
cfps_2018_income <- data.frame(dat_income)


remove(route)
remove(dat_income)
#####

#### Cleaning ####

# 只要就业的
cfps_2018_income_cut <- filter(
  cfps_2018_income, EMPLOY == 1) 

# 生成variable对照表
names <- colnames(cfps_2018_income)
footnote <- c(1:length(names))
for (i in c(1:length(names))){
  name_list <- c("cfps_2018_income_cut$", names[i])
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
          file = "var_names.csv")
remove(footnote, names, variable_names)


#行业
# KGD3CODE
v1 <- cfps_2018_income_cut$KGD3CODE
length(v1) #6658
sum(is.na(v1)) #437 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #3105 缺失

# QG302CODE 最优
v1 <- cfps_2018_income_cut$QG302CODE
length(v1) #6658
sum(is.na(v1)) #437 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #703 缺失

# QGA4CODE
v1 <- cfps_2018_income_cut$QGA4CODE
length(v1) #6658
sum(is.na(v1)) #437 NA
v1 <- na.omit(v1)
sum(str_detect(v1, pattern = "-8")) #6200 缺失



# 精简数据
{
cfps_2018_income_final <- subset(cfps_2018_income_cut,
                                 select = c("CFPS2018EDU", #学历
                                            "QA301", #户口
                                            "QEA0", #婚姻
                                            "QA701CODE",#民族
                                            "GENDER", 
                                            "AGE",
                                            "PROVCD18",
                                            "INCOME",
                                            "QG2", #雇主性质
                                            "QG302CODE", #行业
                                            "QG18", #外语
                                            "QG19", #计算机
                                            "QG16", #单位规模
                                            "KZ207" #智力
                                            )) %>%
  filter(!INCOME %in% c("-9", "-8", "0")) %>%
    filter(!QG302CODE == "-8")
}
#9024

# Save 
setwd("/Users/mac/Desktop/UCL\ Publish/Data")
saveRDS(cfps_2018_income_final, file = "cfps_2018_income.rds")

#####




               
  

