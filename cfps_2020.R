#### Environment ####
library("tidyverse")
library("readstata13")

rstudioapi::writeRStudioPreference("data_viewer_max_columns",
                                   1500L)
options(max.print = 1500)
getwd()
#####

#### Import ####

route <- paste0(
  "/Users/mac/cfps2020person_202306.dta"
)
dat_income <- read.dta13(route)
cfps_2020_income <- data.frame(dat_income)

remove(route)
remove(dat_income)
#####

#### Cleaning ####

# 生成variable对照表
names <- colnames(cfps_2020_income)
names 



# 只要就业的
cfps_2020_income_cut <- filter(
  cfps_2020_income, employ == 1) 

# 精简数据
{
cfps_2020_income_final <- subset(cfps_2020_income_cut,
                                 select = c("cfps2020edu", #学历
                                            "qa301", #户口
                                            "qea0", #婚姻
                                            "qa701code",#民族
                                            "gender", 
                                            "age",
                                            "provcd20",
                                            "emp_income", #收入
                                            "qg2", #雇主性质
                                            "qg302code", #行业
                                            "qg18", #外语
                                            "qg19", #计算机
                                            "qg16", #单位规模
                                            "qz207" #智力
                                            )) %>%
  filter(!emp_income %in% c("-8", "0")) %>%
    filter(!qg302code == "-8")
  }
#total observation = 8118

v1 <- cfps_2020_income_final$qg302code
sum(str_detect(v1, pattern = "-8"))
remove(v1)
#行业信息有166个缺失



# Save 
setwd("/Users/mac/Desktop/UCL\ Publish/Data")
saveRDS(cfps_2020_income_final, file = "cfps_2020_income.rds")

#####



               
  

