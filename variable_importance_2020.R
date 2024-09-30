devtools::install_github("imbs-hl/ranger", force = T)

install.packages("randomForestSRC")
library("randomForestSRC")


test_2020 <- subset(data_2020,
                    select = c("edu", "marr", "age",
                               "gender", "exp", "exp2",
                               "prov", "income", "employer",
                               "indu", "lang", "comp", "bac"))

# young
test_2020 <- test_2020 %>%
  filter(age <= 40)
test_2020 <- subset(test_2020,
                    select = c("edu", "marr", "comp",
                               "gender", "exp", "exp2",
                               "prov", "income", "lang",
                               "indu"))

#### ranger BC-MDA ####


forest <- ranger(test_2020$income ~.,
                 data = test_2020,
                 num.trees = 500,
                 importance = 'permutation')
MDA <- forest$variable.importance
sort(MDA, decreasing = T)

forest <- ranger(test_2020$income ~.,
                 data = test_2020,
                 num.trees = 500,
                 importance = "impurity_corrected")
MDA <- forest$variable.importance
sort(MDA, decreasing = T)
#####


#### SRC IK-MDA ####
forest <- rfsrc(income ~ .,
                data = test_2020,
                importance = "permute")
MDA <- forest$importance
sort(MDA, decreasing = T)

forest <- rfsrc(income ~ .,
                data = test_2020,
                importance = "random")
MDA <- forest$importance
sort(MDA, decreasing = T)
#####