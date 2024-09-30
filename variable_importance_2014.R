devtools::install_github("imbs-hl/ranger", force = T)

install.packages("randomForestSRC")
library("randomForestSRC")


test_2014 <- subset(data_2014,
                    select = c("edu", "marr", "age",
                               "gender", "exp", "exp2",
                               "prov", "income", "employer",
                               "indu", "lang", "comp", "bac", "iq"))

# young
test_2014 <- test_2014 %>%
  filter(age <= 40)
test_2014 <- subset(test_2014,
                    select = c("edu", "marr", "lang",
                               "gender", "exp", "exp2",
                               "prov", "income", "comp",
                               "indu"))

#### ranger BC-MDA ####


forest <- ranger(test_2014$income ~.,
                 data = test_2014,
                 num.trees = 500,
                 importance = 'permutation')
MDA <- forest$variable.importance
sort(MDA, decreasing = T)

forest <- ranger(test_2014$income ~.,
                 data = test_2014,
                 num.trees = 500,
                 importance = "impurity_corrected")
MDA <- forest$variable.importance
sort(MDA, decreasing = T)
#####

remove(forest, MDA)


#### SRC IK-MDA ####
forest <- rfsrc(income ~ .,
                data = test_2014,
                importance = "permute")
MDA <- forest$importance
sort(MDA, decreasing = T)

forest <- rfsrc(income ~ .,
                data = test_2014,
                importance = "random")
MDA <- forest$importance
sort(MDA, decreasing = T)
#####