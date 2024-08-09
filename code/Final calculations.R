library(haven)
library(tidyverse)
library(dplyr)
library(survey)
library(gtsummary)
library(ggplot2)
library(ltm)
library(sjlabelled)
library(forcats)
library(matrixStats)
library(corrplot)
library(kableExtra)
library(sjPlot)
library(knitr)
library(lavaan)
library(psych) 
library(semPlot)
library(summarytools)
library(ggfortify)
library(weights)
library(car)


#-------------------------------------------------------------------------------
  
# read data 2018
all_data2018 <- read.csv("../data/daF3482-2018.csv", sep = ";")
  
# read data 2019
all_data2019 <- read.csv("../data/daF3493-2019.csv", sep = ";")
  
# read data 2020
all_data2020 <- read.csv("../data/daF3645-2020.csv", sep = ";")
  
# read data 2021
all_data2021 <- read.csv("../data/daF3727-2021.csv", sep = ";")
  
# read data 2022
all_data2022 <- read.csv("../data/daF3784-2022.csv", sep = ";")
  
#-------------------------------------------------------------------------------
    
# select variables of interest
data2018 <- all_data2018 %>% dplyr::select(fsd_no, fsd_id, 
                                               tb_paino, Syvu, sose, ika, 
                                               sukup, lisco_1,
                                               K11A_1, K11A_2, K11A_3,
                                               K21B_1, K52c, K52a, K52b, 
                                               K48, K43A)
  
  
data2019 <- all_data2019 %>% dplyr::select(fsd_no, fsd_id, 
                                             tb_paino, syvu, sose, ika, 
                                             sukup, lisco_1, 
                                             k11a_1, k11a_2, k11a_3,
                                             k21b_1, k52c, k52a, k52b, 
                                             k48, k43a)
  
  
  
data2020 <- all_data2020 %>% dplyr::select(fsd_no, fsd_id, 
                                             tb_paino, Syvu, sose, ika, 
                                             sukup, lisco_1,
                                             K11A_1, K11A_2, K11A_3,
                                             K21B_1, K52c, K52a, K52b, 
                                             K48, K43A)
  
  
  
data2021 <- all_data2021 %>% dplyr::select(fsd_no, fsd_id, vastaustapa, 
                                             tb_paino, Syvu, palkansa, ika, 
                                             sukup, lisco2010_1,
                                             K11a_1, K11a_2, K11a_3,
                                             K21b_1, K52c, K52a, K52b, 
                                             K48, K43a)
  
  
  
data2022 <- all_data2022 %>% dplyr::select(fsd_no, fsd_id, vastaustapa, 
                                             tb_paino, Syvu, palkansa, ika, 
                                             sukup, lisco2010_1,
                                             K11a_1, K11a_2, K11a_3,
                                             K21b_1, K52c, K52a, K52b, 
                                             K48, K43a)
  
#-------------------------------------------------------------------------------
  
for (i in 6:ncol(data2018))
    print(table(data2018[i], useNA = "ifany"))

data2018$lisco_1[is.na(data2018$lisco_1)] <- 99
data2018$K11A_1[data2018$K11A_1 == 5] <- NA
data2018$K11A_3[data2018$K11A_3 == 5] <- NA
data2018$K21B_1[data2018$K21B_1 == 9] <- NA
data2018$K52b[data2018$K52b == 9] <- NA
data2018$K48[data2018$K48 == 3] <- NA
data2018$K43A[data2018$K43A == 9] <- NA


for (i in 6:ncol(data2018))
  print(table(data2018[i], useNA = "ifany"))

#-------------------------------------------------------------------------------

for (i in 6:ncol(data2019))
  print(table(data2019[i], useNA = "ifany"))

data2019$lisco_1[is.na(data2019$lisco_1)] <- 99
data2019$k11a_1[data2019$k11a_1 == 5] <- NA
data2019$k11a_2[data2019$k11a_2 == 5] <- NA
data2019$k11a_3[data2019$k11a_3 == 5] <- NA
data2019$k21b_1[data2019$k21b_1 == 9] <- NA
data2019$k52b[data2019$k52b == 9] <- NA
data2019$k48[data2019$k48 == 3] <- NA
data2019$k43a[data2019$k43a == 9] <- NA


for (i in 6:ncol(data2019))
  print(table(data2019[i], useNA = "ifany"))

#-------------------------------------------------------------------------------
  
for (i in 6:ncol(data2020))
    print(table(data2020[i], useNA = "ifany"))

data2020$lisco_1[is.na(data2020$lisco_1)] <- 99
data2020$K11A_1[data2020$K11A_1 == 5] <- NA
data2020$K11A_3[data2020$K11A_3 == 5] <- NA
data2020$K52b[data2020$K52b == 9] <- NA
data2020$K48[data2020$K48 == 3] <- NA


for (i in 6:ncol(data2020))
  print(table(data2020[i], useNA = "ifany"))

#-------------------------------------------------------------------------------
  
for (i in 7:ncol(data2021))
    print(table(data2021[i], useNA = "ifany"))

data2021$lisco2010_1[is.na(data2021$lisco2010_1)] <- 99
data2021$K11a_1[data2021$K11a_1 == 8] <- NA
data2021$K11a_2[data2021$K11a_2 == 8] <- NA
data2021$K11a_2[data2021$K11a_2 == 9] <- NA
data2021$K11a_3[data2021$K11a_3 == 9] <- NA
data2021$K21b_1[data2021$K21b_1 == 9] <- NA
data2021$K52c[data2021$K52c == 9] <- NA
data2021$K52a[data2021$K52a == 9] <- NA
data2021$K52b[data2021$K52b == 9] <- NA
data2021$K48[is.na(data2021$K48)] <- 2
data2021$K43a[data2021$K43a == 9] <- NA


for (i in 7:ncol(data2021))
  print(table(data2021[i], useNA = "ifany"))

#-------------------------------------------------------------------------------
  
for (i in 7:ncol(data2022))
    print(table(data2022[i], useNA = "ifany"))

data2022$lisco2010_1[is.na(data2022$lisco2010_1)] <- 99
data2022$K11a_1[data2022$K11a_1 == 9] <- NA
data2022$K11a_2[data2022$K11a_2 == 9] <- NA
data2022$K11a_3[data2022$K11a_3 == 9] <- NA
data2022$K21b_1[data2022$K21b_1 == 9] <- NA
data2022$K52c[data2022$K52c == 9] <- NA
data2022$K52a[data2022$K52a == 9] <- NA
data2022$K52b[data2022$K52b == 9] <- NA
data2022$K48[is.na(data2022$K48)] <- 2


for (i in 7:ncol(data2022))
  print(table(data2022[i], useNA = "ifany"))

#-------------------------------------------------------------------------------
  
# turn weighting coeff 2020 from int to chr to be compatible with other years
data2020$tb_paino <- as.character(data2020$tb_paino)


# turn data sets' variable to factor
cdata2018 <- data.frame(sapply(data2018, as.factor))
cdata2019 <- data.frame(sapply(data2019, as.factor))
cdata2020 <- data.frame(sapply(data2020, as.factor))
cdata2021 <- data.frame(sapply(data2021, as.factor))
cdata2022 <- data.frame(sapply(data2022, as.factor))


# harmonize column's names
cdata2018 <- rename(data2018, K11a_1 = K11A_1, K11a_2 = K11A_2, K11a_3 = K11A_3,
                    K21b_1 = K21B_1, K43a = K43A)


cdata2019 <- rename(data2019, Syvu = syvu, K11a_1 = k11a_1, 
                    K11a_2 = k11a_2, K11a_3 = k11a_3,
                    K21b_1 = k21b_1, K52c = k52c, K52a = k52a,
                    K52b = k52b, K48 = k48, K43a = k43a)


cdata2020 <- rename(data2020, K11a_1 = K11A_1, K11a_2 = K11A_2, K11a_3 = K11A_3,
                    K21b_1 = K21B_1, K43a = K43A)


cdata2021 <- rename(data2021, lisco_1 = lisco2010_1, sose = palkansa)

cdata2022 <- rename(data2022, lisco_1 = lisco2010_1, sose = palkansa)




# combine 5 data sets
data <- dplyr::bind_rows(cdata2018, cdata2019, cdata2020, cdata2021, cdata2022)




# create 'year' variable out of 'fsd_no'
data <- data %>% 
  mutate(year = fsd_no %>%
           factor() %>%
           fct_recode("2018" = "3482",
                      "2019" = "3493",
                      "2020" = "3645",
                      "2021" = "3727",
                      "2022" = "3784"))


# create 'age' variable out of 'ika'
data$age <- cut(data$ika, breaks = c(18, 25, 35, 45, 55, 66),
                            include.lowest = TRUE,
                            right = FALSE, labels = c(1, 2, 3, 4, 5)) 
# 18-24 / 25-34 / 35-44 / 45/54/ 55-66
# 35 years old = group 3




# transform weight from string to numeric 
data$tb_paino <- ifelse(grepl("^,", data$tb_paino), 
                        paste0("0", data$tb_paino), 
                        data$tb_paino)
data$tb_paino <- gsub(",", ".", data$tb_paino)
data$tb_paino <- as.numeric(data$tb_paino)




# check the integrated data (only categorical variables)
for (i in 5:17)
  print(table(data[i], useNA = "ifany"))

#-------------------------------------------------------------------------------
  
# change scale and direction for all items of dimension 1
data$new_K11a_1 <- as.numeric(data$K11a_1)
data$new_K11a_1 <- (1/3)*(4-data$new_K11a_1)

data$new_K11a_2 <- as.numeric(data$K11a_2)
data$new_K11a_2 <- (1/3)*(4-data$new_K11a_2)

data$new_K11a_3 <- as.numeric(data$K11a_3)
data$new_K11a_3 <- (1/3)*(4-data$new_K11a_3)

# 2-level variables: change direction
data$new_K21b_1 <- as.numeric(data$K21b_1)
data$new_K21b_1 <- (2-data$new_K21b_1)

data$new_K52c <- as.numeric(data$K52c)
data$new_K52c <- (2-data$new_K52c)

data$new_K52a <- as.numeric(data$K52a)
data$new_K52a <- (2-data$new_K52a)

data$new_K52b <- as.numeric(data$K52b)
data$new_K52b <- (2-data$new_K52b)

data$new_K48 <- as.numeric(data$K48)
data$new_K48 <- (2-data$new_K48)

data$new_K43a <- as.numeric(data$K43a)
data$new_K43a <- (1/4)*(5-data$new_K43a)

#-------------------------------------------------------------------------------
  
# autonomy
data$autonomy <- rowMeans(data[,21:23], na.rm=TRUE)
summary(data$autonomy)  # mean: 0.4555


# continuous skill-building
data$skill_building <- rowMeans(data[,24:27], na.rm=TRUE)  
summary(data$skill_building)   # mean: 0.4739  


# collaborative_work
data$collaborative_work <- rowMeans(data[,28:29], na.rm=TRUE)  
summary(data$collaborative_work)   # mean: 0.4839 


# work complexity
data$work_complexity <- rowMeans(data[,30:32], na.rm=TRUE)
summary(data$work_complexity)   # mean: 0.4711  


# save data
write.table(data, file = "../data/final_data.txt", row.names = FALSE, sep = ',')


#-------------------------------------------------------------------------------

# weighting data


# read register data 
register2022 <- read.csv("../data/register2022.csv", sep = ',')
register2021 <- read.csv("../data/register2021.csv", sep = ',')
register2020 <- read.csv("../data/register2020.csv", sep = ',')
register2019 <- read.csv("../data/register2019.csv", sep = ',')
register2018 <- read.csv("../data/register2018.csv", sep = ',')

register <- dplyr::bind_rows(register2018, register2019, register2020, register2021, register2022)

register_long <- register %>%
  pivot_longer(cols = c('Males', 'Females'), names_to = "Gender.group", values_to = "Joint")

register_long <- register_long %>%
  mutate(Population.size = case_when(Year == '2018' ~ 2135347,
                                     Year == '2019' ~ 2133398,
                                     Year == '2020' ~ 2046297,
                                     Year == '2021' ~ 2133908,
                                     Year == '2022' ~ 2180180))


register_long <- register_long %>%
  mutate(Population.proportion = Joint/Population.size)




# save the output of cross-tabulation of 'occupation' and 'gender'
cross_class2018 <- as.data.frame(unclass(table(cdata2018$lisco_1, cdata2018$sukup)))
colnames(cross_class2018) <- c('males', 'females')
cross_class2018$Occupational.group <- rownames(cross_class2018)
cross_class2018$Year <- 2018

cross_class2019 <- as.data.frame(unclass(table(cdata2019$lisco_1, cdata2019$sukup)))
colnames(cross_class2019) <- c('males', 'females')
cross_class2019$Occupational.group <- rownames(cross_class2019)
cross_class2019$Year <- 2019


cross_class2020 <- as.data.frame(unclass(table(cdata2020$lisco_1, cdata2020$sukup)))
colnames(cross_class2020) <- c('males', 'females')
cross_class2020$Occupational.group <- rownames(cross_class2020)
cross_class2020$Year <- 2020


cross_class2021 <- as.data.frame(unclass(table(cdata2021$lisco_1, cdata2021$sukup)))
colnames(cross_class2021) <- c('males', 'females')
cross_class2021$Occupational.group <- rownames(cross_class2021)
cross_class2021$Year <- 2021


cross_class2022 <- as.data.frame(unclass(table(cdata2022$lisco_1, cdata2022$sukup)))
colnames(cross_class2022) <- c('males', 'females')
cross_class2022$Occupational.group <- rownames(cross_class2022)
cross_class2022$Year <- 2022


sample <- dplyr::bind_rows(cross_class2018, cross_class2019, cross_class2020, cross_class2021, cross_class2022)

sample_long <- sample %>%
  pivot_longer(cols = c('males', 'females'), names_to = "gender.group", values_to = "joint")


sample_long <- sample_long %>%
  mutate(sample.size = case_when(Year == '2018' ~ nrow(cdata2018),
                                 Year == '2019' ~ nrow(cdata2019),
                                 Year == '2020' ~ nrow(cdata2020),
                                 Year == '2021' ~ nrow(cdata2021),
                                 Year == '2022' ~ nrow(cdata2022)))

sample_long <- sample_long %>%
  mutate(gender.group = gender.group %>%
           factor() %>%
           fct_recode("Males" = "males",
                      "Females" = "females"))
sample_long <- sample_long %>% rename(Gender.group = gender.group)

merged_sample <- merge(register_long, sample_long)

merged_sample$weight <- merged_sample$Population.proportion/merged_sample$joint
merged_sample$scaled_weight <- merged_sample$weight*1000



weight <- merged_sample[c(1, 2, 3, 9, 10)]

weight <- weight %>%
  mutate(Gender.group = Gender.group %>%
           factor() %>%
           fct_recode("1" = "Males",
                      "2" = "Females"))


data <- merge(data, weight,
              by.x = c("sukup", "lisco_1", "year"),
              by.y = c("Gender.group", "Occupational.group", "Year"))


data$weighted_autonomy <- data$autonomy*data$scaled_weight
data$weighted_skill_building <- data$skill_building*data$scaled_weight
data$weighted_collaborative_work <- data$collaborative_work*data$scaled_weight
data$weighted_work_complexity <- rowMeans(data[,37:39], na.rm=TRUE)



#-------------------------------------------------------------------------------

# calculate overall work complexity and dimensions per year
data %>% 
  group_by(year) %>%
  summarise(wc_mean = weighted.mean(work_complexity, na.rm=TRUE, w = scaled_weight),
            auto_mean = weighted.mean(autonomy, na.rm=TRUE, w = scaled_weight),
            sb_mean = weighted.mean(skill_building, na.rm=TRUE, w = scaled_weight),
            cw_mean = weighted.mean(collaborative_work, na.rm=TRUE, w = scaled_weight))




# calculate overall work complexity and dimensions for gender groups
data %>% 
  group_by(sukup) %>%
  summarise(wc_mean = weighted.mean(work_complexity, na.rm=TRUE, w = scaled_weight),
            auto_mean = weighted.mean(autonomy, na.rm=TRUE, w = scaled_weight),
            sb_mean = weighted.mean(skill_building, na.rm=TRUE, w = scaled_weight),
            cw_mean = weighted.mean(collaborative_work, na.rm=TRUE, w = scaled_weight))


data %>% 
  group_by(sukup) %>%
  summarise(wc_mean = sd(work_complexity, na.rm=TRUE),
            auto_mean = sd(autonomy, na.rm=TRUE),
            sb_mean = sd(skill_building, na.rm=TRUE),
            cw_mean = sd(collaborative_work, na.rm=TRUE))



# calculate overall work complexity and dimensions for age groups
data %>% 
  group_by(age) %>%
  summarise(wc_mean = weighted.mean(work_complexity, na.rm=TRUE, w = scaled_weight),
            auto_mean = weighted.mean(autonomy, na.rm=TRUE, w = scaled_weight),
            sb_mean = weighted.mean(skill_building, na.rm=TRUE, w = scaled_weight),
            cw_mean = weighted.mean(collaborative_work, na.rm=TRUE, w = scaled_weight))


#-------------------------------------------------------------------------------


# autonomy and skill_building
auto_skill <-cor.test(data$autonomy, data$skill_building,  
                        method = "spearman")
auto_skill     # 0.09147227, p < 2.2e-16

# autonomy and interaction
auto_colab <-cor.test(data$autonomy, data$collaborative_work,  
                      method = "spearman")
auto_colab    # 0.2476456, p < 2.2e-16

# skill_building and interaction
skill_colab <-cor.test(data$skill_building, data$collaborative_work,  
                       method = "spearman")
skill_colab   # 0.3129578, p < 2.2e-16

#-------------------------------------------------------------------------------

# missing data treatment

complete_cases_data <- data

complete_cases_data <- complete_cases_data[complete.cases(complete_cases_data[8:17]), ]

# autonomy
complete_cases_data$autonomy <- rowMeans(complete_cases_data[,20:22])
summary(complete_cases_data$autonomy)  # mean: 0.4559


# continuous skill-building
complete_cases_data$skill_building <- rowMeans(complete_cases_data[,23:26])  
summary(complete_cases_data$skill_building)   # mean: 0.4742  


# collaborative_work
complete_cases_data$collaborative_work <- rowMeans(complete_cases_data[,27:28])  
summary(complete_cases_data$collaborative_work)   # mean: 0.4854 


# work complexity
complete_cases_data$work_complexity <- rowMeans(complete_cases_data[,29:31])
summary(complete_cases_data$work_complexity)   # mean: 0.4718  


#-------------------------------------------------------------------------------

# split data
library(caret)

set.seed(1234)  # For reproducibility

# Create an index to split the data
index <- createDataPartition(data$sukup, p = 0.7, list = FALSE)

# Create training and test sets
train_data <- data[index, ]
test_data <- data[-index, ]




# EFA
fa <- fa(r = train_data[9:17], 
         nfactors = 3, 
         rotate = "promax",
         scores = "regression",
         fm = "ml") 

fa.diagram(fa, cut = 0)
summary(fa)
loadings(fa)
fa$communality
plot(fa)




# CFA
model <- '
# LA: Level of autonomy
# SB: Continuous skill building
# CW: Collaborative work
LA =~ new_K11a_1 + new_K11a_2 + new_K11a_3
SB =~ new_K21b_1 + new_K52c + new_K52a + new_K52b
CW =~ new_K48 + new_K43a
'
fit <- cfa(model, data = test_data, estimator = "MLM")
summary(fit, fit.measures = TRUE, standardized = TRUE)
modindices(fit, standardized = TRUE, sort = TRUE, maximum.number = 10)

semPaths(fit, style = "lisrel")

#-------------------------------------------------------------------------------

vis_data <- cor(data[9:17])
corrplot(vis_data, method = 'square', order = 'FPC', type = 'upper', diag = FALSE,
         tl.cex = 0.7, tl.col = "black")
#-------------------------------------------------------------------------------

# show WC with bar plot
sum_wc <- data %>%
  group_by(year) %>%
  summarise(work_complexity_sum=sum(weighted_work_complexity, na.rm=TRUE))


line_thickness = 0.90
point_size = 2

ggplot(sum_wc, aes(x=year, group=1)) +
  geom_line(aes(y=work_complexity_sum, color="Work complexity"), size=line_thickness) +
  geom_point(aes(y=work_complexity_sum, color="Work complexity"), size=point_size) +
  scale_color_manual(name = "", values = c("Work complexity" = 'black')) +
  labs(x="Year", y = "Average")
  #geom_text(aes(label = round(work_complexity_sum, 3)), hjust = 1.4, vjust = 0.4, size = 4)


#------------------------------------------------------------------------------


sums <- data %>%
  group_by(year) %>%
  summarise(work_complexity=sum(weighted_work_complexity, na.rm=TRUE),
            collaborative_work=sum(weighted_collaborative_work, na.rm=TRUE),
            skill_building=sum(weighted_skill_building, na.rm=TRUE),
            autonomy=sum(weighted_autonomy, na.rm=TRUE))


line_thickness = 0.75
point_size = 2

ggplot(sums, aes(x=year, group=1)) +
  geom_line(aes(y=work_complexity, color="Work complexity"), size=line_thickness) +
  geom_point(aes(y=work_complexity, color="Work complexity"), size=point_size) +
  geom_line(aes(y=collaborative_work, color="Collaborative work"), size=line_thickness) +
  geom_point(aes(y=collaborative_work, color="Collaborative work"), size=point_size) +
  geom_line(aes(y=skill_building, color="Continuous skill-building"),size=line_thickness)+
  geom_point(aes(y=skill_building, color="Continuous skill-building"), size=point_size) +
  geom_line(aes(y=autonomy, color="Level of autonomy"), size=line_thickness) +
  geom_point(aes(y=autonomy, color="Level of autonomy"), size=point_size) +
  scale_color_manual(name = "", values = c("Work complexity" = 'black',
                                           "Collaborative work" = 'darkblue',
                                           "Continuous skill-building" = 'darkgreen',
                                           "Level of autonomy" = 'orange')) +
  labs(x="Year", y = "Average")

data %>% 
group_by(lisco_1) %>%
summarise(Sum_test = sum(weighted_work_complexity, na.rm=TRUE))


#ggplot(data, aes(x=work_complexity)) +
  geom_histogram(color="black", fill = "gray") +
  labs(x="Work complexity", y = "Frequency")


ggplot(data, aes(x = year, y = weighted_work_complexity, color = year)) +
  geom_boxplot(colour = "#1F3552", fill = "#4271AE", alpha = 0.7, width = 0.5) +
  #stat_summary(fun = su, geom = "pointrange", shape = 18, size = 0.9, 
               #color = "darkorange", fill = "darkorange") +
  labs(x="Year", y = "Work complexity") 
  #geom_text(data = means, aes(x = group, y = mean_value + 0.2, label = round(mean_value, 2)))



# check diagnostics plots
autoplot(model1)
return


# check linearity
ggplot(data, aes(x = sukup, y = collaborative_work)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_jitter()


ggplot(data, aes(x = age, y = weighted_work_complexity)) +
  geom_point() +
  geom_smooth() +
  geom_jitter()


# chech normality
ggplot(data, aes(x = skill_building)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(sukup))


#-------------------------------------------------------------------------------

# regression analysis with own calculated weight
model <- lm(work_complexity ~ as_factor(sukup) + as_factor(age), data = data21_22, weight = weight)
summary(model)



# regression analysis with original weight
# Create subsets based on year
data21_22 <- data[data$year %in% c(2021, 2022), ]
data21_22$tb_paino <- as.numeric(data21_22$tb_paino)


data22 <- data[data$year %in% c(2022), ]
data22$tb_paino <- as.numeric(data22$tb_paino)


model1 <- lm(work_complexity ~ as_factor(sukup) + as_factor(age), data = data21_22, weight = tb_paino)
summary(model1)

model2 <- lm(autonomy ~ as_factor(sukup) + as_factor(age), data = data21_22, weight = tb_paino)
summary(model2)

model3 <- lm(skill_building ~ as_factor(sukup) + as_factor(age), data = data21_22, weight = tb_paino)
summary(model3)

model4 <- lm(collaborative_work ~ as_factor(sukup) + as_factor(age), data = data21_22, weight = tb_paino)
summary(model4)





data$weighted_work_complexity <- data$work_complexity*data$weight

t.test(weighted_work_complexity ~ sukup, data = data)
summary(aov(weighted_work_complexity ~ age, data = data))

data_design <- svydesign(ids=~0, strata=NULL, weights=~scaled_weight,
                         nest=TRUE, data=data)

data_design2 <- svydesign(ids=~0, strata=NULL, weights=~tb_paino,
                         nest=TRUE, data=data21_22)

data22 %>% 
  group_by(age) %>%
  summarise(wc_mean = svymean(~work_complexity, design = data_design2))
            


svyttest(work_complexity ~ sukup, design = data_design)
anova.svyglm()
h <- aov(work_complexity ~ age, data = data)
summary(h)



leveneTest(work_complexity ~ as_factor(sukup), data = data)
leveneTest(work_complexity ~ as_factor(age), data = data)

# Perform Welch's ANOVA
oneway.test(work_complexity ~ as_factor(age), data = data, var.equal = FALSE)


#------------------------------------------------------------------------------

# Gender

# means
data %>% 
  group_by(sukup) %>%
  summarise(wc_mean = mean(work_complexity, na.rm=TRUE),
            auto_mean = mean(autonomy, na.rm=TRUE),
            sb_mean = mean(skill_building, na.rm=TRUE),
            cw_mean = mean(collaborative_work, na.rm=TRUE))


# t-test
## work complexity
### check normality
ggplot(data, aes(x = work_complexity)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(sukup))  # is normal

### check homogeneity
leveneTest(work_complexity ~ as_factor(sukup), data = data)  # is homogeneous


t.test(work_complexity ~ as_factor(sukup), data = data)  # sig


## autonomy
### check normality
ggplot(data, aes(x = autonomy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(sukup))  # is normal

### check homogeneity
leveneTest(autonomy ~ as_factor(sukup), data = data)  # is homogeneous

### standard t.test
t.test(autonomy ~ as_factor(sukup), data = data)  # sig


## skill-building
### check normality
ggplot(data, aes(x = skill_building)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(sukup))  # is normal

### check homogeneity
leveneTest(skill_building ~ as_factor(sukup), data = data)  # not homogeneous


### Welch's t-test
t.test(skill_building ~ as_factor(sukup), data = data, var.equal = FALSE) # sig


## collaborative work
### check normality
ggplot(data, aes(x = collaborative_work)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(sukup))  
shapiro.test(data21_22$collaborative_work) # is normal

### check homogeneity
leveneTest(collaborative_work ~ as_factor(sukup), data = data)  # is homogeneous

### standard t.test
t.test(collaborative_work ~ as_factor(sukup), data = data)  # sig



#------------------------------------------------------------------------------

# Age

# means
data %>% 
  group_by(age) %>%
  summarise(wc_mean = mean(work_complexity, na.rm=TRUE),
            auto_mean = mean(autonomy, na.rm=TRUE),
            sb_mean = mean(skill_building, na.rm=TRUE),
            cw_mean = mean(collaborative_work, na.rm=TRUE))


# t-test
## work complexity
### check normality
ggplot(data, aes(x = work_complexity)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(age))  # is normal

### check homogeneity
leveneTest(work_complexity ~ as_factor(age), data = data)  # not homogeneous

### Welch's ANOVA
oneway.test(work_complexity ~ as_factor(age), data = data, var.equal = FALSE) # sig


## autonomy
### check normality
ggplot(data, aes(x = autonomy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(age))  # is normal

### check homogeneity
leveneTest(autonomy ~ as_factor(age), data = data)  # not homogeneous

### Welch's ANOVA
oneway.test(autonomy ~ as_factor(age), data = data, var.equal = FALSE) # not sig


## skill-building
### check normality
ggplot(data, aes(x = skill_building)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(age))  
shapiro.test(data21_22$skill_building) # is normal

### check homogeneity
leveneTest(skill_building ~ as_factor(age), data = data)  # is homogeneous

### standard anova
ano1 <- aov(collaborative_work ~ as_factor(sukup), data = data)  
summary(ano1)  # sig


## collaborative work
### check normality
ggplot(data, aes(x = collaborative_work)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  facet_wrap(vars(age))   # is normal

### check homogeneity
leveneTest(collaborative_work ~ as_factor(age), data = data)  # not homogeneous

### Welch's ANOVA
oneway.test(collaborative_work ~ as_factor(age), data = data, var.equal = FALSE) # sig
