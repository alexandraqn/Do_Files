#Sample code to evaluate a fictitious RCT program 

#setting the libraries

library(tidyverse)  # ggplot(), %>%, mutate(), and friend
library(ggdag)  
library(scales) 
library(broom)  
library(patchwork)  

#setting the seed to allow for reproduction
set.seed(1234)

#reading dataset
dataset <- read.csv("~/Documents/GitHub/Randomization_example1/village_randomized.csv",header=T)

########################################
#checking for balance 
########################################

dataset %>%
  count(program) %>%
  mutate(prop = n / sum(n) )

#it seems it is balanced but we need to check pre-treatment balance also

dataset %>%
  group_by(program) %>%
  summarize(prop_male = mean(sex_num),
            m_age=mean(age),
            m_pre_income=mean(pre_income))

#plot to check the difference

plot_dif_gen <- ggplot(dataset, aes(x = program, y = sex_num, color = program)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "Proportion male")

#plot proportion gender

plot_prop_gen <- ggplot(dataset, aes( x = program, fill = sex)) +
  geom_bar(position = "fill")+
  labs(x = NULL, y = "proportion", fill = NULL) +
  scale_fill_manual(values = c("darkblue", "darkred"))

#show the graphs side by side

plot_dif_gen + plot_prop_gen

#it seems that there is no significant difference 
#now I will plot av age to check the distribution

plot_diff_age <- ggplot(dataset, aes(x = program, y = age, color = program)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "age")

plot_hist_age <- ggplot(dataset, aes( x = age, fill = program)) +
  geom_histogram(binwidth = 1, color = "white") +
  guides(fill = FALSE) +
  labs(x = "Age", y = "Count") +
  facet_wrap(vars(program), ncol = 1)

plot_diff_age + plot_hist_age

#no substantial diff in the av age across groups
#now we do the same for income

plot_diff_income <- ggplot(dataset, aes(x = program, y = pre_income, color = program)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "pre income")

plot_hist_income <- ggplot(dataset, aes( x = pre_income, fill = program)) +
  geom_histogram(binwidth = 20, color = "white") +
  guides(fill = FALSE) +
  labs(x = "Pre income", y = "Count") +
  facet_wrap(vars(program), ncol = 1)

plot_diff_income + plot_hist_income

#all variables look balances 

#######################################
#causal effect estimation
#######################################

#calculating the average treatment effect to find the causal effect of the program

#the average outcome for people in the program minus 
#the average outcome for people not in the program.

dataset  %>%
  group_by(program)  %>%
  summarize(avg_post= mean(post_income))

#on average the program caused an increase in income of 99 (1279-1180).

#alternative is to run a regression

model_rct <- lm(post_income ~ program, data = dataset)
tidy(model_rct)

#the program increased an average increase in income of $99.25

#end

#bonus
#I could make a grapth to make it more intuitive

ggplot(dataset, aes(x = program, y = post_income, color = program)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult=1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "Post intv Income")




