library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(ggdag)  # Make DAGs
library(scales)  # Format numbers with functions like comma(), percent(), and dollar()
library(broom)  # Convert models to data frames
library(patchwork)  # Combine ggplots into single composite plots

set.seed(1234)   # Make all random draws reproducible

cashtransfers %>%
  count(treat) %>% 
  mutate(prop = n / sum(n))

cashtransfers %>%
  count(purecontrol) %>% 
  mutate(prop = n / sum(n))

cashtransfers %>%
  count(spillover) %>% 
  mutate(prop = n / sum(n))
##############

cashtransfers %>% 
  group_by(treat) %>% 
  summarize(avg_post = mean(cons_food))

model_rct1 <- lm(cons_food ~ treat, data = cashtransfers)
tidy(model_rct1)

#consumption of food increased in 141 Kenyan currency on avg

##############

cashtransfers %>% 
  group_by(treat) %>% 
  summarize(avg_post1 = mean(cons_social))

model_rct2 <- lm(cons_social ~ treat, data = cashtransfers)
tidy(model_rct2)

#social consumption increased in 568 Kenyan currency on avg

##############

cashtransfers %>% 
  group_by(treat) %>% 
  summarize(avg_post2 = mean(cons_total))

model_rct3 <- lm(cons_total ~ treat, data = cashtransfers)
tidy(model_rct3)

#total consumption increased in 1920 Kenyan currency on average

##############

ggplot(cashtransfers, aes(x = treat, y = cons_total, color = treat)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "Total Consumption")


##############

cashtransfers %>% 
  group_by(spillover) %>% 
  summarize(avg_post3 = mean(cons_food))

model_rct4 <- lm(cons_food ~ spillover, data = cashtransfers)
tidy(model_rct4)

#consumption of food reduced in 194 Kenyan currency on avg

##############

cashtransfers %>% 
  group_by(spillover) %>% 
  summarize(avg_post4 = mean(cons_social))

model_rct5 <- lm(cons_social ~ spillover, data = cashtransfers)
tidy(model_rct5)

#social consumption increased in -1374 Kenyan currency on avg

##############

cashtransfers %>% 
  group_by(spillover) %>% 
  summarize(avg_post5 = mean(cons_total))

model_rct6 <- lm(cons_total ~ spillover, data = cashtransfers)
tidy(model_rct6)

#total consumption increased in 1751 Kenyan currency on average


