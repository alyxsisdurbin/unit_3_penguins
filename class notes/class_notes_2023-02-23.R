# Haley Durbin
# 2023-02-23

library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(broom)

####### Linear Regression

head(penguins)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color = species))

penguins %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs()

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm_1)

ggplot(data=penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

class(lm_1)
plot(lm_1) #no esta bien

gentoo = penguins %>% 
  filter(species=="Gentoo")

gentoo %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs() 

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)
