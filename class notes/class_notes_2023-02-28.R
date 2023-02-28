# Haley Durbin
# 2023-02-28


#y= Bo + B1x1 +B2x2
#as.factor(year)
#nominal 
#discrete

library(tidyverse)
library(GGally)
library(palmerpenguins)
library(broom)
library(ggiraph)
library(ggiraphExtra)

ggplot(data=penguins) + #plot from last class
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method = "lm") +
  geom_smooth(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm), method = "lm", color="black")

### multiple regression

head(penguins)

penguins_lm_3 = penguins %>% #generating a data set for lm 3
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
dim(penguins_lm_3)


# build model

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)        
coef(lm_3) #not the best way
anova(lm_3)
broom::tidy(lm_3) # Erin's favorite way to call/ read the coefficents
my_results=broom::tidy(lm_3, conf.int=TRUE) %>% #how to add confidence intervals
  mutate_if(is.numeric, round, 2)


# Visualize model

ggPredict(lm_3) #from the giraffe package
