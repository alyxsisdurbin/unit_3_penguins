# Haley Durbin
# 2023-03-02

library(tidyverse)
library(GGally)
library(palmerpenguins)
library(broom)
library(ggiraph)
library(ggiraphExtra)


## build model

penguins_lm_3 = penguins %>% #generating a data set for lm 3
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)        

broom::tidy(lm_3) # Erin's favorite way to call/ read the coefficents

my_results=broom::tidy(lm_3, conf.int=TRUE) %>% #how to add confidence intervals
  mutate_if(is.numeric, round, 2)


lm_3_predictions=predict(lm_3, interval="confidence", level=0.95)

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=newdata+predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, y=bill_depth_mm, fill=species, alpha=0.5))+
  geom_line(data=lm_3_predictions, aes(y=.fitted, x=bill_length_mm, color=species))


###generate new data

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)


lm_3_predict = lm_3 %>%
  broom::augment(newdata=newdata, se_fit=TRUE, interval="confidence")
head(lm_3_predict)

#visualize data (this is written wrong for me)
ggplot() +
  geom_point(data=penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = species, color = NULL), alpha = 0.2) +
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))

#interaction term

lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm:species,
          data=penguins_lm_3)

lm_4 = lm(bill_depth_mm ~ bill_length_mm * species,
          data=penguins_lm_3) ##this method is better
summary(lm_4)

AIC(lm_3, lm_4) ##model comparison
best_model = step(lm_4)

#plot with interaction
lm_4_predict = lm_4 %>%
  broom::augment(interval="confidence")
head(lm_4_predict)


ggplot(data=lm_4_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(y=.fitted, x=bill_length_mm, color=species)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), alpha=0.5)

# depth ~ bill_length + flipper_length
library(carData) #vif()

gentoo = penguins %>%
  filter(species=="Gentoo")

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_4)
step(lm_gentoo_3)
vif(lm_gentoo_3) # multi-colinearaity

#visualize this model
head(penguins_lm_3)

newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE),
         body_mass_g = median(gentoo$body_mass_g, na.rm=TRUE)) #create new column

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")

ggplot(data=lm_gentoo_3_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=bill_length_mm)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm),alpha=0.5) +
  annotate("text", x=57, y=13.5, label=paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm=TRUE), " mm")) +
  annotate("text", x=57, y=13.75, label=paste0("bill length = ", median(gentoo$bill_length_mm, na.rm=TRUE), " mm"))


#### exercise 5.3

newdata2 = gentoo %>%
  select(bill_depth_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE),
         body_mass_g = median(gentoo$body_mass_g, nax.rm=TRUE)) 

lm_gentoo_3_predict2 = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")

ggplot(data=lm_gentoo_3_predict) +
  geom_point(aes(y=bill_depth_mm, x=flipper_length_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=flipper_length_mm)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=flipper_length_mm),alpha=0.5) 
  annotate("text", x=57, y=13.5, label=paste0("flipper length = ", median(gentoo$bill_length_mm, na.rm=TRUE), " mm")) 
  annotate("text", x=57, y=13.75, label=paste0("bill length = ", median(gentoo$body_mass_g, na.rm=TRUE), " mm"))
  

  
  # ANOVA
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
anova(penguin_lm)

penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
 summary(penguin_anova)

