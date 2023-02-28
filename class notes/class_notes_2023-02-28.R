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

ggPredict(lm_3, se=TRUE, interactive=TRUE) #from the giraffe package, se=TRUE makes there a dim standard error model

lm_3_predictions=predict(lm_3, interval="confidence", level=0.95)
head(lm_3_predictions)

penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

ggplot(data=penguins_lm_3_predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species, color=NULL), alpha=0.5) +
  geom_point() +
       geom_line(aes(y=fit)) 

# generate new data
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm),
                             by=0.1)
head(newdata_bill_length_mm)       

newdata = expand.grid(bill_length_mm=newdata_bill_length_mm, 
                      species=unique(penguins_lm_3$species))
newdata_predict_lm_3 = predict(lm_3, newdata=newdata)

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata=newdata, interval="confidence"))

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=newdata+predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, y=bill_depth_mm, fill=species, alpha=0.5))+
  geom_line()

#tidyverse way of generating predictions
lm_3_predict = lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=TRUE, interval="confidence")
glimpse(lm_3_predict)


                      