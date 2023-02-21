# Haley Durbin
# 2023-02-21

library(palmerpenguins)
library(rstatix) # for levene_test
library(knitr)  # prints pretty tables with RMarkdown
library(tidyverse)


####### One-sample t-test ##########

head(penguins)
ggplot(data=penguins) +
  geom_histogram(aes(x=body_mass_g, fill=species))

gentoo = penguins %>%
  filter(species=="Gentoo")
head(gentoo)

ggplot(data=gentoo) +
  geom_histogram(aes(x=body_mass_g))

ggplot(data=gentoo) +
  stat_qq(aes(sample=body_mass_g))

gentoo %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE),
            sd_body_mass_g = sd(body_mass_g, na.rm=TRUE))


###### run t-test. (are the means similar... a p-value less than 0.05 means that they are different)

t.test(gentoo$body_mass_g, mu=5500)

t_test_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu=5500)
t_test_results




######## two-sample t-test
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()


summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g),
            sd = sd(body_mass_g))
ggplot(data=data_for_t_test) +
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species, scales="free")

# Check equality of variances; Levene's test null hypothesis: variances are equal
data_for_t_test %>% 
  levene_test(body_mass_g ~ species) # if p<0.05, variances are NOT equal

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal=TRUE) #automatically uses Welch's test, so we have to tell it to use Lavene's test by confirming that the variances are equal


########### CORRELATIONS ###############

gentoo = penguins %>% 
  filter(species=="Gentoo")

ggplot(data=gentoo)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))

ggplot(data=gentoo) +
  stat_qq(aes(sample=bill_length_mm))

ggplot(data=gentoo) +
  stat_qq(aes(sample=bill_depth_mm))

cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs") #use means to only give us complete observations and not NAs

cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

head(gentoo)

cor(gentoo[ ,c(3:6)], use="complete.obs")

library(GGally)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs(aes(color=species))

