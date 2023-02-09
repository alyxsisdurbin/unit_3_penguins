# Haley Durbin
# 2023-02-09

library("tidyverse")
library("palmerpenguins")

tidyverse_packages() ##will show you all of the functions that downloaded within the packages


# getting a feel an an idea of the data frame
head(penguins)
summary(penguins)
glimpse(penguins) #another way to look at the first bits of data other than head()
class(penguins)

mean(as.data.frame(penguins$bill_depth_mm, na.rm=TRUE))

#filter by species
gentoo = filter(.data=penguins, species=="Gentoo") #we use 2 equals because this a test we are evaluating, not assigning.
head(gentoo)
summary(gentoo)
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
summary(gentoo_ladies)

#pipe changes how we name the first parameter in the pipe
gentoo_ladies = penguins %>%
  filter(species=="Gentoo") %>%
  filter(sex=="female")
summary(gentoo_ladies)


# what is the mean mass of just the female penguins?

mean_ladies_mass = penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g = mean(body_mass_g)) 

mean_ladies_mass = mean(penguins$body_mass_g[penguins$sex=="female"], na.rm=TRUE)

penguins %>%
  group_by(sex) %>%
  summarize(mean_mass_g = mean(body_mass_g))

species_sex_mass = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g))

write_csv(x=species_sex_mass, file="data/peguin_mean_body_mass_g.csv")

species_sex_count = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(count = n())


species_count = penguins %>%
 # filter(!is.na(sex)) %>% #
  group_by(species) %>%
  summarize(count = n())

penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022)

head(penguins_for_america)
glimpse(penguins_for_america)

for_my_advisor = penguins %>%
  distinct(island)

for_my_advisor = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

penguins %>%
  arrange$desc(body_mass_g)

###exersice 1.3 

mean_bill_length_in = penguins %>%
  filter(species=="Adelie",
         island %in% c("Biscoe","Dream"),
          !is.na(bill_length_mm)) %>% #filter by two things
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion: 0.039 inches/mm
  summarize(mean_bill_length_in = mean(bill_length_in), 
            sd_bill_length_in = sd(bill_length_in))
  
  
head(mean_bill_length_in)

mean_bill_length_in_Tor = penguins %>%
  filter(species=="Adelie",
         island=="Torgersen",
         !is.na(bill_length_mm)) %>% #filter by two things
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion: 0.039 inches/mm
  summarize(mean_bill_length_in = mean(bill_length_in), 
            sd_bill_length_in = sd(bill_length_in))


head(mean_bill_length_in_Tor)


mean_bill_length_in_Tor
