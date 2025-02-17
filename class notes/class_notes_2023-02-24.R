# Haley Durbin
# 2023-02-14

library("tidyverse")
library("palmerpenguins")


head(penguins)
find("filter")
stats::filter()
dplyr::select()


penguins_without_nas = penguins %>%
  filter(is.na(flipper_lenght_mm)) #not necessary

my_flipper_v_mass_plot = ggplot(data=penguins) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species, shape=sex)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g)) +
  xlab("Flipper length (mm)") +
  ylab("Body Mass (g)") +
  ggtitle("Penguins are cute") +
  theme_bw()
ggsave(my_flipper_v_mass_plot, filename="figures/flipper_v_mass.png", width=7, height=5, units="in", dpi=300)


penguins_ts = penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins = n())

ggplot(data=penguins_ts) +
  geom_line(aes(x=year, y=num_penguins, color=species))


ggplot(penguins) + 
  geom_histogram(aes(x=flipper_length_mm, fill=species, color=species), 
                 position="identity", alpha=0.5) + #alpha = transparency
  scale_fill_manual(values=c("darkorange","darkorchid", "cyan4"))



##### box plot

ggplot(penguins) + 
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) 


### bar charts


ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1)  # Create a new plot for each species, line them all up into 1 row

ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

