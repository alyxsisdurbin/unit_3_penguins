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
