
# Exercise 1

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(
    aes(
      x = body_mass_g,
      fill = species
    ) 
  ) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~sex, nrow = 2) + 
  labs(
    x = "Body Mass (g)",
    y = "",
    fill = "Species"
  )

# Exercise 2

ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = bill_length_mm)
) +
  geom_point(
    aes(color = species, shape = species), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(
    title = "Flipper and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
    x = "Flipper length (mm)",
    y = "Bill length (mm)",
    color = "Penguin species",
    shape = "Penguin species"
  ) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, face= "italic"),
    plot.caption.position = "plot"
  )
