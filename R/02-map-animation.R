library(tidyverse)
library(sf)
library(here)
library(ggtext)
library(gganimate)

timezones_with_pop <- read_rds(here("data", "timezones-with-population.rds"))

color_pal <- c("#ADA8B6", "#52FFEE")
names(color_pal) <- c("FALSE", "TRUE")


p <- timezones_with_pop |> 
  ggplot() +
  geom_sf(
    data = select(timezones_with_pop, -tz_difference),
    aes(fill = FALSE),
    color = "grey10", linewidth = 0.1
  ) +
  geom_sf(
    aes(fill = TRUE),
    color = "grey10", linewidth = 0.1
  ) +
  scale_fill_manual(values = color_pal, labels = c("no", "yes")) +
  labs(
    title = "Is it New Year already?",
    subtitle = "Which timezones are <span style='color:#52FFEE'>already in the new year</span>?",
    fill = "Is it the New Year?") +
  theme_void(base_family = "Roboto", base_size = 9) + 
  theme(
    plot.background = element_rect(color = "grey10", fill = "grey10"),
    panel.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = "top",
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    text = element_text(color = "white"),
    plot.title = element_markdown(hjust = 0.5, size = 18),
    plot.subtitle = element_markdown(
      hjust = 0.5, size = 12, margin = margin(t = 6, b = 12)),
    panel.grid = element_line(
      color = "grey80", linewidth = 0.25, linetype = "dotted"),
    legend.key.size = unit(2, "mm")
  ) +
  transition_states(-tz_difference) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.8)

animate(p, res = 150, width = 4, height = 3, units = "in",
                    rewind = FALSE, start_pause = 2, end_pause = 5, bg = "grey10")
anim_save(file.path("media", "new-year.gif"))
