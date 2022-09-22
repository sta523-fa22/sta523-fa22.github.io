library(tidyverse)

## Example 1 - MC Pi

library(tidyverse)

draw_points = function(n) {
  list(
    x = runif(n, -1, 1),
    y = runif(n, -1, 1)
  )
}

in_unit_circle = function(d) {
  sqrt(d$x^2 + d$y^2) <= 1
}


draw_points(1e5) %>%
  in_unit_circle() %>%
  sum() %>%
  {4 * . / 1e5}


tibble(
  n = 10^(1:6)
) %>%
  mutate(
    draws = purrr::map(n, draw_points),
    n_in_ucirc = purrr::map_int(draws, ~sum(in_unit_circle(.x))),
    pi_approx = 4 * n_in_ucirc / n,
    pi_error = abs(pi - pi_approx)
  ) %>%
  as.data.frame()


## Example 2 - dicog

View(repurrrsive::discog)

tibble(disc = repurrrsive::discog)

tibble(disc = repurrrsive::discog) %>%
  mutate(
    id     = purrr::map_int(disc, "id"),
    year   = purrr::map_int(disc, c("basic_information", "year")),
    title  = purrr::map_chr(disc, c("basic_information", "title")),
    artist = purrr::map_chr(disc, list("basic_information", "artists", 1, "name")),
    label  = purrr::map_chr(disc, list("basic_information", "labels", 1, "name"))
  )


tibble(disc = repurrrsive::discog) %>% 
  hoist(
    disc, 
    id = "id",
    year = c("basic_information", "year"), 
    title = c("basic_information", "title"),
    artist = list("basic_information", "artists", 1, "name"),
    label = list("basic_information", "labels", 1, "name")
  )
