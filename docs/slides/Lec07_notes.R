library(tidyverse)

## Example 1

(grades = tibble::tribble(
  ~name, ~hw_1, ~hw_2, ~hw_3, ~hw_4, ~proj_1, ~proj_2,
  "Alice",    19,    19,    18,    20,      89,      95,
  "Bob",      18,    20,    18,    16,      77,      88,
  "Carol",    18,    20,    18,    17,      96,      99,
  "Dave",     19,    19,    18,    19,      86,      82
))

grades %>% 
  pivot_longer(-name, 
    names_to = "assignment", 
    values_to = "score"
  ) %>%
  separate(col = assignment, into = c("type", "number"), sep = "_") %>%
  group_by(name, type) %>%
  summarize(avg_score = sum(score) / n()) %>%
  pivot_wider(id_cols = name, names_from = type, values_from = avg_score) %>%
  mutate(
    overall = 0.5 * (hw/20) + 0.5 * (proj/100)
  )


## Summarize and .groups = "drop"

d = tibble::tribble(
  ~country, ~year,   ~type, ~count,
  "A",  1999, "cases", "0.7K",
  "A",  1999,   "pop",  "19M",
  "A",  2000, "cases",   "2K",
  "A",  2000,   "pop",  "20M",
  "B",  1999, "cases",  "37K",
  "B",  1999,   "pop", "172M",
  "B",  2000, "cases", " 80K",
  "B",  2000,   "pop", "174M",
  "C",  1999, "cases", "212K",
  "C",  1999,   "pop",   "1T",
  "C",  2000, "cases", "213K",
  "C",  2000,   "pop",   "1T"
)

d %>%
  group_by(country, year) %>%
  summarize(
    n = n(),
    .groups = "keep"
  ) %>%
  summarize(
    n = n()
  )


## Exercise 1

palmerpenguins::penguins %>%
  count(island, species) %>%
  pivot_wider(id_cols = island, names_from = species, values_from = n, values_fill = 0)



## Exercise 2

library(repurrrsive)

### Which planet appeared in the most starwars film (according to the data in sw_planet)?

tibble::tibble(planet = sw_planets) %>%
  unnest_wider(planet) %>%
  select(name, url, films) %>%
  unnest_longer(films) %>%
  count(name) %>%
  top_n(2, n)

### Which planet was the homeworld of the most characters in the starwars films?

left_join(
  tibble::tibble(people = sw_people) %>%
    unnest_wider(people) %>%
    select(name, homeworld),
  tibble::tibble(planet = sw_planets) %>%
    unnest_wider(planet) %>%
    select(name, url),
  by = c("homeworld" = "url")
) %>%
  select(
    char_name = name.x,
    homeworld = name.y
  ) %>% 
  count(homeworld) %>%
  top_n(1, n)