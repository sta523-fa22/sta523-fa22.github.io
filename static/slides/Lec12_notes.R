library(rvest)
library(tidyverse)

## Example
url = "https://www.rottentomatoes.com/"
polite::bow(url)

page = read_html(url)

movies = tibble::tibble(
  title = page %>% 
    html_elements(".ordered-layout__list--score:nth-child(1) .clamp-1") %>%
    html_text2(),
  tomatometer = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .b--medium") %>%
    html_text2() %>%
    str_remove("%$") %>%
    as.numeric() %>%
    {./100},
  status = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .icon--tiny") %>%
    html_attr("class") %>%
    str_remove("icon ") %>%
    str_remove("icon--tiny ") %>%
    str_remove("icon__"),
  url = page %>% 
    html_elements(".ordered-layout__list--score:nth-child(1) a:nth-child(1)") %>%
    html_attr("href") %>%
    paste0("https://www.rottentomatoes.com/", .)
)


## Exercise 1

scrape_movie_page = function(url) {
  page = read_html(url)
  
  list(
    n_reviews = page %>% 
      html_elements(".scoreboard__link--tomatometer") %>%
      html_text2() %>%
      str_remove(" Reviews") %>%
      as.integer(),
    aud_ratings = page %>%
      html_elements(".scoreboard__link--audience") %>%
      html_text2() %>%
      str_remove(" Ratings"),
    run_time = page %>%
      html_elements(".scoreboard__info") %>%
      html_text2() %>%
      str_split(", ", simplify = TRUE) %>%
      {.[,3]}
  )
}

movies = movies %>%
  mutate(
    details = purrr::map(url, scrape_movie_page)
  ) %>%
  unnest_wider(details)


## Exercise 1 - Bonus - demo inspector and shutting off javascript


