set.seed(3212016)
d = data.frame(x = 1:120) %>%
  mutate(y = sin(2*pi*x/120) + runif(length(x),-1,1))

l = loess(y ~ x, data=d)
p = predict(l, se=TRUE)

d = d %>% mutate(
  pred_y = p$fit,
  pred_y_se = p$se.fit
)

n_rep = 500

bs = purrr::map_dfr(
  seq_len(n_rep),
  function(i) {
    d %>%
      select(x, y) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      mutate(
        .,
        iter = i,
        pred = loess(y ~ x, .) %>% predict()
      )
  }
) %>%
  group_by(x, y) %>%
  summarize(
    bs_low = quantile(pred, probs = 0.025),
    bs_upp = quantile(pred, probs = 0.975),
    .groups = "drop"
  )

ggplot(d, aes(x,y)) +
  geom_point(color="gray50") +
  geom_ribbon(
    aes(ymin = pred_y - 1.96 * pred_y_se, 
        ymax = pred_y + 1.96 * pred_y_se), 
    fill="red", alpha=0.25
  ) +
  geom_line(aes(y=pred_y)) +
  theme_bw() +
  geom_ribbon(
    data = bs,
    aes(ymin = bs_low, ymax = bs_upp),
    color = "blue", alpha = 0.25
  )





future::plan(future::multisession, workers=2)


system.time({

bs_mc = furrr::future_map_dfr(
  seq_len(5000),
  function(i) {
    d %>%
      select(x, y) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      mutate(
        .,
        iter = i,
        pred = loess(y ~ x, .) %>% predict()
      )
  },
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
) %>%
  group_by(x, y) %>%
  summarize(
    bs_low = quantile(pred, probs = 0.025),
    bs_upp = quantile(pred, probs = 0.975),
    .groups = "drop"
  )

})