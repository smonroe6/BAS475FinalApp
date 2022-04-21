souvenirs %>% 
  autoplot(Sales)


souvenirs %>%
  select(Sales) %>%
  ACF() %>%
  autoplot()

souvenirs %>%
  select(Sales) %>%
  gg_season()

souvenirs %>%
  model(
    classical_decomposition(Sales, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot()

souvenirs %>%
  model(
    MEAN()
    ) %>%
  autoplot()

sales <- souvenirs %>%
  filter_index("1987 Jan" ~ "1991 Dec") %>%
  select(Sales)

sales_fit <- sales %>%
  model(
    Mean = MEAN(Sales),
    `Naïve` = NAIVE(Sales),
    `Seasonal naïve` = SNAIVE(Sales)
  )
# Generate forecasts for 14 quarters
sales_fc <- sales_fit %>% forecast(h = 24)
# Plot forecasts against actual values
sales_fc %>%
  autoplot(sales, level = NULL) +
  autolayer(
    filter_index(souvenirs, "1992 Jan" ~ "1993 Dec"),
    colour = "black"
  ) +
  labs(
    y = "Australian Dollars",
    title = "Forecasts for monthly souvenir sales"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

sales %>% model(MEAN(Sales))
