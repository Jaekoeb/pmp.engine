
#---------------------------------------------------------------------------
# This Demo should illustrate how one can
# do a portfolio analysis of our portfolio
# by Jakob



# Libraries and Data ------------------------------------------------------

# Load our package and tidyverse because thats always useful
library(pmp.engine)
library(tidyverse)
library(xts)
library(PerformanceAnalytics)

# Load the data here
# In this case I load the demo data we have inside the package
data("port_id")
data("port_price")

id <- port_id
price <- port_price
rm(port_id, port_price)



# Data Cleaning -----------------------------------------------------------


# First we will exclude all currencies
# These cash positions dont account for interest and thus dont make sense to analyze
price <- price |>
  filter(!(id %in% c(
    "TRY Curncy",
    "USD Curncy",
    "ZAR Curncy",
    "BOP001GLSY7 Curncy"
  )))


id <- id |>
  filter(!(id %in% c(
    "TRY Curncy",
    "USD Curncy",
    "ZAR Curncy",
    "BOP001GLSY7 Curncy"
  )))


# Now readjust the weights to sum to 1
id <- id |> mutate(weight = weight / sum(weight))



# Next we take a look at the NAs
plot_price_NAs(price)

# We see that we have a lot of missings,
# let us remove the two assets that have a lot of missings and
# forward fill the rest

price <- price |>
  filter(!(id %in% c(
    "AX613951     Corp",
    "TT333904     Corp"
  )))


id <- id |>
  filter(!(id %in% c(
    "AX613951     Corp",
    "TT333904     Corp"
  )))


# Again let us readjust the weights
id <- id |> mutate(weight = weight / sum(weight))


# Now we fill forward the prices
price <- fill_price_NAs(price,
                        col.date = date,
                        col.id = id,
                        col.px = px,
                        method = "forward")


# Now let us plot the NAs again
plot_price_NAs(price)
# We see that the NAs are gone




# Finally we manually change the sectors
# The default BBG sectors are not the best
id$mkt_sector <- c(
  "Bond",
  "Equity",
  "Equity",
  "Bond",
  "Equity",
  "Bond",
  "Alternative",
  "Equity",
  "Bond",
  "Bond",
  "Bond",
  "Bond",
  "Bond"
)

# Compute Returns ---------------------------------------------------------

# compute weekly returns
return <- security_returns(price)

# remove first day
return <- na.omit(return)


# norm prices
price <- price |>
  group_by(id) |>
  mutate(px = 100 * px / first(px)) |>
  ungroup()



# Aggregating Returns -----------------------------------------------------


# Next we want to analyze returns for a given sector
# We will aggregate by currency and by asset class

# compute aggregated returns
sector <- aggregate_returns(return, id)
currency <- aggregate_returns(return, id, aggregate.by = crncy)


# compute prices
sector_price <- sector |>
  group_by(mkt_sector) |>
  arrange(date) |>
  mutate(px = 100 * cumprod(1 + ret) / first(1 + ret)) |>
  ungroup() |>
  select(-ret)

sector_price$date <- as.Date(sector_price$date)


# compute prices
currency_price <- currency |>
  group_by(crncy) |>
  arrange(date) |>
  mutate(px = 100 * cumprod(1 + ret) / first(1 + ret)) |>
  ungroup() |>
  select(-ret)

currency_price$date <- as.Date(currency_price$date)




# Basic Performance Analytics ---------------------------------------------

# convert to an xts object
stats <- return |>
  pivot_wider(names_from = id, values_from = ret)

stats$date <- as.Date(stats$date)

stats <- xts(stats[, -1], order.by = stats$date)


# Compute desired perfomance statistics
# the package `PerformanceAnalytics` offers many different functions for that
# Here we have weekly return data so sometimes we have
# to scale for annualized values


stats <- rbind(
  Return.annualized(stats),
  StdDev.annualized(stats),
  SharpeRatio.annualized(stats, Rf = 0.05 / 52),
  SemiDeviation(stats) * sqrt(52),
  maxDrawdown(stats),
  VaR(stats)
)


# Equivalently one can do the perfomance analytics for
# the aggregated returns as well



# Risk Decomposition ------------------------------------------------------


## Volatility --------------------------------------------------------------

# Next we can decompose our volatility to see
# how much each position contributes

rsk <- StdDev_decomposition(
  return,
  id
)


## VaR ---------------------------------------------------------------------

# We can do the same for value at risk
Var <- VaR_decomposition(
  return,
  id
)


# From here we have computed many different statistics
# In a next step we should make the numbers understandable
# For this we can visualize, make tables etc.
# Some suggestions:

# - Price Linegraph of all sectors / currencies to see how they performed
# - Table with the basics performance statistics (Sharpe, VaR, etc.)
# - Bar Chart showing how much vola / var each position contributed (or do it for sector / currency)
# - Risk-to-Return plots: Dotplot with x-axis risk (e.g vola contribution)
#   and y-axis performance (e.g annualized return) and color the sector / currency


# Two examples:

gg <- sector_price |>
  ggplot(aes(x = date, y = px, color = `mkt_sector`, group = `mkt_sector`)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Alternative" = "#e72731",
                                "Bond" = "#0f618a",
                                "Equity" = "#38a45e")) +
  theme_bw() +
  labs(
    title = "YTD Performance of our Portfolio",
    x = "",
    y = "",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

plot(gg)


# Usually you would save the plot to your pc using `ggsave`
# ggsave(gg,
#        filename = "result/performance.pdf",
#        units = "cm",
#        height = 12,
#        width = 18)



gg <- data.frame(
  Name = names(rsk$pct_contrib_StdDev),
  Value = rsk$pct_contrib_StdDev
)

# Order the data by decreasing Value
gg <- gg[order(-gg$Value), ]

# Create the bar graph
gg <- ggplot(gg, aes(x = reorder(Name, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  theme_minimal() +
  labs(
    title = "Volatility Contribution",
    x = "Assets",
    y = "Contribution (%)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


plot(gg)
