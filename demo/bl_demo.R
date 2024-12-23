

# load the sample data
data("bl_data")

# Define the long term target weights
wgt <- c("fi" = 0.4, "equity" = 0.4, "comm" = 0.1, "reales" = 0.1)

# Set the uncertainty parameter
tau <-  0.05

# Set the Risk Aversion parameter
lambda <- 3

# Set the frequency of returns (in this case we have weekly returns)
freq <- 52

# Specify the views
view <- data.frame(
  asset = c("fi", "realest"),
  min = c(0.06, 0.03),
  max = c(0.07, 0.035),
  conf = c(0.8, 0.7)
)

# Take a look at the documentation
?black_litterman

# Run the model
black_litterman(
  returns = bl_data,
  weights = wgt,
  tau = tau,
  view = view,
  freq = freq,
  lambda = lambda,
  col.date = "date",
  col.symb = id,
  col.return = ret
)

