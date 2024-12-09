# install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
# devtools::install_github("rmcelreath/rethinking@slim")


library(rethinking)


# Functions ---------------------------------------------------------------


plot_priors <- function(fit_model, total_lines = 50) {
  prior <- extract.prior(fit_model)
  
  mu <- link(fit_model, post=prior, data=list(sc_age = c(-2, 2)))
  
  # lapply(prior, head)
  # head(mu, 20)
  
  plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
  for (i in 1:total_lines) {
    lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))
  }
  
}

plot_posteriors <- function(fit_model, total_lines = 50) {
  age_values <- seq(-3, 3.2, length.out = 30) 
  mu <- link(fit_model, data = list(sc_age = age_values))
  mu_mean <- apply(mu, 2, mean)
  mu_PI <- apply(mu, 2, PI)
  
  plot(sc_divorce ~  sc_age, data = df_divorce, col=rangi2)
  lines(age_values, mu_mean, lwd = 2)
  shade(mu_PI, age_values)
  
}


# Data --------------------------------------------------------------------

data("WaffleDivorce")

df_divorce <- WaffleDivorce

df_divorce$sc_divorce <- scale(df_divorce$Divorce)
df_divorce$sc_age <- scale(df_divorce$MedianAgeMarriage)
df_divorce$sc_marriage <- scale(df_divorce$Marriage)


# Fit Model ---------------------------------------------------------------

m5.1 <- quap(
  alist(
    sc_divorce ~ dnorm(mu, sigma), 
    mu <- a + bA * sc_age, 
    a ~  dnorm(0, 0.2), 
    bA ~  dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), 
  data = df_divorce
)


# Extract -----------------------------------------------------------------

set.seed(7)
plot_priors(m5.1)
plot_posteriors(m5.1)


# prior <- extract.prior(m5.1)
# mu <- link(m5.1, post=prior, data=list(sc_age = c(-2, 2)))
# # lapply(prior, head)
# # head(mu, 20)
# 
# age_values <- seq(-3, 3.2, length.out = 30) 
# mu <- link(m5.1, data = list(sc_age = age_values))
# mu_mean <- apply(mu, 2, mean) # mean of mu for each age value
# mu_PI <- apply(mu, 2, PI)

# Bootstrap Regression ----------------------------------------------------
plot(sc_divorce ~  sc_age, data = df_divorce)
for (bs in 1:100) {
  
  filas_rnd <- sample(1:nrow(df_divorce), size = nrow(df_divorce), replace = TRUE)
  df_bs <- df_divorce[filas_rnd, ]
  
  fit_det <- lm(sc_divorce ~  sc_age, data = df_bs)
  coefs <- coef(fit_det)
  
  divorcios <- coefs[1] + coefs[2] * df_bs$sc_age
  lines(df_bs$sc_age, divorcios, col = col.alpha("black", 0.1))
}


# Exercise ----------------------------------------------------------------

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
df <- data.frame(y, x_real, x_spur)

pairs(df)
cor(df$y, df$x_spur)

fit_xxy <- quap(
  alist(
    y ~ dnorm(mu, sigma), 
    mu <- a + bXr * x_real + bXs * x_spur, 
    a ~ dnorm(0, 0.2), 
    bXr ~ dnorm(1, 0.5), 
    bXs ~ dnorm(1, 0.5), 
    sigma ~ dexp(1)
    
  ), 
  data = df
)

precis(fit_xxy)


