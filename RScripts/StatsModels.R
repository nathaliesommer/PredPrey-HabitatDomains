# Stats models
# FER
# Dec 2021


# load libraries
library(lme4)
library(rstan)
library(rstanarm)
library(ggplot2)
library(bayesplot)
library(viridis)
library(loo)
library(lme4)
library(bayestestR)
library(ggridges)
library(tidybayes)
library(dplyr)
library(modelr)
library(shinystan)

# load data
oneyr <- read_csv("Data/NCvsC_1year_TSH_Nov29.csv")
fiveyr <- read_csv("Data/NCvsC_5year_TSH_Nov29.csv")
oneyr_null <- read_csv("Data/NCvsC_Nullyear_TSH_Nov19.csv")
fiveyr_null <- read_csv("Data/NCvsC_NULL_5year_TSH_Nov29.csv")
# use df data from NCvsC_5years_Space_Time_Habitat

# separate predator strategies
active <- subset(data, Pred.Strat == "Active")

# hunting mode and starting conditions as factors
data$Pred.Strat <- as.factor(data$Pred.Strat)
data$Prey.Start.Con <- as.factor(data$Prey.Start.Con)
data$Pred.Start.Con <- as.factor(data$Pred.Start.Con)
str(data)

# binomial model for black vs. white in frequentist first for fun ----
binom_mod <- glm(cbind(Black.Table, White.Table) ~ Pred.Strat + Pred.Start.Con*Prey.Start.Con, 
                 family = binomial("logit"), data = oneyr)
summary(binom_mod)
plot(binom_mod)

# calculate shifts ----
# space shifts
oneyr$propW <- oneyr$White.Table/(oneyr$Black.Table + oneyr$White.Table)
oneyr_null$propW <- oneyr_null$White.Table/(oneyr_null$Black.Table + oneyr_null$White.Table)
fiveyr$propW <- fiveyr$White.Table/(fiveyr$Black.Table + fiveyr$White.Table)
fiveyr_null$propW <- fiveyr_null$White.Table/(fiveyr_null$Black.Table + fiveyr_null$White.Table)

# time shifts
# proportion of time spent in predator free hrs 13:00-01:00
# sum all time active
oneyr$SumTime <- apply(oneyr[,c(15:38)], 1, sum)
oneyr$PredOverlap <- apply(oneyr[,c(17:27)], 1, sum)
oneyr$propPredOverlap <- oneyr$PredOverlap/oneyr$SumTime

oneyr_null$SumTime <- apply(oneyr_null[,c(15:38)], 1, sum)
oneyr_null$PredOverlap <- apply(oneyr_null[,c(17:27)], 1, sum)
oneyr_null$propPredOverlap <- oneyr_null$PredOverlap/oneyr_null$SumTime

fiveyr$SumTime <- apply(fiveyr[,c(15:38)], 1, sum)
fiveyr$PredOverlap <- apply(fiveyr[,c(17:27)], 1, sum)
fiveyr$propPredOverlap <- fiveyr$PredOverlap/fiveyr$SumTime

fiveyr_null$SumTime <- apply(fiveyr_null[,c(15:38)], 1, sum)
fiveyr_null$PredOverlap <- apply(fiveyr_null[,c(17:27)], 1, sum)
fiveyr_null$propPredOverlap <- fiveyr_null$PredOverlap/fiveyr_null$SumTime

## the proportions of proportions
# let's go!
oneyr$shifts <- oneyr$propW/oneyr$propPredOverlap
oneyr_null$shifts <- oneyr_null$propW/oneyr_null$propPredOverlap
fiveyr$shifts <- fiveyr$propW/fiveyr$propPredOverlap
fiveyr_null$shifts <- fiveyr_null$propW/fiveyr_null$propPredOverlap


# switch to Bayesian ----
# models to look at proportional shifts in space:time use

# One year null model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
onenullmod <- stan_glm(shifts ~ 0 + Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                     data = oneyr_null,
                     prior = t_prior,
                     cores = 2,
                     seed = 12345,
                     iter = 4000,
                     family = gaussian)

round(posterior_interval(onenullmod, prob = 0.95), 3)
loo(onenullmod) # fit is good
summary(onenullmod, digits = 3)

# get posterior summary
describe_posterior(
  onenullmod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(onenullmod)

# plot of posteriors
posterior <- as.matrix(onenullmod)

# plot it
plot_title <- ggtitle("One-year null model posterior distributions",
                      "with medians and 95% intervals")
oneyearnull_plot <- mcmc_areas(posterior,
                           pars = c("Pred.StratActive",
                                    "Pred.StratSit-and-Pursue",
                                    "Pred.StratSit-and-Wait",
                                    "Prey.Start.ConSmall",
                                    "Pred.Start.ConSmall",
                                    "Pred.StratSit-and-Pursue:Prey.Start.ConSmall",
                                    "Pred.StratSit-and-Wait:Prey.Start.ConSmall",
                                    "Pred.StratSit-and-Pursue:Pred.Start.ConSmall",
                                    "Pred.StratSit-and-Wait:Pred.Start.ConSmall",
                                    "Prey.Start.ConSmall:Pred.Start.ConSmall",
                                    "Pred.StratSit-and-Pursue:Prey.Start.ConSmall:Pred.Start.ConSmall",
                                    "Pred.StratSit-and-Wait:Prey.Start.ConSmall:Pred.Start.ConSmall"),
                           prob = 0.95) + 
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")

# Five-year null model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fivenullmod <- stan_glm(shifts ~ 0 + Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                       data = fiveyr_null,
                       prior = t_prior,
                       cores = 2,
                       seed = 12345,
                       iter = 4000,
                       family = gaussian)

round(posterior_interval(fivenullmod, prob = 0.95), 3)
loo(fivenullmod) # fit is good
summary(fivenullmod, digits = 3)

# get posterior summary
describe_posterior(
  fivenullmod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(fivenullmod)

# plot of posteriors
posterior <- as.matrix(fivenullmod)

# plot it
plot_title <- ggtitle("Five-year null model posterior distributions",
                      "with medians and 95% intervals")
fiveyearnull_plot <- mcmc_areas(posterior,
                               pars = c("Pred.StratActive",
                                        "Pred.StratSit-and-Pursue",
                                        "Pred.StratSit-and-Wait",
                                        "Prey.Start.ConSmall",
                                        "Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Prey.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Prey.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Pred.Start.ConSmall",
                                        "Prey.Start.ConSmall:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Prey.Start.ConSmall:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Prey.Start.ConSmall:Pred.Start.ConSmall"),
                               prob = 0.95) + 
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")




# One year real deal model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
onemod <- stan_glm(shifts ~ 0 + Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                       data = oneyr,
                       prior = t_prior,
                       cores = 2,
                       seed = 12345,
                       iter = 4000,
                       family = gaussian)

round(posterior_interval(onemod, prob = 0.95), 3)
loo(onemod) # fit is good
summary(onemod, digits = 3)

# get posterior summary
describe_posterior(
  onemod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(onemod)

# plot of posteriors
posterior <- as.matrix(onemod)

# plot it
plot_title <- ggtitle("One-year model posterior distributions",
                      "with medians and 95% intervals")
oneyear_plot <- mcmc_areas(posterior,
                               pars = c("Pred.StratActive",
                                        "Pred.StratSit-and-Pursue",
                                        "Pred.StratSit-and-Wait",
                                        "Prey.Start.ConSmall",
                                        "Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Prey.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Prey.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Pred.Start.ConSmall",
                                        "Prey.Start.ConSmall:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Pursue:Prey.Start.ConSmall:Pred.Start.ConSmall",
                                        "Pred.StratSit-and-Wait:Prey.Start.ConSmall:Pred.Start.ConSmall"),
                               prob = 0.95) + 
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")





# Five-year model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fivemod <- stan_glm(shifts ~ 0 + Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                        data = fiveyr,
                        prior = t_prior,
                        cores = 2,
                        seed = 12345,
                        iter = 4000,
                        family = gaussian)

round(posterior_interval(fivemod, prob = 0.95), 3)
loo(fivemod) # fit is good
summary(fivemod, digits = 3)

# get posterior summary
describe_posterior(
  fivemod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(fivemod)

# plot of posteriors
posterior <- as.matrix(fivemod)

# plot it
plot_title <- ggtitle("Five-year model posterior distributions",
                      "with medians and 95% intervals")
fiveyear_plot <- mcmc_areas(posterior,
                                pars = c("Pred.StratActive",
                                         "Pred.StratSit-and-Pursue",
                                         "Pred.StratSit-and-Wait",
                                         "Prey.Start.ConSmall",
                                         "Pred.Start.ConSmall",
                                         "Pred.StratSit-and-Pursue:Prey.Start.ConSmall",
                                         "Pred.StratSit-and-Wait:Prey.Start.ConSmall",
                                         "Pred.StratSit-and-Pursue:Pred.Start.ConSmall",
                                         "Pred.StratSit-and-Wait:Pred.Start.ConSmall",
                                         "Prey.Start.ConSmall:Pred.Start.ConSmall",
                                         "Pred.StratSit-and-Pursue:Prey.Start.ConSmall:Pred.Start.ConSmall",
                                         "Pred.StratSit-and-Wait:Prey.Start.ConSmall:Pred.Start.ConSmall"),
                                prob = 0.95) + 
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")


