# Stats models for proportional shifts
# FER
# Last update Jan 2022

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Packages that I want
packages <- c("remotes",
              "tidyr",
              "tidyverse",
              "reshape2",
              "ggridges",
              "lme4",
              "rstan",
              "rstanarm",
              "shinystan",
              "bayesplot",
              "loo",
              "bayestestR",
              "ggridges",
              "tidybayes",
              "modelr",
              "shinystan",
              "dplyr")

#Run the ipak loop
ipak(packages)



# load data ----
oneyr <- read_csv("Data/NCvsC_1year_TSH_Nov29.csv")
fiveyr <- read_csv("Data/NCvsC_5year_TSH_Nov29.csv")
oneyr_null <- read_csv("Data/NCvsC_Nullyear_TSH_Nov19.csv")

fiveyr_null <- read_csv("Data/NCvsC_NULL_5year_TSH_Nov29.csv")
# use df data from NCvsC_5years_Space_Time_Habitat

# convert to factors ----
# hunting mode and starting conditions as factors
oneyr$Pred.Strat <- as.factor(oneyr$Pred.Strat)
oneyr$Prey.Start.Con <- as.factor(oneyr$Prey.Start.Con)
oneyr$Pred.Start.Con <- as.factor(oneyr$Pred.Start.Con)

oneyr_null$Pred.Strat <- as.factor(oneyr_null$Pred.Strat)
oneyr_null$Prey.Start.Con <- as.factor(oneyr_null$Prey.Start.Con)
oneyr_null$Pred.Start.Con <- as.factor(oneyr_null$Pred.Start.Con)

fiveyr$Pred.Strat <- as.factor(fiveyr$Pred.Strat)
fiveyr$Prey.Start.Con <- as.factor(fiveyr$Prey.Start.Con)
fiveyr$Pred.Start.Con <- as.factor(fiveyr$Pred.Start.Con)

fiveyr_null$Pred.Strat <- as.factor(fiveyr_null$Pred.Strat)
fiveyr_null$Prey.Start.Con <- as.factor(fiveyr_null$Prey.Start.Con)
fiveyr_null$Pred.Start.Con <- as.factor(fiveyr_null$Pred.Start.Con)


# calculate shifts ----
# Habitat shifts - White.Table has lower detectability
oneyr$propW <- oneyr$White.Table/(oneyr$Black.Table + oneyr$White.Table)
oneyr_null$propW <- oneyr_null$White.Table/(oneyr_null$Black.Table + oneyr_null$White.Table)
fiveyr$propW <- fiveyr$White.Table/(fiveyr$Black.Table + fiveyr$White.Table)
fiveyr_null$propW <- fiveyr_null$White.Table/(fiveyr_null$Black.Table + fiveyr_null$White.Table)

# Space shift: overlap with predator in space
# Domain.Prey is where only prey are found (no predators)
oneyr$propW <- oneyr$Domain.Prey/(oneyr$DomainOverlap + oneyr$Domain.Prey)
oneyr_null$propW <- oneyr_null$Domain.Prey/(oneyr_null$DomainOverlap + oneyr_null$Domain.Prey)
fiveyr$propW <- fiveyr$Domain.Prey/(fiveyr$DomainOverlap + fiveyr$Domain.Prey)
fiveyr_null$propW <- fiveyr_null$Domain.Prey/(fiveyr_null$DomainOverlap + fiveyr_null$Domain.Prey)

# time shifts
# proportion of time spent in predator free hrs 0-11
# sum all time active
oneyr$SumTime <- apply(oneyr[,c(16:39)], 1, sum)
oneyr$PredOverlap <- apply(oneyr[,c(16:27)], 1, sum)
oneyr$propPredFree <- 1 - oneyr$PredOverlap/oneyr$SumTime

oneyr_null$SumTime <- apply(oneyr_null[,c(16:39)], 1, sum)
oneyr_null$PredOverlap <- apply(oneyr_null[,c(16:27)], 1, sum)
oneyr_null$propPredFree <- 1 - oneyr_null$PredOverlap/oneyr_null$SumTime

fiveyr$SumTime <- apply(fiveyr[,c(16:39)], 1, sum)
fiveyr$PredOverlap <- apply(fiveyr[,c(16:27)], 1, sum)
fiveyr$propPredFree <- 1 - fiveyr$PredOverlap/fiveyr$SumTime

fiveyr_null$SumTime <- apply(fiveyr_null[,c(16:39)], 1, sum)
fiveyr_null$PredOverlap <- apply(fiveyr_null[,c(16:27)], 1, sum)
fiveyr_null$propPredFree <- 1 - fiveyr_null$PredOverlap/fiveyr_null$SumTime

## the proportions of proportions
# let's go!
oneyr$shifts <- oneyr$propW/oneyr$propPredFree
oneyr_null$shifts <- oneyr_null$propW/oneyr_null$propPredFree
fiveyr$shifts <- fiveyr$propW/fiveyr$propPredFree
fiveyr_null$shifts <- fiveyr_null$propW/fiveyr_null$propPredFree


# separate predator strategies
active1 <- subset(oneyr, Pred.Strat == "Active")
sitwait1 <- subset(oneyr, Pred.Strat == "Sit-and-Wait")
pursue1 <- subset(oneyr, Pred.Strat == "Sit-and-Pursue")

active5 <- subset(fiveyr, Pred.Strat == "Active")
sitwait5 <- subset(fiveyr, Pred.Strat == "Sit-and-Wait")
pursue5 <- subset(fiveyr, Pred.Strat == "Sit-and-Pursue")




# switch to Bayesian ----
# models to look at proportional shifts in space:time use


# One year null model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
onenullmod <- stan_glm(shifts ~ 0 + Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                     data = oneyr_null,
                     prior = t_prior,
                     cores = 2,
                     seed = 12345,
                     iter = 2000,
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
plot_title <- ggtitle("One-year null model for shifts posterior distributions",
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
  scale_y_discrete(labels = c('Active predators',
                              'Sit-and-Pursue predators',
                              'Sit-and-Wait predators',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Sit-and-Pursue x Prey small habitiat',
                              'Sit-and-Wait x Prey small habitat',
                              'Sit-and-Pursue x Predator small habitat',
                              'Sit-and_Wait x Predator small habitat',
                              'Predator small habitat x Prey small habitat',
                              'Sit-and-Pursue x Prey small habitat x Predator small habitat',
                              'Sit-and-Wait x Prey small habitat x Predator small habitat'
                                                                 )) +
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
  scale_y_discrete(labels = c('Active predators',
                              'Sit-and-Pursue predators',
                              'Sit-and-Wait predators',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Sit-and-Pursue x Prey small habitiat',
                              'Sit-and-Wait x Prey small habitat',
                              'Sit-and-Pursue x Predator small habitat',
                              'Sit-and_Wait x Predator small habitat',
                              'Predator small habitat x Prey small habitat',
                              'Sit-and-Pursue x Prey small habitat x Predator small habitat',
                              'Sit-and-Wait x Prey small habitat x Predator small habitat'
  )) +
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
  scale_y_discrete(labels = c('Active predators',
                              'Sit-and-Pursue predators',
                              'Sit-and-Wait predators',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Sit-and-Pursue x Prey small habitiat',
                              'Sit-and-Wait x Prey small habitat',
                              'Sit-and-Pursue x Predator small habitat',
                              'Sit-and_Wait x Predator small habitat',
                              'Predator small habitat x Prey small habitat',
                              'Sit-and-Pursue x Prey small habitat x Predator small habitat',
                              'Sit-and-Wait x Prey small habitat x Predator small habitat'
  )) +
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
  scale_y_discrete(labels = c('Active predators',
                              'Sit-and-Pursue predators',
                              'Sit-and-Wait predators',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Sit-and-Pursue x Prey small habitiat',
                              'Sit-and-Wait x Prey small habitat',
                              'Sit-and-Pursue x Predator small habitat',
                              'Sit-and_Wait x Predator small habitat',
                              'Predator small habitat x Prey small habitat',
                              'Sit-and-Pursue x Prey small habitat x Predator small habitat',
                              'Sit-and-Wait x Prey small habitat x Predator small habitat'
  )) +
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")





# Models by predator strategy (one-yr) ----
# active predator time shifts ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
active1timemod <- stan_glm(propPredFree ~ Prey.Start.Con*Pred.Start.Con,
                    data = active1,
                    prior = t_prior,
                    cores = 2,
                    seed = 12345,
                    iter = 4000,
                    family = gaussian)

round(posterior_interval(active1timemod, prob = 0.95), 3)
loo(active1timemod) # fit is good
summary(active1timemod, digits = 3)

# get posterior summary
describe_posterior(
  active1timemod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(active1timemod)

# plot of posteriors
posterior <- as.matrix(active1timemod)

# plot it
plot_title <- ggtitle("One-year model time shifts",
                      "active predators")
active1time_plot <- mcmc_areas(posterior,
                            pars = c("(Intercept)",
                                     "Prey.Start.ConSmall",
                                     "Pred.Start.ConSmall",
                                     "Prey.Start.ConSmall:Pred.Start.ConSmall"), 
                            prob = 0.95) + 
  scale_y_discrete(labels = c('Baseline overlap',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Predator small habitat x Prey small habitat'
  )) +
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")


# sit-and-pursue predator time shifts ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
pursue1timemod <- stan_glm(propPredFree ~ Prey.Start.Con*Pred.Start.Con,
                           data = pursue1,
                           prior = t_prior,
                           cores = 2,
                           seed = 12345,
                           iter = 4000,
                           family = gaussian)

round(posterior_interval(pursue1timemod, prob = 0.95), 3)
loo(pursue1timemod) # fit is good
summary(pursue1timemod, digits = 3)

# get posterior summary
describe_posterior(
  pursue1timemod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(pursue1timemod)

# plot of posteriors
posterior <- as.matrix(pursue1timemod)

# plot it
plot_title <- ggtitle("One-year model time shifts",
                      "sit-and-pursue predators")
pursue1time_plot <- mcmc_areas(posterior,
                               pars = c("(Intercept)",
                                        "Prey.Start.ConSmall",
                                        "Pred.Start.ConSmall",
                                        "Prey.Start.ConSmall:Pred.Start.ConSmall"), 
                               prob = 0.95) + 
  scale_y_discrete(labels = c('Baseline overlap',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Predator small habitat x Prey small habitat'
  )) +
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")


# sit-and-wait predator time shifts ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
sitwait1timemod <- stan_glm(propPredFree ~ Prey.Start.Con*Pred.Start.Con,
                           data = sitwait1,
                           prior = t_prior,
                           cores = 2,
                           seed = 12345,
                           iter = 4000,
                           family = gaussian)

round(posterior_interval(sitwait1timemod, prob = 0.95), 3)
loo(sitwait1timemod, k_threshold = 0.7) # fit is good
summary(sitwait1timemod, digits = 3)

# get posterior summary
describe_posterior(
  sitwait1timemod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# look at model fit in Shiny
shinystan::launch_shinystan(sitwait1timemod)

# plot of posteriors
posterior <- as.matrix(sitwait1timemod)

# plot it
plot_title <- ggtitle("One-year model time shifts",
                      "sit-and-wait predators")
pursue1time_plot <- mcmc_areas(posterior,
                               pars = c("(Intercept)",
                                        "Prey.Start.ConSmall",
                                        "Pred.Start.ConSmall",
                                        "Prey.Start.ConSmall:Pred.Start.ConSmall"), 
                               prob = 0.95) + 
  scale_y_discrete(labels = c('Baseline overlap',
                              'Prey small habitat',
                              'Predator small habitat',
                              'Predator small habitat x Prey small habitat'
  )) +
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red")