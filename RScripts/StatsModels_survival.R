# Stats models for binomial survival models
# FER
# Last update 6 Jan 2022

# function to install and load packages
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

# separate predator strategies
active1 <- subset(oneyr, Pred.Strat == "Active")
sitwait1 <- subset(oneyr, Pred.Strat == "Sit-and-Wait")
pursue1 <- subset(oneyr, Pred.Strat == "Sit-and-Pursue")

active5 <- subset(fiveyr, Pred.Strat == "Active")
sitwait5 <- subset(fiveyr, Pred.Strat == "Sit-and-Wait")
pursue5 <- subset(fiveyr, Pred.Strat == "Sit-and-Pursue")




# switch to Bayesian ----
# models to look at survival (ticks)
# one yr = 8760 ticks, so create column for number of ticks not survived
oneyr_null$dead <- 8760 - oneyr_null$ticks


# One year null model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
onenullmod <- stan_glm(cbind(ticks, dead) ~ Pred.Strat*Prey.Start.Con*Pred.Start.Con,
                       data = oneyr_null,
                       prior = t_prior,
                       cores = 2,
                       seed = 12345,
                       iter = 2000,
                       family = binomial)

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
                               prob = 0.95)    +
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

