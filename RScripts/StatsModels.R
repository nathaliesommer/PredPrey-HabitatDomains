# Freya code
# first stats attempt
# 7 Oct 2021

# load libraries
library(lme4)
library(rstanarm)
library(ggplot2)
library(bayesplot)
library(viridis)

# load data
data <- read.csv("dataMay31.csv")


# hunting mode and starting conditions as factors
data$Pred.Strat <- as.factor(data$Pred.Strat)
data$Prey.Start.Con <- as.factor(data$Prey.Start.Con)
data$Pred.Start.Con <- as.factor(data$Pred.Start.Con)
str(data)

# binomial model for black vs. white in frequentist first for fun
binom_mod <- glm(cbind(Black.Table, White.Table) ~ Pred.Strat + Pred.Start.Con + Prey.Start.Con, 
                 family = binomial("logit"), data = data)
summary(binom_mod)

# explore distribution of the data ----
data$propW <- data$White.Table/(data$Black.Table + data$White.Table)
p <- ggplot(data = data, aes(x=Pred.Strat, y=propW, fill=Pred.Strat)) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "grey30") +
  theme_bw(base_size = 16) +
  xlab("Predator strategy") +
  ylab("Prop. time in low detection")
p # pred strategy seems to be the main effect

p2 <- ggplot(data = data, aes(x=Pred.Start.Con, y=propW, fill=Pred.Strat)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d(alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "grey30") +
  xlab("Predator starting condition") +
  ylab("Prop. time in low detection")
p2 # it doesn't look like predator starting condition matters

p3 <- ggplot(data = data, aes(x=Prey.Start.Con, y=propW, fill=Pred.Strat)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d(alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "grey30") +
  xlab("Prey starting condition") +
  ylab("Prop. time in low detection")
p3 # it doesn't look like predator starting condition matters

p4 <- ggplot(data = data, aes(x=Prey.Start.Con, y=propW, fill=Pred.Strat)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d(alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "grey30") +
  xlab("Prey starting condition") +
  ylab("Prop. time in low detection")
p4

# interaction of all three
data$f1f2f3 <- interaction(data$Pred.Strat, data$Pred.Start.Con, data$Prey.Start.Con)
p5 <- ggplot(data = data, aes(x=f1f2f3, y=propW, fill=Pred.Strat)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "grey30") +
  xlab("Interaction") +
  ylab("Prop. time in low detection")
p5



# switch to Bayesian ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(cbind(White.Table, Black.Table) ~ 0 + Pred.Strat*Pred.Start.Con*Prey.Start.Con,
                 data = data,
                 prior = t_prior,
                 cores = 2,
                 seed = 12345,
                 family = binomial(link = "logit"))

round(posterior_interval(fit1, prob = 0.95), 3)

# check model fit
(loo1 <- loo(fit1)) # everything looks good

# look at model fit in Shiny
sso <- shinystan::as.shinystan(fit1, ppd = FALSE)
if (interactive()) launch_shinystan(sso)

# predicted probability as a function of x
# pr_pred <- function(x, est) plogis(ests[1] + ests[2] * x)
# 
# ggplot(data, aes(y = cbind(Black.Table, White.Table), x = Pred.Strat, fill = Pred.Strat)) +
#   geom_point(size = 4, pch = 21) +
#   stat_function(fun = pr_pred, args = list(ests = coef(fit1)),
#                 size = 2, color = "gray25") +
#   theme_bw(base_size = 16)


# time use - explore first through ggridges ----
library(ggridges)
hr0 <- ggplot(data, aes(x = hour0, y = Pred.Strat, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

hr1 <- ggplot(data, aes(x = hour1, y = Pred.Strat, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

hr2 <- ggplot(data, aes(x = hour2, y = Pred.Strat, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

hr3 <- ggplot(data, aes(x = hour3, y = Pred.Strat, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

hr4 <- ggplot(data, aes(x = hour4, y = Pred.Strat)) + 
  geom_density_ridges2() + theme_bw(base_size = 16)

hr5 <- ggplot(data, aes(x = hour5, y = Pred.Strat)) + 
  geom_density_ridges2() + theme_bw(base_size = 16)

# abandoned because this isn't working

# proportion of time spent in predator free hrs 13:00-01:00
# sum all time active
library(dplyr)
data$SumTime <- apply(data[,c(15:38)], 1, sum)
data$PredOverlap <- apply(data[,c(17:27)], 1, sum)
data$propPredOverlap <- data$PredOverlap/data$SumTime

# only predator type
ggplot(data, aes(y = Pred.Strat, x = propPredOverlap, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  xlab("Prop of time overlapping with predator") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

# using all three factors together
ggplot(data, aes(y = f1f2f3, x = propPredOverlap, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  ylab("Predator strategy") +
  xlab("Prop of time overlapping with predator") +
  scale_fill_viridis_d(name = "Quartiles", alpha = 0.7) +
  theme_bw(base_size = 16)

# quantifying time shift using a binomial model ----
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit2 <- stan_glm(propPredOverlap ~ 0 + Pred.Strat*Pred.Start.Con*Prey.Start.Con,
                 weights = SumTime,
                 data = data,
                 prior = t_prior,
                 cores = 2,
                 seed = 12345,
                 family = binomial(link = "logit"))

round(posterior_interval(fit1, prob = 0.95), 3)

# check model fit
(loo1 <- loo(fit2)) # everything looks good

# look at model fit in Shiny
sso <- shinystan::as.shinystan(fit1, ppd = FALSE)
if (interactive()) launch_shinystan(sso)


## the proportions of proportions ----
# let's go!
data$shifts <- data$propW/data$propPredOverlap

# create new labels
# New facet label names for dose variable
pred.labs <- c("Pred Large Domain", "Pred Small Domain")
names(pred.labs) <- c("Large", "Small")

# New facet label names for supp variable
prey.labs <- c("Prey Large Domain", "Prey Small Domain")
names(prey.labs) <- c("Large", "Small")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)

# all pred effects together
ggplot(data, aes(y = shifts, x = Pred.Strat, fill = shifts)) + 
  geom_jitter(pch = 21, size = 2) +
  #scale_fill_viridis_c(option = "cividis") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 1,
    na.value = "grey50"
  ) +
  ylab("Space:time shift") +
  xlab("Predator strategy") +
  geom_hline(yintercept = 1, linetype="dashed", color = "grey30") +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = pred.labs, Prey.Start.Con = prey.labs)) +
  theme_bw(base_size = 16)

# recode habitat domain (may not be necessary)
data$Pred.Start.Con2 <- ifelse(data$Pred.Start.Con == "Large", 1, 0)
data$Prey.Start.Con2 <- ifelse(data$Prey.Start.Con == "Large", 1, 0)

# separated by predator-type
SitWait <- subset(data, Pred.Strat=="Sit-and-Wait")
SitPursue <- subset(data, Pred.Strat=="Sit-and-Pursue")
Active <- subset(data, Pred.Strat=="Active")

ggplot(SitWait, aes(y = shifts, x = Pred.Strat, fill = shifts)) + 
  geom_jitter(pch = 21, size = 2) +
  scale_fill_viridis_c() +
  ylab("Space:Time Shift") +
  geom_hline(yintercept = 1, linetype="dashed", color = "grey30") +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con) +
  theme_bw(base_size = 16)

# a model to look at it shifts ----


# quantifying space:time shift using a gaussian model

# comparing all predators
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
Predmod <- stan_glm(shifts ~ 0 + Pred.Strat,
                    data = data,
                    prior = t_prior,
                    cores = 2,
                    seed = 12345,
                    family = gaussian)

round(posterior_interval(Predmod, prob = 0.95), 3)
loo(Predmod)

# active predators
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
Shiftmod <- stan_glm(shifts ~ Pred.Start.Con2*Prey.Start.Con2,
                     data = Active,
                     prior = t_prior,
                     cores = 2,
                     seed = 12345,
                     family = gaussian)

round(posterior_interval(Shiftmod, prob = 0.95), 3)

# sit-and-pursue predators
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
SitPursuemod <- stan_glm(shifts ~ Pred.Start.Con*Prey.Start.Con,
                         data = SitPursue,
                         prior = t_prior,
                         cores = 2,
                         seed = 12345,
                         family = gaussian)

round(posterior_interval(SitPursuemod, prob = 0.95), 3)

# check model fit
(loo1 <- loo(SitPursuemod)) # everything looks good

# sit-and-wait predators
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
SitWaitmod <- stan_glm(shifts ~ Pred.Start.Con*Prey.Start.Con,
                       data = SitWait,
                       prior = t_prior,
                       cores = 2,
                       seed = 12345,
                       family = gaussian)

round(posterior_interval(SitWaitmod, prob = 0.95), 3)

# check model fit
(loo1 <- loo(SitPursuemod)) # everything looks good

# look at model fit in Shiny
sso <- shinystan::as.shinystan(fit1, ppd = FALSE)
if (interactive()) launch_shinystan(sso)


# interactions vs. prop of time spent in low detection

