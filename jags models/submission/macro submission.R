# macro settings ----------------------------------------------------------
macro <- list()

### MCMC settings

if(!exists("test")){
  test <- FALSE
  message("Test does not exists, and is set to negative")
}

if(!exists("fast")){
  fast <- FALSE
  message("fast does not exists, and is set to negative")
}


options(tidyverse.quiet = TRUE)


  ### load required packages
c(library(dplyr),
    library(lubridate),
    library(tidyr),
    library(rjags),
    library(coda),
    library(CovidGer),
    library(lubridate),
    library(R2jags),
    library(MCMCvis),
    library(dclone))
message("Packages loaded.")

# +++ TEST +++ --------------------------------------------------------------------
# run short version for testing
if(test){
  macro$lk_quantile_threshold <- 1/10
  if(server)
  {
    macro$MCMC_n_adapt <- 1000
    macro$MCMC_n_burn <- 1000
    macro$MCMC_n <- 2000
    macro$MCMC_chains <- 4
    macro$MCMC_thin <- 4
  } else{
    macro$MCMC_n_adapt <- 5
    macro$MCMC_n_burn <- 5
    macro$MCMC_n <- 10
    macro$MCMC_chains <- 1
    macro$MCMC_thin <- 1
  }
  message("Test macro settings active.")
} else {

  # no test -----------------------------------------------------------------
  macro$lk_quantile_threshold <- 4/10

  macro$MCMC_n_adapt <- 1000
  macro$MCMC_n_burn <- 4000
  macro$MCMC_n <- 5000
  macro$MCMC_chains <- 4
  macro$MCMC_thin <- 5
}






# +++ FAST +++ --------------------------------------------------------------------
if (fast){

  # age groups that are not analyzed
  macro$age.drop <- c("A00-A04","A05-A14")

  # + timing  ---------------------------------------------------------------

  # date to end model (if FALSE uses all available data)
  macro$end_date <- as.Date("2020-05-15")

  # number of cases to start modeling
  macro$total_limit <- 1

  # date to start model (if no date includes start model based on total_limit)
  macro$start_date <- FALSE

  # + priors ----------------------------------------------------------------

  ### R priors
  R_mean <- 2.5
  R_sd <- .5


  # + data inclusion --------------------------------------------------------

  # include all subregions(lk) with treatment?
  macro$include_treatment_lk <- TRUE

  # include only regions with treatment
  macro$include_only_treatment_BL <- TRUE

  ### Covariates

  # include weekday FE
  macro$wday_FE <- TRUE
  # include week FE
  macro$time_FE<- FALSE

  macro$dummies.interventions <- setdiff(colnames(CovidGer::interventions),
                                         c("date","unit","Bundesland","all_interventions_exist"))

  macro$dummies.interventions <- macro$dummies.interventions[!grepl("full testing",macro$dummies.interventions)]
  macro$dummies.interventions <- macro$dummies.interventions[
    !grepl("limited",macro$dummies.interventions) |
      grepl("kitas",macro$dummies.interventions) |
      grepl("schools",macro$dummies.interventions) |
      grepl("speech",macro$dummies.interventions) |
      grepl("distance",macro$dummies.interventions)|
      grepl("gatherings",macro$dummies.interventions)|
      grepl("events",macro$dummies.interventions)]
  macro$dummies.interventions <- macro$dummies.interventions[
    !grepl("open",macro$dummies.interventions)|
      grepl("sports openlimited",macro$dummies.interventions)|
      grepl("churches",macro$dummies.interventions)|
      grepl("kitas openlimited",macro$dummies.interventions)|
      grepl("schools openlimited",macro$dummies.interventions)]
  macro$dummies.interventions <- macro$dummies.interventions[!grepl("kitas full",macro$dummies.interventions)]
  macro$dummies.interventions <- macro$dummies.interventions[!grepl("local lockdown",macro$dummies.interventions)]
  macro$dummies.interventions <- macro$dummies.interventions[!grepl("app",macro$dummies.interventions)]

  macro$dummies.interventions <-
    setdiff(macro$dummies.interventions,
             c("kitas","schools",
               "events recommendedlimited",
               "kitaslimited","schoolslimited",
               "festivities","universities"))
  macro$dummies.interventions <- c(macro$dummies.interventions,
                                   "education")

  message("Speed macro settings active.")

} else {

  # not fast -----------------------------------------------------------------

  # age groups that are not analyzed
  macro$age.drop <- c("A00-A04")

  # date to start model (if FALSE use total_limit to determine start)
  macro$start_date<- as.Date("2020-05-01")

  # date to end model (if FALSE uses all available data)
  macro$end_date<- as.Date("2020-09-01")

  ### R priors
  R_mean <- 1
  R_sd <- .5

  ### Covariates


  macro$dummies.interventions <- c("holidays")

  # include weekday FE
  macro$wday_FE <- TRUE
  # include week FE
  macro$time_FE<- TRUE

  # include all subregions(lk) with treatment?
  macro$include_treatment_lk <- FALSE

  # include only regions with treatment
  macro$include_only_treatment_BL <- FALSE

}



# +++ ALWAYS +++ ------------------------------------------------------------------

# + priors -------------------------------------------------------------------

# standard deviation of random noise of R
macro$error_sd <- .1
#hist(rnorm(1000,1,macro$error_sd))

# standard deviation of effects
macro$effect_sd <- .2
#hist(rnorm(1000,1,macro$effect_sd))

# dispersion assumed to have N^+(disp_mean,disp_sd)
macro$disp_mean <- 0
macro$disp_sd <- 5

# mean and standard deviation of detection rate(not used in standard model)
macro$srate_mean <- 1/4
macro$srate_sd <- .001
#hist(rnorm(1000,srate_mean,srate_sd))

### time distribution transmission to symptom onset
macro$mean_SI_mean <- 5.5
macro$mean_SI_sd <- .1
#hist(rnorm(1000,macro$mean_SI_mean, macro$mean_SI_sd))
macro$sd_SI <- 2
#plot_gamma_dist(macro$mean_SI_mean,macro$sd_SI)

### distribution of secondary transmissions over time
macro$mean_trans_mean <- 5.5
macro$mean_trans_sd <- .1
#hist(rnorm(1000,macro$mean_trans_mean, macro$mean_trans_sd))
macro$sd_transmission <- 2
#plot_gamma_dist(macro$mean_trans_mean,macro$sd_transmission)


### AR term
macro$ar_sd <- .001
#plot_ar_dist(macro$ar_sd)

### initial infections
if(!is.null(macro$total_limit)){
  macro$tau_mean <- macro$total_limit*4
} else {
  macro$tau_mean <- 10
  }

macro$tau_sd <- 4
#hist(rnorm(1000,macro$tau_mean,macro$tau_sd))


# Define number of days after infection in
# which new infections and sympton onset are considered
max_past <- 14

# + data inclusion ----------------------------------------------------------

# ++ timing ----------------------------------------------------------

# initilize infected
macro$days_init <- 6

# date from which rki cases are considered valid
macro$data_start <- as.Date("2020-02-15")

# date data is extended to for modeling
macro$expand_dates_from <- as.Date("2020-01-15")

# define number of days between information reaching Gesundheitsamt and reaching public
macro$info_lag <- 1

# ++ covariates -----------------------------------------------------------

# continuous covariates that are not normalized
macro$cov.not.normalize <- c(
  "delay1n",
  paste0("delay",0:6),
  "traced",
  "cumsum_incidence100")

macro$cov.not.demean <- c(
  "info_incidence",
  "info_lincidence"
)

# if TRUE use historic weather as robustness check
macro$pseudo.weather <- FALSE

# factorized vars
macro$cov.factorize <- c()#c(macro$cov.weather)
macro$n_factors <- 3

macro$timediff_cumsum <- 14

# ++ regions ----------------------------------------------------------

macro$lk_max_pos <- Inf
macro$lk_min_pos <- -Inf
# average number of asymptomatic cases in subregion
macro$lk_min_intercept <- .5
macro$lk_max_intercept <- .95


# helper functions --------------------------------------------------------

factorize <- function(x,length=5,digits=2) {

  quantiles.cur <- quantile(x, seq(0+1/length,1-1/length,length.out = length-1),na.rm=TRUE)

  quantiles.cur.round <- signif(quantiles.cur,digits = digits)

  factor(
    findInterval(x,quantiles.cur)-(length-1)/2,
    labels = paste0(c("",quantiles.cur.round), c("<", rep("-",length(quantiles.cur.round)-1),"<"),c(quantiles.cur.round,""))
    #,    levels = seq(-length/2+.5, length/2-.5,length.out = length)
  )
}
