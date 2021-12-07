# Preparation -------------------------------------------------------------
library(tidyverse)
options(tidyverse.quiet = TRUE)

n.runs <- 10
speed.factor <- 1

library(foreach)
doParallel::registerDoParallel(10)

save<-foreach(seed.cur = 1:n.runs,
        .combine = rbind,
        .errorhandling='pass',
        .packages = c("dplyr","lubridate","tidyverse","R2jags","MCMCvis","mcmcplots","coda","rjags")
)  %dopar% {

  ### check if file is executed on server
  if(grepl("quamet",getwd())){
    message("Working directory looks like server. Different options are now valid")
    server <- TRUE
  } else {
    # set working directory to project (needed if run in jobs)
    setwd(gsub("/RCovModel.*","/RCovModel",getwd()))
    server <- FALSE
  }

  message("load data")
  if(server){
    load("/home/quamet/patrick/submission/save/submission.RData")
  } else {
    load("./jags models/submission/save/submission.RData")}


  # load results ------------------------------------------------------------
  message("load results")
  mcmc <-
    mymcmc::extract_res(samps,
                        regex=c("mean_SI|mean_transmission|Rzero|beta_effect|beta_effectxd|i_disp|tau"))

  mcmc.res <- mcmc%>%
    group_by(varname,V1,V2,dim,maxV1,maxV2)%>%
    summarise(value = mean(value))%>%
    ungroup()

  # extract names without brackets and dimensions
  vars.summary <- mcmc.res %>%
    select(varname,dim,contains("maxV"))%>%unique()

  # loop over names and fill arrays according to mcmc iteration
  message("gen list")
  estimates.list <- list()
  for(i in 1:nrow(vars.summary)){

    # get dimensions
    dim.cur <- vars.summary[i,]%>%select(starts_with("maxV",ignore.case = F))
    dim.cur <- as.numeric(dim.cur)
    dim.cur <- dim.cur[is.finite(dim.cur)]

    # generate array if dimensions accordingly
    if(length(dim.cur)>1){
      # choose data
      res.cur <- mcmc.res %>%
        filter(varname == vars.summary$varname[i])

      if(vars.summary$maxV1[i]>=vars.summary$maxV2[i]){
        res.cur <- res.cur %>% arrange(V1,V2) %>% pull(value)
        res.cur <- array(res.cur,dim = dim.cur)
      } else {
        res.cur <- res.cur %>% arrange(V2,V1) %>% pull(value)
        res.cur <- array(res.cur,dim = dim.cur)
      }
    } else {
      res.cur <- mcmc.res %>%
        filter(varname == vars.summary$varname[i])%>%
        pull(value)
    }
    # add data entry to list
    estimates.list[[vars.summary$varname[i]]]<-res.cur
  }


  # simulate ----------------------------------------------------------------

  ### create model path
  if(server){
    model_path <- paste0("/home/quamet/patrick/submission/sim ","base error tau",".R")
  } else {
    model_path <- paste0("./jags models/submission/sim ","base error tau", ".R")
  }


  # safe reports of data and empty dat entry
  original.reports <- dat$reports
  dat$reports <- NULL
  dat$tau_mean<-NULL

  # create jags model
  jm <- rjags::jags.model(model_path,
                          inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seed.cur),
                          data = c(dat,
                                   estimates.list),n.chains = 1,n.adapt = 0)

  # simulate from jags model
  res.cur <- rjags::coda.samples(model = jm,
                                 variable.names = "reports",
                                 n.iter = 1)

  # extract results for reports variable
  res.df <- mymcmc::extract_res(res.cur,regex = 'reports')%>%select(value,V1,V2,V3)
  res.df <- res.df%>%complete(V1=1:max(V1),V2=1:max(V2),V3=1:max(V3),fill = list(value=0))
  res.df <- res.df%>%
    left_join(data.frame(name=colnames(dat$x),V2 = 1:ncol(dat$x)))%>%
    left_join(data.frame(age=dimnames(original.reports)[3][[1]],V3 = 1:length(dimnames(original.reports)[3][[1]])))


  # safe simulated reports in array
  res.df <- res.df %>% arrange(age,name,V1)
  reports.sim <- array(res.df %>% pull(value),
                   dim = c(length(unique(data$t)),
                           length(unique(data$name)),
                           length(unique(data$age))),
                   dimnames = list( unique(data$t),
                                    unique(data$name),
                                    unique(data$age))
  )


  # estimate ----------------------------------------------------------------

  # load all data from main estimation
  rm("dat")
  if(server){
    load("/home/quamet/patrick/submission/save/submission.RData")
  } else {
    load("./jags models/submission/save/submission.RData")}


  ### change model path
  if(server){
    model_path <- paste0("/home/quamet/patrick/submission/jags ",macro$model_name,".R")
  } else {
    model_path <- paste0("./jags models/submission/jags ",macro$model_name, ".R")
  }

  monitor <- c("beta_effect",
               "beta_effectxd",
               "mean_SI",
               "mean_transmission",
               "i_disp")


  # add reports from simulation
  dat$reports <- reports.sim


  cat(macro$notes)
  message(paste("Starting sampling with",macro$MCMC_n_adapt,macro$MCMC_n_burn,macro$MCMC_n))
  message(paste("Number of units is",dim(dat$reports)[2]))

  start.time <- Sys.time()

  samps.list<- R2jags::jags(n.iter = macro$MCMC_n/speed.factor,
                                     model= model_path,
                                     data=dat,
                                     n.burnin =  macro$MCMC_n/speed.factor/2,
                                     n.chains= 1,
                                     parameters.to.save = monitor,
                                     n.thin = macro$MCMC_thin)

  macro$time.passed <- Sys.time()-start.time
  message(paste("Sampling finished after", macro$time.passed,units(macro$time.passed)))

  resMCMC <- MCMCsummary(samps.list)


# save results ------------------------------------------------------------
  macro$file_name <- "sim12b_"

  if(server){ # save with date
    name.cur <- paste0(macro$file_name,seed.cur)
    path.save <- paste0("/home/quamet/patrick/submission/save/sim/",name.cur,".RData")
  } else{
    name.cur <- paste0(macro$file_name,seed.cur)
    path.save <- paste0("./jags models/submission/sim and est/save/",name.cur,".RData")
  }

  samps <- coda::as.mcmc(samps.list)
  res <- summary(samps)
  save(res,resMCMC,samps,reports.sim,estimates.list,
       file = path.save)
  macro$time.passed <- Sys.time()-start.time
  message(paste("Saving finished after", macro$time.passed,units(macro$time.passed)))
  message(paste("Safed to", macro$file_name,"at",Sys.time()))

  return(resMCMC%>%mutate(run=seed,variable = rownames(resMCMC)))
  }

# if server save
if(grepl("quamet",getwd())){
  saveRDS(save,file = "/home/quamet/patrick/submission/save/sim/resmcmc12b.RDS")
}

