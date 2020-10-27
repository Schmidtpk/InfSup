#' @export
extract_effects <- function(cov.choice = macro$cov,
                         times = FALSE,
                         filter_age = NULL)
{
  if(is.null(cov.choice)|length(cov.choice)==0){
    message("No variables selected")
    return(NULL)
  }

  dim_names = c("age","m")

  if(length(dat$dummies)!=0){
    ### extract dummies
    res.dummies <- extract_mcmc("beta_effectxd\\[",
                                dim_names = dim_names,
                                samples = TRUE)

    dummies <- data.frame(
      m=1:length(dat$dummies),
      covariate = dat$dummies)

    res.dummies <- res.dummies %>%
      left_join(dummies)%>%
      select(-m)%>%
      rename(m=covariate,
             draw = `beta_effectxd\\[`)
  } else {
    res.dummies <- data.frame()
  }

  if(length(dat$cov)!=0){
    ### extract continuous covariates
    res.cov <- extract_mcmc("beta_effect\\[",
                            dim_names = dim_names,
                            samples = TRUE)

    cov <- data.frame(
      m=1:length(dat$cov),
      covariate = dat$cov
    )

    res.cov <- res.cov %>%
      left_join(cov)%>%
      select(-m)%>%
      rename(m=covariate,
             draw = `beta_effect\\[`)
  } else {
    res.cov <- data.frame()
  }

  # join dummies and continuos covariates
  res <- rbind(res.cov,res.dummies)

  #filter according to covariate choice
  res <- res %>% filter(m %in% cov.choice)

  return(res)
}

#' Extract MCMC output
#'
#' @param name name of parameter
#' @param quantiles extract mean and sd or quantiles (default FALSE, which is mean and sd)
#' @param df data.frame optional ,default is res in parent environment
#' @param dim_names vector of dimension names that were extracted
#' @param add_info adds automatically date to t from data and country/region name from colnames of d
#' @param sample_name subsample of name observations to look at. logical takes 16 random. vector of characters takes those. number takes that number at random.
#' @param without_name substrings that should not be taken from mcmc output
#' @param exact_name if name of parameter has to match exactly. standard FALSE.
#'
#' @return data.frame
#' @export
extract_mcmc <- function(name,
                        dim_names = NULL,
                        quantiles=F,
                        samples=F,
                        df=NULL,
                        add_info = TRUE,
                        t_only_model = FALSE,
                        sample_name = NULL,
                        sample_age = NULL,
                        without_name = NULL,
                        exact_name=FALSE)
{
  if(!is.null(df))
    res <- df

  if(class(res)=="summary.mcmc"){
    # type of output
    if(quantiles)
      res <- res$quantiles
    else if(samples){
      res <- as.data.frame(t(coda:::as.matrix.mcmc.list(samps)))
      } else
      res <- res$statistics
  } else {
    stop("Class of res or df is wrong")
  }


  if(!exists("d"))
    d <- dat$reports


  if(exact_name){
    dat.cur <- data.frame(res[
      substr(rownames(res),1,nchar(name))==name &
        substr(rownames(res), nchar(name)+1,nchar(name)+1)=="["
      ,])
  } else{
    dat.cur <- data.frame(res[
      (grepl(name,rownames(res)))
      ,])
  }


  if(!is.null(without_name))
    dat.cur <- dat.cur[!grepl(without_name,rownames(dat.cur)),]

  a <- stringr::str_split(rownames(dat.cur),",")
  b <- readr::parse_number(unlist(a))
  c <- data.frame(matrix(b,nrow = nrow(dat.cur),byrow = T))

  if(is.null(dim_names)){
    if(length(colnames(c))==3)
      dim_names = c("t","name","age")
    if(length(colnames(c))==2)
      dim_names = c("t","name")
    if(length(colnames(c))==1){
      if(nrow(c)<10)
        dim_names<-"age"
      else
        dim_names <- "t"
    }
    message(paste(c("Automatically assigned the dimension names:", dim_names),collapse = " "))
  }


  colnames(c)<-dim_names
  dat.cur <- cbind(dat.cur,c)

  # reshape format if samples
  if(samples){
    dat.cur <- dat.cur %>%
      tidyr::pivot_longer(cols = -c(dim_names),names_to = "iteration",values_to = name) %>%
      mutate(
        iteration = readr::parse_number(as.character(iteration))
      )

    dat.cur <- as_tibble(dat.cur)
  }


  # add date
  if(add_info & ("t" %in% colnames(dat.cur))){
    dat.cur <- dat.cur %>% left_join(data%>%select(t,date)%>%unique())
  }


  if(add_info & ("name" %in% colnames(dat.cur)))
  {
    dat.cur$name <- factor(dat.cur$name, labels = colnames(d))
  }

  if(t_only_model & ("t" %in% colnames(dat.cur))){
    Ti <- data.frame(Ti = dat$Ti, name= names(dat$Ti))
    dat.cur <- left_join(dat.cur,Ti)
    dat.cur <- dat.cur %>% filter(t >= Ti)
    message(paste0(c("Drop observations before dat$Ti, which is ", dat$Ti),collapse = " "))
  }

  if(add_info & ("age" %in% colnames(dat.cur)))
  {
    dat.cur$age <- factor(dat.cur$age, labels = dimnames(d)[3][[1]])
  }


  if(!is.null(sample_name) & ("name" %in% colnames(dat.cur)))
  {
    if(is.numeric(sample_name))
      dat.cur <- dat.cur %>% filter(name %in% sample(unique(dat.cur$name), sample_name))
    else if(is.logical(sample_name))
      dat.cur <- dat.cur %>% filter(name %in% sample(unique(dat.cur$name), min(16,length(unique(dat.cur$name)))))
    else if(is.character(sample_name)|is.vector(sample_name))
      dat.cur <- dat.cur %>% filter(grepl(paste(sample_name,collapse = "|"),name))
  }

  if(!is.null(sample_age) & ("age" %in% colnames(dat.cur)))
  {
    if(is.numeric(sample_age))
      dat.cur <- dat.cur %>% filter(age %in% sample(unique(dat.cur$age), sample_age))
    else if(is.logical(sample_age))
      dat.cur <- dat.cur %>% filter(age %in% sample(unique(dat.cur$age), min(16,length(unique(dat.cur$age)))))
    else if(is.character(sample_age)|is.vector(sample_age))
      dat.cur <- dat.cur %>% filter(grepl(paste(sample_age,collapse = "|"),age))
  }

  # add variable name as comment
  comment(dat.cur)<-name

  return(dat.cur)
}




#' pull sample of one variable of mcmc draws
#'
#' @param var_name name of variable
#'
#' @return vector of draws
#' @export
pull_mcmc_sample <- function(var_name){
  as_tibble(data.table::rbindlist(lapply(samps,as.data.frame)))%>%pull(var_name)}


#' Plot combined effect of weather on time
#'
#' @param grid return grid with name on age instead of standard which is one single plot for each name
#' @param forecast standard FALSE, if TRUE takes weather variables for whole year and averages for each day in year
#' @param model standard FALSE, if TRUE uses linear model, if FALSE uses kernel sieve
#' @param wrap if TRUE uses wrap instead of grid plotting ages in one plot
#' @param hlines adds hline at zero
#' @param dim_names dimension names for effects standard age with m for covariate
#' @param draws_max maximum number of draws from posterior for speed, standard 100
#' @param choose_age subset of ages to look at
#' @param choose_c subset of units (countries) to look at
#' @param average_c show all results or only average for all countries
#' @param ... further parameters for extract_mcmc
#'
#' @return ggplot
#' @export
show_total_effect <- function(covariates = macro$cov.weather,
                              forecast=FALSE,
                              CI_large = TRUE,
                              hlines = TRUE,
                              draws_max = NULL,
                              choose_age = FALSE,
                              choose_c = NULL,
                              average_c = FALSE,
                              average_age = FALSE,
                              smooth = FALSE,
                              show_covariate = FALSE,
                              ...)
{
  dim_names <- c("age","m")

  R <- extract_effects(covariates)

  # R <- extract_mcmc("beta_effect\\[",
  #                     dim_names = dim_names,
  #                     samples = TRUE,...) %>%
  #     rename(draw = 'beta_effect\\[')


  if(!is.null(draws_max)) # drop some draws from mcmc for speed
    R <- R %>% filter(iteration %in% sample(unique(iteration),draws_max))

  if(choose_age){# filter chosen age groups
    if(is.numeric(choose_age))
      choose_age <- dimnames(dat$reports)[[3]][choose_age]
    R <- R %>% filter(age %in% choose_age)
  }

  # load x
  if(!forecast){
    x <- as_tibble(as.data.frame.table(dat$x))
    names(x)<- c("t","c","m","x")

    x <- x %>%
      mutate(t=as.numeric(t)) %>%
      filter(t >= min(dat$Ti))

    message(paste("Not in model:", setdiff(covariates,unique(x$m))))
    x <- x %>% filter(m %in% covariates)

    x <- x %>% filter(!is.na(x))

    x <- x %>% left_join(data%>%select(t,date)%>%unique()) %>% select(-c(t))

  } else {

    if(any(!covariates %in% macro$cov.weather))
      stop("can only forecast weather variables")

    #load data
    x <- as_tibble(Covid::weather_dwd)%>%select(date,covariates)
    x$date <- as.Date(x$date)


    # summarise for each date
    x <- x %>%
      mutate(date=as.Date(paste0("2020-",lubridate::month(date),"-",lubridate::day(date)))) %>%
      group_by(date)%>%
      summarise_at(vars(covariates),mean, na.rm=TRUE)


    for(i in 1:nrow(macro$df.standardize)){
      cov.cur <- macro$df.standardize$cov[i]
      if(cov.cur %in% colnames(x))
      {
        mean.cur <- macro$df.standardize$mean[i]
        sd.cur <- macro$df.standardize$sd[i]
        x[,cov.cur] <- (x[,cov.cur]-mean.cur)/sd.cur
      }
    }

    x$c <- "average"
    #x$t <- as.numeric(x$date-min(x$date)+1)

    x <- x %>% pivot_longer(cols = covariates,
                            names_to = 'm',
                            values_to = 'x')

  }


  if(!is.null(choose_c)){# filter chosen countries
    x2 <- x %>% filter(c %in% choose_c)
    if(nrow(x2)==0){
      warning("choose_c drops all observations. Take random instead.")
      x2 <- x %>% filter(c %in% sample(c,1))
    }
    x <- x2
  }

  if(average_c){
      x <- x %>% group_by(date,m) %>%
        summarise(
          x = mean(x,na.rm = TRUE)
        ) %>% mutate(c="average")
  }

  # join x and posterior draws
  x <- x %>% left_join(R)

  x$effect <- 1+x$x * x$draw


  # compute
  effects <- x %>%
    group_by(date,c,age,iteration) %>%
    summarise(
      value = (prod(effect)-1)*100
    )

  if(!CI_large){
    effects <- effects %>%
      group_by(date,c,age) %>%
      summarise(
        down = quantile(value,.25),
        X50. = quantile(value,.5),
        up = quantile(value,.75)
      )
  } else {
    effects <- effects %>%
      group_by(date,c,age) %>%
      summarise(
        down = quantile(value,.025),
        X50. = quantile(value,.5),
        up = quantile(value,.975)
      )
  }

  if(average_age){
    effects <- effects %>% group_by(date,c) %>%
      summarise(
        down = mean(down),
        X50. = mean(X50.),
        up = mean(up),
      ) %>% mutate(age="average")
  }

  if(smooth){
    effects <- effects %>%
      group_by(age,c)%>%
      arrange(date)%>%
      mutate(up = zoo::rollapplyr(up, na.rm=TRUE,width = smooth, FUN = mean, partial = TRUE),
             down = zoo::rollapplyr(down, na.rm=TRUE,width = smooth, FUN = mean, partial = TRUE),
             X50. = zoo::rollapplyr(X50., na.rm=TRUE,width = smooth, FUN = mean, partial = TRUE))
  }

  # change age labels
  x$age <- age_labels(x$age)
  effects$age <- age_labels(effects$age)


  p <- ggplot(effects,aes(x=date))

  if(hlines) # add horizontal lines at 0 and at minimum of medians
    p <- p +
    geom_hline(yintercept = 0)

  p <- p +
    geom_ribbon(alpha=.3,aes(ymin=down,ymax=up),alpha=.1)+
    geom_line(aes(y=X50.))+ylab("total effect")

  p <- p+
    facet_grid(age~.,scales = "free")+
    theme(axis.title.x = element_blank())

  if(!show_covariate){
    return(p+theme_bw())
  }else{
    p2 <- ggplot(
      x,
      aes(x=date,y=x,color=c))+
      geom_point()+
      geom_line()

    p <- p +
      geom_ribbon(alpha=.3,aes(group=c,color=c,fill = c, ymin=down,ymax=up),alpha=.1)+
      geom_line(aes(group=c,color =c,y=X50.))+
      ylab("total effect")+
      guides(fill=FALSE)+
      facet_grid(age~.,scales = "free")

    ggpubr::ggarrange(p,p2,
                      ncol = 1,
                      common.legend = TRUE,
                      legend = "bottom")
  }
}







#' Plot covariate effects according to mcmc output
#'
#' New version of plot_effects
#'
#' @param times if numeric, multiplicates output by times, if function applies function
#' @param dim_names dimension names of effects (standard is age and m, which is the number of covariates)
#' @param ...
#'
#' @return
#' @export
show_effects_sample <- function(
                         cov.choice = macro$cov,
                         times = FALSE,
                         filter_age = NULL,
                         average.age = FALSE,
                         hlines = TRUE,
                         CI_large = TRUE,
                         add_prior = FALSE,
                         background_prior = TRUE,
                         scale = "free_y",
                         order = FALSE,
                         width_errorbar = 1,
                         ...)
{
  if(is.null(cov.choice)|length(cov.choice)==0){
    message("No variables selected")
    return(NULL)
  }

  res <- extract_effects(cov.choice)

  if(is.logical(times)|is.numeric(times)){
    if(times){
      res <- res %>%
        mutate(draw = times*draw)
    }
  } else if(is.function(times)) {
    res <- res %>% mutate(
      draw = times(draw))
  } else {
    stop("times wrong type")
  }


  if(!is.null(filter_age)){
    if(is.numeric(filter_age))
      filter_age <- unique(res$age)[filter_age]

    res <- res %>% filter(age %in% filter_age)
  }

  if(average.age){#weighted mean
    res <- res %>%
      left_join(Covid::regionaldatenbank%>%
                  filter(adm.level==1)%>%
                  select(age,total)) %>%
      group_by(m,iteration)%>%
      summarise(
        draw = weighted.mean(draw,w = total),
        age = "average"
      )
  }

  # show percentage
  res$draw <- res$draw * 100

  # choose quantiles to illustrate
  if(CI_large)
    qlevels <- c(.025,0.975)
  else
    qlevels <- c(.25,.75)

  res <- res %>% group_by(m,age)%>%
  summarise(
    middle = median(draw),
    down = quantile(draw, qlevels[1]),
    up = quantile(draw, qlevels[2]))





  if(add_prior){
    first.row <- res[1,]
    sample.prior <- rnorm(1e5,0,dat$effect_sd)
    sample.prior <- sample.prior[sample.prior>-1]
    first.row$category <- "others"
    first.row$m <- "prior"
    first.row$age <- "prior"

    if(CI_large){
      first.row$down <- quantile(sample.prior,probs = 0.025)
      first.row$middle <- quantile(sample.prior,probs = 0.5)
      first.row$up <- quantile(sample.prior,probs = 0.975)
    } else {
      first.row$down <- quantile(sample.prior,probs = 0.25)
      first.row$middle <- quantile(sample.prior,probs = 0.5)
      first.row$up <- quantile(sample.prior,probs = 0.75)
    }
    res <- rbind(res,first.row)
  }

  if(background_prior){
    prior<-list()
    sample.prior <- rnorm(1e5,0,dat$effect_sd)
    sample.prior <- sample.prior[sample.prior>-1]
    if(CI_large){
      res$prior.down <- quantile(sample.prior,probs = 0.025)
      res$prior.middle <- quantile(sample.prior,probs = 0.5)
      res$prior.up <- quantile(sample.prior,probs = 0.975)
    } else {
      res$prior.down <- quantile(sample.prior,probs = 0.25)
      res$prior.middle <- quantile(sample.prior,probs = 0.5)
      res$prior.up <- quantile(sample.prior,probs = 0.75)
    }
  }

  # relabel age
  res$age <- age_labels(res$age)

  if(!order){

    # make nice names
    res$category <- "others"

    res$category[grepl("temperat",res$m)|
                   grepl("Relative_Feuchte",res$m)|
                   grepl("Dampf",res$m)|
                   grepl("Wind",res$m)|
                   grepl("Regen",res$m)|
                   grepl("Sonne",res$m)|
                   grepl("info",res$m)]<- "weather, info"


    res$category[grepl("testing",res$m)|
                   grepl("education",res$m)|
                   grepl("kita",res$m)|
                   grepl("holidays",res$m)|
                   grepl("school",res$m)]<- "education and testing"

    res$category[grepl("bars",res$m)|
                   grepl("festivities",res$m)|
                   grepl("events",res$m)|
                   grepl("restaurants",res$m)|
                   grepl("sports",res$m)]<- "entertainment"


    res$category[grepl("masks",res$m)|
                   grepl("curfew",res$m)|
                   grepl("distance",res$m)|
                   grepl("churches",res$m)|
                   grepl("gathering",res$m)]<- "public space"

    res$category[grepl("open",res$m)|
                   grepl("trace",res$m)|
                   grepl("delay",res$m)]<- "reopening"


    res$category[res$m %in% macro$dummies.factors]<- "factors"


    res$m <- gsub("eventslimited","events lim.",res$m)
    res$m <- gsub("limited","",res$m)
    #res$m <- gsub(" open","?",res$m)

    res$m <- gsub("events recommendedlimited","events rec.",res$m)

    res$m <- gsub("speechMerkel","speech (2nd)",res$m)
    res$m <- gsub("cumsum_incidence100","cum. incidence",res$m)
    res$m <- gsub("info_incidence"," incidence",res$m)
    res$m <- gsub("info_lincidence"," log incidence",res$m)
    res$m <- gsub("UPM.Relative_Feuchte","rel. humidity",res$m)
    res$m <- gsub("VPM.Dampfdruck","abs. humidity",res$m)
    res$m <- gsub("SDK.Sonnenscheindauer","sun hours",res$m)
    res$m <- gsub("TMK.Lufttemperatur","temperature",res$m)
    res$m <- gsub("recommended","rec.",res$m)

    # assign week day fixed effects if present
    if(all(unique(res$m)%in%macro$dummies.FE)){
      warning("Assign factor levels to weekday dummies")
      res$m <- factor(res$m,levels = c("FESun","FEMon","FETue","FEWed","FEThu","FEFri"))
    }

    pd <- position_dodge(.7)
    p <- ggplot(res,aes(x=m,shape=age, group=age))+
      geom_errorbar(aes(ymin=down,ymax=up),alpha=.8,position =  pd)+
      geom_point(aes(y=middle),position = pd)+coord_flip()+
      facet_wrap(~category,scale=scale,...)
  } else {

    pd <- position_dodge(3)
    res <- res %>% group_by(m) %>%
      mutate(orderby = -mean(middle))%>%
      ungroup()
    p <- ggplot(res,aes(x=m,shape=age, group=age))+
      geom_errorbar(aes(ymin=down,ymax=up),alpha=.8,position =  pd,width=width_errorbar)+
      geom_point(aes(y=middle),position = pd)+coord_flip()+
      scale_x_discrete(limits = res$m[order(res$orderby)],
                       label = my_labeller2)
  }

  if(hlines)
    p <- p +
    geom_hline(yintercept = 0,alpha=.5)

  if(background_prior){
    if(!order)
      dat.cur <- res %>% select(age,category,m,prior.up,prior.down)%>%
      group_by(category)%>%
      slice(1)
    else
      dat.cur <- res %>% select(age,m,prior.up,prior.down)%>%
        slice(1)

    return(p +
             geom_rect(data= dat.cur,
                       aes(ymin=prior.down*100, ymax=prior.up*100, xmin=-Inf, xmax=Inf),
                       alpha=.1)+
             theme_bw()+
             theme(legend.position="bottom",
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank()))
  }

  return(p+theme_bw()+
           theme(legend.position="bottom",
                 axis.title.y = element_blank()))
}



#' @export
standardise <- function(x)
  (x-mean(x))/sd(x)
