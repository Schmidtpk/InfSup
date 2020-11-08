test<- FALSE
fast <- TRUE
# Preparation -------------------------------------------------------------

### check if file is executed on server
if(grepl("quamet",getwd())){
  message("Working directory looks like server. Different options are now valid")
  server <- TRUE
} else {
  # set working directory to project (needed if run in jobs)
  setwd(gsub("/RCovModel.*","/RCovModel",getwd()))
  server <- FALSE
}

### source macro file
if(server){
  source("/home/quamet/patrick/submission/macro.R")
} else {
  source("./jags models/submission/macro.R",local = TRUE)
}

# Marco settings ---------------------------------------------------

# save under?
macro$file_name <- "submission"

if(test)
  macro$file_name <- paste0(macro$file_name,"_T")

if(!fast)
  macro$file_name <- paste0(macro$file_name,"_late")

# which model to use?
macro$model_name <- "base error"

macro$test <- test
macro$fast <- fast

### note important changes to code
macro$notes <- append(macro$notes,"master: submission/run draft")
macro$notes <- append(macro$notes,"exclude heinsberg")


### create model path
if(server){
  model_path <- paste0("/home/quamet/patrick/submission/jags ",macro$model_name,".R")
} else {
  model_path <- paste0("./jags models/submission/jags ",macro$model_name, ".R")
}

# data inclusion ----------------------------------------------------------
macro$cov.info <- c(
  #"info_incidence",
  "info_lincidence"#,
  #"info_Gincidence",
  #"info_Glincidence"
  )

# append other covariates
if(fast){
  macro$cov.other <- c(
  "traced",
  "cumsum_incidence100")
} else {
  macro$cov.other <- c(
    "traced"
    )
}


macro$cov.weather <- c(
  "UPM.Relative_Feuchte",
  "TMK.Lufttemperatur"#,
  #"VPM.Dampfdruck",
  #"SDK.Sonnenscheindauer",
  #"NM.Bedeckungsgrad",
  #"RSK.Niederschlagshoehe"
)

exclude_locations <- c("LK Heinsberg")

  macro$MCMC_n_adapt <- 4000
  macro$MCMC_n_burn <- 12000
  macro$MCMC_n <- 15000
  macro$MCMC_chains <- 4
  macro$MCMC_thin <- 15



# Priors ------------------------------------------------------------------

# +++ DATA +++ --------------------------------------------------------------------

# load region data ----------------------------------------------------------

### recode Berlin and Hamburg as subregion in population data
# regionaldatenbank %>% filter(name=="Berlin")
# data %>% filter(grepl("Berlin",name))
regionaldatenbank <- CovidGer::regionaldatenbank
regionaldatenbank$id[regionaldatenbank$name=="Berlin"]<-"11000"
regionaldatenbank$adm.level[regionaldatenbank$name=="Berlin"]<-3
# regionaldatenbank %>% filter(name=="Hamburg")
# data %>% filter(grepl("Hamburg",name))
regionaldatenbank$id[regionaldatenbank$name=="Hamburg"]<-"2000"
regionaldatenbank$adm.level[regionaldatenbank$name=="Hamburg"]<-3

# load intervention data
idat <- CovidGer::interventions

# merge schools and kitas to education covariate
idat <- idat %>% mutate(
  education = schools|schoolslimited|kitas
)

# compute incidence of reported cases by subregion
lk_summary <- CovidGer::lk_summary %>%
  left_join(rki_new%>%
              select(Landkreis,IdLandkreis)%>%
              unique())%>%
  left_join(regionaldatenbank%>%
              group_by(id)%>%
              summarise(total = sum(total)), by = c("IdLandkreis"="id"))%>%
  mutate(incidence = pos/total)

# define relevant subregions
cur.kreis <- lk_summary %>%
  filter(pos>=macro$lk_min_pos,
         pos<=macro$lk_max_pos,
         Intercept >= macro$lk_min_intercept,
         Intercept <= macro$lk_max_intercept)%>%
  left_join(rki_new %>% select(Bundesland, Landkreis) %>% unique()) %>%
  group_by(Bundesland) %>%
  mutate(
    threshold = quantile(incidence,probs = 1 - macro$lk_quantile_threshold,na.rm = TRUE) )%>%
    filter(incidence>=threshold) %>%
  ungroup()%>%
  pull(Landkreis) %>% unique()

message("Units selected:", length(cur.kreis))

kreis.w.treatment <- intervention.list%>%filter(adm.level==3,start<=macro$end_date)%>%pull(Landkreis)%>%unique()

data <- rki_new %>%
  mutate(Bundesland = as.character(Bundesland),
         Bundesland = if_else(Bundesland=="ThÃ¼ringen","Thüringen",Bundesland),
         Bundesland = if_else(Bundesland=="Baden-WÃ¼rttemberg","Baden-Württemberg",Bundesland))%>%
  filter(!age %in% macro$age.drop,
         Refdatum > as.Date(macro$data_start))

if(macro$include_only_treatment_BL)
  data <- data %>% filter(Bundesland %in%
                            unique( idat %>%
                                      filter(all_interventions_exist)%>%
                                      pull(Bundesland)))


if(macro$include_treatment_lk){ # add all subregions with treatment if regions also in data
  data <- data %>%
    filter(
      Landkreis %in% c(cur.kreis,
                       "SK Hamburg","Berlin",
                       kreis.w.treatment) &
        Bundesland %in% unique(data$Bundesland))
} else {
  data <- data %>%
    filter(
      Landkreis %in% c(cur.kreis,
                       "SK Hamburg","Berlin"))
}

data <- data %>%
  filter(!Landkreis %in% exclude_locations)


data <- data %>%
  mutate(name = Landkreis) %>%
  group_by(Refdatum,name,age,Bundesland)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Refdatum,
  )%>%ungroup()


if(is.Date(macro$end_date)) # if end_date given, filter
  data <- data %>% filter(date<=macro$end_date)

message(paste("Case data loaded. Number of regions under consideration is",
              length(unique(data$name))))




# Add IdBundesland and IdLandkreis ------------------------------------------------------

# find Bundesland names and ids
names.bl <- CovidGer::rki_new %>%
  select(Bundesland,IdBundesland)%>%
  mutate(Bundesland = as.character(Bundesland),
         Bundesland = if_else(Bundesland=="ThÃ¼ringen","Thüringen",Bundesland),
         Bundesland = if_else(Bundesland=="Baden-WÃ¼rttemberg","Baden-Württemberg",Bundesland))%>%unique()

# add ids
data <- data %>% left_join(names.bl)

# find Kreis names and ids
names.lk <- CovidGer::rki_new %>%
  select(Landkreis,IdLandkreis)%>%
  unique()

# add ids
data <- data %>% left_join(names.lk, by = c("name"="Landkreis"))


# expand into past --------------------------------------------------------
dim(data)
if(is.factor(data$name))
  data$name <- droplevels(data$name)
if(is.factor(data$age))
  data$age <- droplevels(data$age)

new <-  plyr::rbind.fill(data,
                         data.frame(
                           date=seq.Date(macro$expand_dates_from,
                                         max(data$date),by = "days")))

data <- new %>%
  tidyr::expand(date,name,age)%>%
  filter(!is.na(name),!is.na(date),!is.na(age))%>%
  left_join(data %>% select(name,Bundesland,IdBundesland,IdLandkreis) %>% unique())  %>%
  left_join(data %>% select(c(name,date,age,pos.new,dead.new)))

message(paste("Number of regions under considertation is ",length(unique(data$name))))

# add population data -----------------------------------------------------
# merge with regionaldatenbank

# prepare id
data$adm.level <- ifelse(is.na(data$IdLandkreis),2,3)
data <- data %>% mutate(IdLandkreis = as.character(IdLandkreis),
                        IdBundesland = as.character(IdBundesland))
data$id <- ifelse(data$adm.level==2, data$IdBundesland, data$IdLandkreis)



regionaldatenbank$id <- as.character(regionaldatenbank$id)




data <- data %>%
  left_join(regionaldatenbank%>%
              group_by(name) %>%
              mutate(pop_c = sum(total))%>%
              ungroup()%>%
              select(-c(name)))%>%
  rename(
    pop = total
  ) %>% ungroup()

# check for nas in population data
if(nrow(data %>%  filter(is.na(pop)) %>% select(name,IdBundesland,IdLandkreis,adm.level) %>% unique())>0)
  warning(paste("Dropping data as population data missing for",
                list(data %>%  filter(is.na(pop)) %>% pull(name) %>% unique() )))

data <- data %>% filter(!is.na(pop))

# Add delay ----------------------------------------------------------
# add by Refdatum

# compute delay in reporting and cumulative sum by Refdatum
vars <- rki_new %>%
  filter(age %in% unique(data$age),
         Landkreis %in% unique(data$name),
         Refdatum<=max(data$date),
         Refdatum > macro$data_start)%>%
  mutate(name = Landkreis,
         delay_ind = as.numeric(Meldedatum-Refdatum),
         delay_ind = if_else(delay_ind>14,14,delay_ind),
         delay_ind = if_else(delay_ind<(-7),-7,delay_ind)) %>%
  group_by(Refdatum,name)%>%
  summarise(
    pos = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    delay = mean(delay_ind,na.rm=TRUE),
    delay1n = mean(delay_ind<=-1,na.rm = TRUE),
    delay0 = mean(delay_ind==0,na.rm = TRUE),
    delay1 = mean(delay_ind==1,na.rm = TRUE),
    delay2 = mean(delay_ind==2,na.rm = TRUE),
    delay3 = mean(delay_ind==3,na.rm = TRUE),
    delay4 = mean(delay_ind==4,na.rm = TRUE),
    delay5 = mean(delay_ind==5,na.rm = TRUE),
    delay6 = mean(delay_ind==6,na.rm = TRUE),
  ) %>%
  ungroup() %>%
  complete(Refdatum = full_seq(Refdatum, period = 1),
           name,
           fill = list(pos = 0,delay=NA)) %>%
  group_by(name) %>%
  arrange(Refdatum)%>%
  mutate(
    pos.temp = if_else(is.na(pos),0,as.numeric(pos)),
    cumsum = cumsum(pos.temp)
  ) %>%
  select(-c(pos,pos.temp))%>%
  ungroup()


### smooth delay if NA
if("delay" %in% macro$cov.other){
  message(paste0("Ratios of NA in delay that is smoothed is ",mean(is.na(vars%>%filter(Refdatum>macro$data_start)%>%pull(delay)))))
}

vars <- vars %>%
  mutate(week=lubridate::isoweek(Refdatum))%>%
  group_by(week,name)%>%
  mutate(across(contains("delay"),
         function(x) if_else(is.na(x),mean(x,na.rm=TRUE),x))
  )%>%
  ungroup()%>%
  group_by(week)%>%
  mutate(across(contains("delay"),
                function(x) if_else(is.na(x),mean(x,na.rm=TRUE),x))
  )%>%ungroup()%>%
  rename(
    date = Refdatum
  )


# merge to to data based on date of symptom onset
data <- data %>%
  left_join(vars %>% select(-(cumsum)))

# Add cumsum --------------------------------------------------------------
# with lag of macro$timediff_cumsum days

data <- data %>%
  left_join(vars%>%
              select(name,date,cumsum)%>%
              mutate(date = date + as.difftime(macro$timediff_cumsum,
                                               units = "days")))%>%
  mutate(
    cumsum = if_else(is.na(cumsum),0,cumsum),
    cumsum_incidence100 = cumsum/pop_c*100
  )

# Add info (by Meldedatum) ----------------------------------------------------------
# information publicly available at date
# compute local 7-day incidence

info <- rki_new %>%
  filter(age %in% unique(data$age),
         Landkreis %in% unique(data$name))%>%
  mutate(name = Landkreis) %>%
  group_by(Meldedatum,name)%>%
  summarise(
    pos.new = sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])) %>%
  ungroup() %>%
  rename(
    date = Meldedatum
  )

# last seven days
info <- info %>%
  complete(name,
           date = full_seq(date, period = 1),
           fill = list(pos.new = 0,dead.new=0)) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(pos7 = zoo::rollapplyr(pos.new, width = 7, FUN = sum,align = "right", partial = TRUE),
         dead7 = zoo::rollapplyr(dead.new, width = 7, FUN = sum,align = "right", partial = TRUE)) %>%
  ungroup()

# merge to to data with macro$info_lag days difference
data <- data %>%
  left_join(
    info %>%
      select(-c(pos.new,dead.new))%>%
      mutate(date = date +
               lubridate::as.difftime(macro$info_lag,
                                      units = "days")))

# add ratio of traced cases -------------------------------------------------

# smooth over week if no cases observed
df.reported <- CovidGer::traced %>%
  filter(name %in% unique(data$name))%>%
  group_by(name)%>%
  arrange(date)%>%
  mutate(traced_smooth_name7 = zoo::rollapplyr(traced, width = 7, FUN = mean, partial = TRUE,na.rm=TRUE),
         traced_smooth_name14 = zoo::rollapplyr(traced, width = 14, FUN = mean, partial = TRUE,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(date)%>%
  mutate(traced_smooth_7 = zoo::rollapplyr(traced, width = 7,FUN = mean, partial = TRUE,na.rm=TRUE),
         traced_smooth_14 = zoo::rollapplyr(traced, width = 14, FUN = mean, partial = TRUE,na.rm=TRUE))%>%
  ungroup()



if("traced" %in% macro$cov.other){
  message(paste0("Ratios of NA in traced that is smoothed is ",mean(is.na(df.reported%>%filter(date>macro$data_start)%>%pull(traced)))))
}

df.reported <- df.reported %>%
  mutate(traced = if_else(is.na(traced),traced_smooth_name7,traced),
         traced = if_else(is.na(traced),traced_smooth_name14,traced),
         traced = if_else(is.na(traced),traced_smooth_7,traced),
         traced = if_else(is.na(traced),traced_smooth_14,traced))%>%
  ungroup()

# merge to main data
data <- data %>%
  left_join(df.reported%>%select(date,name,traced))%>%
  mutate(traced = if_else(is.na(traced),0,traced))


# na to zero --------------------------------------------------------------
data <- data %>%
  mutate(
    dead.new = ifelse(is.na(dead.new),0,dead.new),
    pos.new = ifelse(is.na(pos.new),0,pos.new),
    pos7 = ifelse(is.na(pos7),0,pos7),
    dead7 = ifelse(is.na(dead7),0,dead7))

# add weather data --------------------------------------------------------

# load weather data
wdat <- as_tibble(weather_dwd)
wdat$date <- as.Date(wdat$date)

wdat <- wdat %>% rename(admin.code = admin3.code)

if(macro$pseudo.weather){ # use average weather from past
  ### drop data from 2020
  wdat <- wdat %>%
    filter(lubridate::year(date)!="2020")

  # average for each date ignoring year
  wdat <- wdat %>%
    mutate(
      day = as.character(lubridate::day(date)),
      day = if_else(nchar(day)==1,paste0("0",day),day),
      month = as.character(lubridate::month(date)),
      month = if_else(nchar(month)==1,paste0("0",month),month),
      date = as.Date(paste0(day,month,"2020"),format = "%d%m%Y"))%>%
    group_by(date,admin.code)%>%
    summarise_at(vars(macro$cov.weather),mean, na.rm=TRUE)%>%ungroup()

  # add 29-02-2020 by copying date before
  wdat_sub <- wdat %>% filter(date==as.Date("2020-02-28"))
  wdat_sub$date <- as.Date("2020-02-29")
  wdat <- rbind(wdat,wdat_sub)
} else {
  # use weather from 2020
  wdat <- wdat %>%
    filter(date>=min(data$date))
}

### merge to data
wdat$admin.code <- as.character(wdat$admin.code)
data <- data %>% left_join(wdat%>%select(-name), by= c("date","IdLandkreis"="admin.code"))

### check for missing data
data <- data %>% group_by(date) %>%
  mutate(
    x.exists.t = !any(is.na(vars(macro$cov.weather)))
  ) %>% filter(x.exists.t) %>% ungroup()

message(paste("Weather data missing from date ",max(data$date)))
message(paste("Number of regions under considertation is ",length(unique(data$name))))


# add factor dummies ------------------------------------------------------
macro$dummies.factors <- NULL
for(var.cur in macro$cov.factorize){
  data$temp <- factorize(data %>% pull(var.cur),
                         length = macro$n_factors,
                         digits=1)
  for(i in 1:(macro$n_factors-1)){
    var.i.cur <- paste0(var.cur,"|",i,"|",levels(data$temp)[i])
    data[,var.i.cur] <- data$temp == levels(data$temp)[i]
    macro$dummies.factors <- c(macro$dummies.factors,var.i.cur)
  }
}


# add intervention dummies ---------------------------------------------------

### drop observations without treatment
data <- data %>%
  mutate(unit = ifelse(name %in% kreis.w.treatment, name, Bundesland))%>%
  filter(unit %in% unique(idat$unit))

### merge to data
data <- data %>% left_join(idat, by= c("Bundesland","date","unit"="unit"))

message(paste("Treatment available for ", length(unique(data$name)), "units of observation."))

# Define in_model ---------------------------------------------------------

### Compute pos.total
data <- data %>%
  group_by(name) %>%
  arrange(date)%>%
  mutate(
    pos.total = cumsum(pos.new)
  ) %>% ungroup()

if(is.null(macro$total_limit)){
  if(is.null(macro$start_date))
    stop("Define total limit or start date")
  macro$total_limit <- 0
}
### in_model for date
data <- data %>% group_by(name,date) %>%
  mutate(
    in_model = pos.total >= macro$total_limit
  ) %>% ungroup()

# drop names without observations
data <- data %>% group_by(name) %>%
  mutate(in_model_total = sum(in_model)>0) %>%
  ungroup() %>%
  filter(in_model_total)
message(paste("After dropping regions below macro$total_limit ", macro$total_limit, "the number of regions under considertation is ",length(unique(data$name))))



# drop unnecessary obseravations ------------------------------------------
# drop beyond end_date and before start date - days considered in model
# if no start date given, use first in_model determined by total_limit

if(is.Date(macro$start_date)){
  data <- data %>%
    filter(date <= macro$end_date,
           date >= macro$start_date-lubridate::as.difftime(max_past+1,units = "days"))
} else {
  temp.date <- data %>%
    filter(in_model) %>%
    summarize(min(date))%>%
    pull()
  data <- data %>%
  filter(date <= macro$end_date,
         date >= temp.date-lubridate::as.difftime(max_past+1,units = "days"))
}

# gen t -------------------------------------------------------------------
data$t <- as.numeric(data$date-min(data$date)+1)

# gen reports -------------------------------------------------------------
data <- data %>% arrange(age,name,t)
reports <- array(data %>% pull(pos.new),
                 dim = c(length(unique(data$t)),
                         length(unique(data$name)),
                         length(unique(data$age))),
                 dimnames = list( unique(data$t),
                                  unique(data$name),
                                  unique(data$age))
)

# gen dead -------------------------------------------------------------
data <- data %>% arrange(age,name,t)
dead <- array(data %>% pull(dead.new),
              dim = c(length(unique(data$t)),
                      length(unique(data$name)),
                      length(unique(data$age))),
              dimnames = list( unique(data$t),
                               unique(data$name),
                               unique(data$age))
)


# gen pop -------------------------------------------------------------
datas <- data %>%
  select(age,name,pop) %>%
  unique() %>%
  arrange(age, name)
pop <- array(datas %>%
               pull(pop),
             dim = c(length(unique(datas$name)),
                     length(unique(datas$age))),
             dimnames = list(unique(datas$name),
                             unique(datas$age))
)



# define time intervals ---------------------------------------------------

### END
if(!is.Date(macro$end_date)){# choose latest as end date
  T <- data %>%
    group_by(name) %>%
    summarize(max(t))%>%
    pull()
} else {  # Choose specific end date
  T <- data %>%
    filter(date == as.Date(macro$end_date)) %>%
    select(name,t)%>%
    unique()%>%
    arrange(name)%>%
    pull(t)
}

### Start

if(!is.Date(macro$start_date)){# Automatic with in_model
  Ti <- data %>%
    filter(in_model) %>%
    group_by(name) %>%
    summarize(min(t))%>%
    pull()
  macro$Ti.dates <- data %>%
    filter(in_model) %>%
    group_by(name) %>%
    summarize(min(date))
  message(paste("Starting dates: ", range(macro$Ti.dates$`min(date)`)))
} else { # Specific date
  Ti <- data %>%
    filter(date == as.Date(macro$start_date)) %>%
    select(name,t)%>%
    unique()%>%
    arrange(name)%>%
    pull(t)
}

Tinit <- numeric(ncol(reports))
for(c in 1:dim(reports)[2]){
  Tinit[c]<- Ti[c]-macro$days_init
}

names(Ti)<-colnames(reports)
names(Tinit)<-colnames(reports)

message(paste(T-Ti,collapse = " "))

if(sum(Tinit<1)>0)
  stop("Tinit is negative, add observations with parameter expand_dates_from!")

range(data$date)




# gen xd -----------------------------------------------------

### add time FE
data <- data %>%
  mutate(week = lubridate::isoweek(date))

FEweeks <- unique(data$week[data$t>=min(Ti) & data$t<=max(T)])[-1]
for(t.cur in FEweeks){
  data[paste0("FE",t.cur)] <- data$week == t.cur
}

# add FE names to covariate.info.d
if(!is.null(macro$time_FE)){
  if(macro$time_FE){
    macro$dummies.FE <- paste0("FE", FEweeks)
  }
} else {
  macro$dummies.FE <- NULL
}

### add wday FE
data <- data %>%
  mutate(wday = lubridate::wday(date,label=TRUE))

FEwdays <- unique(data$wday)[-1]
for(d.cur in FEwdays){
  data[paste0("FE",d.cur)] <- data$wday == d.cur
}

# add wday FE names to dummies.FE
if(!is.null(macro$wday_FE)){
  if(macro$wday_FE){
    macro$dummies.FE <- c(macro$dummies.FE,
                          paste0("FE", FEwdays))
  }
}

### gather dummy variables
macro$dummies <- c(macro$dummies.intervention, macro$dummies.FE,macro$dummies.factors)

### generate info dummies
data <- data %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    tempp = pos7>0,
    firstp = min(date[tempp],na.rm = TRUE),
    info_firstcase = date>=firstp,
    tempd = dead7>0,
    firstd = min(c(date[tempd],max(data$date),na.rm = TRUE)),
    info_firstdead = date>=firstd
  ) %>% select(-c(tempp,tempd,firstp,firstd)) %>%
  ungroup()


### generate covariate array

# initiliaze array with dimensions = (t,c,m)
xd <- array(NA, dim = c(length(unique(data$t)),
                        length(unique(data$name)),
                        length(unique(macro$dummies))),
            dimnames = list( unique(data$t),
                             unique(data$name),
                             unique(macro$dummies)))

for(cov.cur in macro$dummies){
  x.cur <- array(data %>%
                   group_by(name,t) %>%
                   filter(row_number()==1) %>%
                   arrange(name, t) %>%
                   pull(cov.cur),
                 dim = c(length(unique(data$t)),
                         length(unique(data$name))),
                 dimnames = list( unique(data$t),
                                  unique(data$name))
  )

  xd[,,cov.cur] <- x.cur
}


#xd[,,1]%>%View()


# gen x-----------------------------------------------------


# combine covariates from weather and info
macro$cov <- setdiff(c(macro$cov.weather,
                       macro$cov.info,
                       macro$cov.other),
                     macro$cov.factorize)

temp.pop.Ger <- regionaldatenbank%>%filter(adm.level==1)%>%summarise(sum(total))%>%pull()

### generate info covariates
data <- data %>%
  mutate(
    info_pos = pos7,
    info_dead = dead7,
    info_lpos = log(info_pos+1,base = 10),
    info_ldead = log(info_dead+1,base = 10),
    info_incidence = pos7/pop_c*1e5,
    info_lincidence = log(pos7/pop_c*1e5+1,base = 10),
    info_trueincidence = log(7*pos.new/pop+1,base = 10),
    incidence = pos.new/pop_c
  ) %>%
  group_by(date) %>%
  mutate(info_Gerlpos = log(sum(pos7)+1,base = 10),
         info_Gerldead = log(sum(dead7)+1,base = 10),
         info_Gincidence = sum(pos7)/temp.pop.Ger,
         info_Glincidence = log(info_Gincidence+1,base = 10)) %>%
  ungroup()

### generate covariate array

# initiliaze array with dimensions = (t,c,a,m)
x <- array(NA, dim = c(length(unique(data$t)),
                       length(unique(data$name)),
                       length(unique(macro$cov))),
           dimnames = list( unique(data$t),
                            unique(data$name),
                            unique(macro$cov)))


df.standardize <- data.frame()
for(cov.cur in macro$cov){
  dats <- data %>%
    filter(age == unique(age)[1]) %>%
    arrange(name, t)
  x.cur <- array( dats %>% pull(cov.cur),
                  dim = c(length(unique(data$t)),
                          length(unique(data$name))),
                  dimnames = list( unique(data$t),
                                   unique(data$name))
  )

  # standardize if not in cov.not.normalize
  if(cov.cur %in% setdiff(macro$cov,macro$cov.not.normalize)){
    x.in.model <- dats%>%
      filter(t>=min(Ti), t<=max(T))%>%
      pull(cov.cur)
    mean.cur <- mean(x.in.model,na.rm=TRUE)

    # ignore mean if inn cov.not.mean
    if(cov.cur %in% macro$cov.not.demean)
      mean.cur <- 0

    sd.cur <- sd(x.in.model,na.rm=TRUE)

    # safe normalization
    df.standardize <- rbind(df.standardize,
                            data.frame(cov=cov.cur,mean=mean.cur,sd=sd.cur))
    x.cur <- (x.cur-mean.cur)/sd.cur
  }

  # if(sum(is.na(x.cur[min(Ti):max(T),]))>0)
  #   warning(paste(sum(is.na(x.cur[min(Ti):max(T),])),
  #                 "NA for covariate",
  #                 cov.cur, "substituted by mean of covariate"))
  #
  # x.cur[is.na(x.cur)] <- mean(x.cur,na.rm=TRUE)

  x[,,cov.cur] <- x.cur
}
macro$df.standardize <- df.standardize


# data list ---------------------------------------------------------------
dat <- list("T" = T,
            "reports" = reports,
            "tau_max"=macro$tau_max,
            "R_mean"=R_mean,
            "R_sd"=R_sd,
            "effect_sd"=macro$effect_sd,
            "ar_sd"=macro$ar_sd,
            "xd"=xd,
            "mean_SI_mean"=macro$mean_SI_mean,
            "mean_SI_sd"=macro$mean_SI_sd,
            "sd_SI"=macro$sd_SI,
            "mean_trans_mean"=macro$mean_trans_mean,
            "mean_trans_sd"=macro$mean_trans_sd,
            "sd_transmission"=macro$sd_transmission,
            "Tinit"=Tinit,
            "Ti"=Ti,
            "x"=x,
            "dummies" = macro$dummies,
            "cov" = macro$cov,
            "tau_mean" = macro$tau_mean,
            "tau_sd" = macro$tau_sd,
            "srate_mean"=macro$srate_mean,
            "srate_sd"=macro$srate_sd,
            "na" = dim(reports)[3],
            "nc" = dim(reports)[2],
            "nt" = dim(reports)[1],
            "nx" = dim(x)[3],
            "nxd" = dim(xd)[3],
            "max_past" = max_past,
            "disp_mean" = macro$disp_mean,
            "disp_sd" = macro$disp_sd,
            "error_sd" = macro$error_sd
            )


# +++ JAGS +++ ----------------------------------------------------------------
monitor <- c("beta_effect",
             "beta_effectxd",
             "Rzero",
             "mean_SI",
             "mean_transmission",
             "tau",
             "i_disp",
             "transmission_dist",
             "SI_dist",
             "out_Rerror_c",
             "out_Rerror_a",
             "out_Rerror_t",
             "out_Rerror_sd"
)

# Execute -------------------------------------------------------------------

cat(macro$notes)

message(paste("Starting sampling with",macro$MCMC_n_adapt,macro$MCMC_n_burn,macro$MCMC_n))
message(paste("Number of units is",dim(dat$reports)[2]))

# if no continuous x covariates, use other model
if(dim(dat$x)[3]==0)
  model_path <- gsub(paste0("jags ",macro$model_name,".R"),
                     paste0("jags ",macro$model_name,"X.R"),
                     model_path)

start.time <- Sys.time()
if(!test | server){ # parallel
  cl <- makePSOCKcluster(macro$MCMC_chains)
  samps.list <- jags.parfit(cl,
                          model= model_path,
                      data=dat,
                      n.adapt = macro$MCMC_n_adapt,
                      n.update = macro$MCMC_n_burn,
                      n.chains= macro$MCMC_chains,
                      params =
                        monitor,
                      thin = macro$MCMC_thin,
                      n.iter = macro$MCMC_n)
  stopCluster(cl)
  samps <-samps.list
} else { # non parallel
  message("Run not in parallel.")
  samps.list<- R2jags::jags(
              model= model_path,
              data=dat,
              n.burnin = macro$MCMC_n_burn,
              n.chains = macro$MCMC_chains,
              parameters.to.save =
                monitor,
              n.thin = macro$MCMC_thin,
              n.iter = macro$MCMC_n)
  samps <- as.mcmc(samps.list)
  }
macro$time.passed <- Sys.time()-start.time
message(paste("Sampling finished after", macro$time.passed,units(macro$time.passed)))

res <- summary(samps)
resMCMC <- MCMCsummary(samps.list)

if(server){ # save with date
  files <- list.files("/home/quamet/patrick/submission/save/")
  name.cur <- paste0(macro$file_name,substr(Sys.Date(),6,10))
  num.files <- sum(grepl(name.cur,files))
  if(num.files>0)
    name.cur <- paste0(name.cur,rep("X",num.files))
  path.save <- paste0("/home/quamet/patrick/submission/save/",name.cur,".RData")
} else{
  path.save <- paste0("./jags models/save/",macro$file_name,".RData")
}

save(macro, samps.list, res,resMCMC,
     dat,data,samps,
     file = path.save)
macro$time.passed <- Sys.time()-start.time
message(paste("Saving finished after", macro$time.passed,units(macro$time.passed)))
message(paste("Safed to", macro$file_name,"at",Sys.time()))
