
labels.draft <- c(
  info_lincidence = "log info inc.",
  info_incidence = "info inc.",
  eventslimited = "events lim.",
  events = "events cl.",
  `events recommendedlimited` = "events rec.",
  speechlimited = "speech",
  speechMerkel = "speech (2nd)",
  gatheringslimited = "gatherings lim.",
  cumsum_incidence100 = "cum. incidence",
  TMK.Lufttemperatur = "temperature",
  'kitas openlimited' = "daycares reopen",
  'schools openlimited' = "schools reopen",
  shops2 = "shops changed",
  UPM.Relative_Feuchte = "rel. humidity",
  sports = 'sports lim.',
  restaurants = "restaurants cl.",
  bars = "bars cl.",
  shops = "shops cl.",
  education = "schools or daycare cl.",
  gatherings = "gatherings lim.",
  'churches open' = "churches reopen",
  'schools open' = 'schools reopen',
  'kitas open' = 'daycare reopen',
  'curfew' = 'Stay-at-home order'
)


#' @export
my_labeller <- function(value){
  label.cur <- as.character(labels.draft[value])
  return(if_else(is.na(label.cur),value,label.cur))
}



labels.draft2 <- c(
  info_lincidence = "incidence (logarithm) (r,s)",
  info_incidence = "incidence (r,s)",
  eventslimited = "cancellation events recommended",
  speechMerkel = "second speech",
  gatheringslimited = "gatherings forbidden",
  cumsum_incidence100 = "cumulative incidence (%) (r)",
  TMK.Lufttemperatur = "average temperature (r,s)",
  'kitas openlimited' = "daycares reopen",
  'schools openlimited' = "schools reopen",
  shops2 = "shops changed",
  UPM.Relative_Feuchte = "relative humidity (r,s)",
  education = "schools or daycare closed",
  traced = "ratio of traced infectious (r)",
  restaurants = "restaurants closed",
  masks = "masks in public",
  speechlimited = "public awareness rising",
  shops = "non-essential shops closed",
  distance = "distancing in public",
  'churches open' = "churches reopen",
  sports = 'sports limited',
  curfew = "stay-at-home order"
)


#' @export
my_labeller2 <- function(value){
  label.cur <- as.character(labels.draft2[value])
  return(if_else(is.na(label.cur),value,label.cur))
}
#' @export
age_labels <- function(value)
  gsub("A","",value)

#' @export
print_table <- function(label){
  res.cur <- round(res$statistics[
    grepl(label,rownames(res$statistics)),],2)
  xtable::xtable(res.cur[,1:2])
}

#' @export
print_table_a <- function(label){
  res.cur <- round(res$statistics[
    grepl(label,rownames(res$statistics)),],2)
  rownames(res.cur)<- data$age%>%unique
  xtable::xtable(res.cur[,1:2])
}

#' @export
illustrate_corr <- function(data){
  cormat <- round(cor(data, use="complete.obs"),2)
  if(any(is.na(cormat))){
    print(cormat)
    stop("NA in correlation matrix")
  }
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
  # drop leading zero
  melted_cormat$value_string <- stringr::str_replace(as.character(melted_cormat$value), "0\\.", ".")
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     hjust = 1))+
    coord_fixed()

  ggheatmap +
    geom_text(aes(Var2, Var1, label = value_string), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}




#' @export
show_basics <- function()
{
  print(round(res$statistics[
    grepl("SI_dist\\[",rownames(res$statistics))|
      grepl("mean",rownames(res$statistics))|
      grepl("sd_",rownames(res$statistics))|
      grepl("disp",rownames(res$statistics))|
      grepl("dist",rownames(res$statistics)),],2))

  cat("\n Maximum Rhat: ",max(resMCMC$Rhat,na.rm = TRUE))

}



#' @source https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v4.full.pdf,
#' Assessing the Age Specificity of Infection Fatality Rates for COVID-19:
#' Systematic Review, Meta-Analysis, nd Public Policy Implications
#' @export
ifr_estimate <- function(age=45, weights = FALSE){
  if(is.numeric(age)){#return formula from paper
    if(!weights){
       return(mean(exp(-7.56 + 0.121 * age)))
    } else { #return value for weighted mean
        weights <- Covid::pop %>%
          dplyr::filter(name=="Deutschland") %>%
          select(age,total)%>%
          inner_join(data.frame(age=as.numeric(age)))
        return(weighted.mean(exp(-7.56 + 0.121 * weights$age),weights$total))
      }
    } else {
    age = as.character(age)
    if(length(age)>1){
      res <- sapply(age,ifr_estimate,weights = weights)
      return(res)
    } else{
      age <- switch(age,
          "A15-A34" = 15:34,
          "A35-A59" = 35:59,
          "A60-A79" = 60:79,
          "A80+" = 80:90)
      return(ifr_estimate(age,weights = weights))
    }
  }
}




#' Compute difference in reproduction rate between two inputs
#'
#' @param df.values data.frame with name and value1 and value2 of covariate
#' @param choose_age if age should be filtered
#' @param average if output should be weighted average across age groups
#' @param cov.cur covariates that one can choose from (standard only continuous)
#' @export
total_diff_effect <- function(df.values, choose_age = NULL, average = FALSE,cov.cur = macro$cov) {

  draws <- extract_effects(unique(df.values$m))


  if(!is.null(choose_age)){
    if(is.numeric(choose_age))
      choose_age <- dimnames(dat$reports)[[3]][choose_age]
    draws <- draws %>% dplyr::filter(age %in% choose_age)
  }


  if(average){
    draws <- draws %>% left_join(Covid::regionaldatenbank%>%
                                   dplyr::filter(adm.level==1)%>%
                                   select(age,total))%>%
      group_by(iteration,m)%>%
      summarise(draw = weighted.mean(draw,w = total))%>%
      mutate(age="average")
  }

  effect <- draws %>%
    left_join(macro$df.standardize,by=c("m"="cov"))%>%
    left_join(df.values) %>%
    mutate(mean = ifelse(is.na(mean),0,mean),
           sd = ifelse(is.na(sd),1,sd),
           value1 = (value1 - mean) / sd,
           value2 = (value2 - mean) / sd,
           effect = draw*(value1-value2),
           effect = pmin(pmax(effect,-1),1))

  effect %>%
    group_by(age,iteration)%>%
    summarise(remaining = prod(1+effect),
              effect = remaining-1)%>%
    group_by(age)%>%
    summarise(mean(effect),
              t(quantile(effect,probs=c(.025,.1,.9,.975))))
}




#' Simulate number of secondary infections for given dispersion
#'
#' @param disp dispersion
#' @param R repdructive number
#' @param chain length of chain
#'
#' Compute percentage of secondary infections from upper percentage of primary cases
#'
#' @param disp dispersion
#' @param R reproductive number
#' @param sample.size sample size for simulation
#' @param prob ratio of primary cases under consideration. 20% is standard.
#'
#' @export
compute_dispersion_percentage <- function(disp, R=1, sample.size = 1000,prob=.2){
    sample <- rnbinom(sample.size,size=disp,mu=R)
    sample <- sort(sample,decreasing = TRUE)
    sum(sample[1:ceiling(length(sample)*prob)])/sum(sample)
}
