#' Calculate water density from wtemperature
#'
#' Calculate water density from water wtemperature using the formula from (Millero & Poisson, 1981).
#' @param wwtemp vector or matrix; Water wtemperatures
#' @return vector or matrix; Water densities in kg/m3
calc_dens <- function(wwtemp){
  dens = 999.842594 + (6.793952 * 10^-2 * wwtemp) - (9.095290 * 10^-3 *wwtemp^2) + (1.001685 * 10^-4 * wwtemp^3) - (1.120083 * 10^-6* wwtemp^4) + (6.536336 * 10^-9 * wwtemp^5)
  return(dens)
}


# calculate stratified summer period
calc_strat_dur <- function(df, filter.year){
  year.ind <- which(year(df$time) == filter.year)
  filter.df <- df$sim[,year.ind]
  dens_df <- calc_dens(filter.df)
  
  dens.diff <- c()
  surf.wtemp <- c()
  for (ii in 1:ncol(dens_df)){
    dens.diff <- append(dens.diff,
                        rev(dens_df[!is.na(dens_df[,ii]),ii])[1] - dens_df[!is.na(dens_df[,ii]),ii][1])
    surf.wtemp <- append(surf.wtemp,
                         filter.df[!is.na(filter.df[,ii]),ii][1])
  }
  index <- which(dens.diff > dens.difference & surf.wtemp> 4)
  
  #Separate sequences in lists
  breaks <- c(0, which(diff(index) != 1), length(index))
  if(length(breaks) != 2){
    lst_index <- sapply(seq(length(breaks) - 1),
                        function(i) index[(breaks[i] + 1):breaks[i+1]])
    
    #Loop though to find longest sequence
    len1 = 0 #start at 0
    for(inds in 1:length(lst_index)){
      len2 = length(lst_index[[inds]])
      if(len2 > len1){
        len1 = len2
        lind = inds
      }
    }
    ind_req = lst_index[[lind]]
  } else {
    ind_req = index
  }
  return(list('length' = length(ind_req),'start' = min(ind_req),'end' = max(ind_req)))
}

# what are stratification durations?
output_stratdur <- function(df) {
  all.strat.durs <- c()
  all.begin <- ones(length(unique(year(df$time))),2)
  for (ii in 1:length(unique(year(df$time)))){
    filter.year <- unique(year(df$time))[ii]
    all.strat.durs <- append(all.strat.durs, calc_strat_dur(df, filter.year)$length)
    all.begin[ii,] <- c(calc_strat_dur(df, filter.year)$start, calc_strat_dur(df, filter.year)$end)
  }
  all.begin <- data.frame(all.begin)
  all.begin$year <- unique(year(df$time))
  return(list('duration' = all.strat.durs,
              'start' = all.begin))
}