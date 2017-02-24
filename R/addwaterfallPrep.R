#' Prepares additive data for a waterfall plot
#'
#'addwaterfallPrep() : a function that takes the data on the reported and evaluated savings values and returns the average permuted values of the additive factors and prepare a data frame that is ready for the waterfall plot in R or Excel
#'
#' @param df a dataframe with columns 'params' (character) and 'value' (numeric); column names are immaterial. This is the order dependent value.
#' @param gross.report is the reported gross (ex ante) savings for the program, default is 100
#' @param NTG.report is the percentage of gross assumed net for program, default is 1
#' @param NTG.eval is the percentage of gross evaluated net for program, default is 1
#' @param altparamnames is an optional vector of names for the parameters, default is the names in df[,1]; if supplied, ensure same order as df
#' @param output is an optional request for table, takes "none","gross","net","hybrid","all", default is "all". Note that "none" and "gross" are the same table for additive parameters.
#' @return if output is "all", returns a list of 4 dataframes. Else, if output is "gross","net", or "hybrid", returns a dataframe with columns variable (character), given, total, calc, decrease, increase, and base (numeric). If output is "none" returns the same, less "calc".
#' @import dplyr
#' @export
#' @examples
#'  rawparamdf <- data.frame( # lighting example
#'                          params = c("ISR","deltaWatts","HOU","x"),
#'                          value = c(0.5, 0.7, 1.2, 1.5),
#'                          stringsAsFactors = FALSE
#'                         )
#'  #assume gross.report is 200, NTG.report is 0.6 & NTG.eval is 0.8
#'  waterfallPrep(rawparamdf, 200, 0.6, 0.8)
#'
#'  #To just get the net permute table and rename the variables, example
#'  waterfallPrep(rawparamdf, 200, 0.6, 0.8,
#'  altparamnames=c("ISR", "Dif Watts", "Daily Hours", "Control"),
#'  output = "net")
#'
addwaterfallPrep <- function(df, gross.report=100, NTG.report=1, NTG.eval=1,
                             altparamnames = NULL,
                             output = "all") {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # check that data will be valid
  n_params <- nrow(df)
  # remove handling params warnings, in wParamPermute()

  # get the parameter labels correct
  if(!is.null(altparamnames)){
    # check to ensure same length
    if(length(altparamnames) != n_params) {
      stop("alternate parameter name vector length not the same as parameter length")
    } else {param.names <- altparamnames} # set them here
  } else {param.names <- df[,1]} # keep param names if alt not supplied



  givendf <- data.frame(stringsAsFactors=FALSE,
                        variable = c("Gross.XA","NTG.XA","Net.XA",
                                     param.names,"Gross.XP","NTG.XP","Net.XP"),
                        given = c(gross.report,NTG.report, NA,
                                  df[,2],NA,NTG.eval,NA))

  # data in givendf, g, pg
  `%ni%` <- Negate(`%in%`)  # not in operator
  totalvars <- givendf$variable[grepl("^Gr|Ne", givendf$variable)]

  # make the NO PERMUTATION TABLE for EXCEL Waterfall
  # columns: total base decrease increase
  nonedropvars <- c("Net.XA", "NTG.XA")
  NTG_mult <- NTG.eval*(gross.report + sum(df$value)) - (gross.report + sum(df$value))
  none <- filter(givendf, variable %ni% nonedropvars) %>% # remove vars
    mutate(total = given) # get the first var, others will be overwritten
  none$decrease <- none$increase <- none$base <- NA
  for (i in 2:nrow(none)) {
    none$total[i] <- ifelse(is.na(none$given[i]), none$total[i-1],
                            none$given[i]+none$total[i-1])
    none$decrease[i] <- ifelse(none$given[i] >=0, 0, none$total[i-1] - none$total[i])
    none$increase[i] <- ifelse(none$given[i] < 0, 0,none$total[i] - none$total[i-1])
    # excel requires no change in base for positive change
    none$base[i] <- ifelse(none$variable[i] %in% totalvars, NA,
                           ifelse(none$decrease[i] == 0, none$total[i-1],
                                  none$total[i]))
  }
  none$increase[which(none$variable == "NTG.XP")] <- ifelse(NTG_mult > 0, NTG_mult, 0)
  none$decrease[which(none$variable == "NTG.XP")] <- ifelse(NTG_mult < 0, abs(NTG_mult), 0)
  none$base[which(none$variable == "NTG.XP")] <- ifelse(NTG_mult > 0,
                                                        none$total[which(none$variable == "Gross.XP")],
                                                        none$total[which(none$variable == "Gross.XP")]+NTG_mult)
  none$total[which(none$variable == "Net.XP")] <- none$total[which(none$variable == "Gross.XP")]+NTG_mult
  none$total <- ifelse(none$variable %in% totalvars,
                       none$total,
                       NA)
  # make gross.permute table - no permutation on GROSS for Additive!!!
  # columns: total base decrease increase
  gross.permute <- none

  # make net.permute table
  # columns: total base decrease increase
#  if(NTG.report==1 & NTG.eval==1){
#    net.permute <- gross.permute  #if NTG is 1, no permutation necessary!
#  }else{
    dfp <- df %>%
      mutate(a = .5 * value * (NTG.report + NTG.eval))
    net.permute <- filter(givendf, variable %ni% "Gross.XP")  # remove vars
    net.permute <- left_join(net.permute, select(dfp, variable = params, a)) %>%
      mutate(total = ifelse(is.na(a), given, NA))
    # set up empty table
    net.permute$decrease <- net.permute$increase <- net.permute$base <- NA
    # calculate Net.XA
    net.permute$total[3] <- net.permute$base[3] <- net.permute$total[1] * net.permute$total[2]
    net.permute$base[2] <-  min(net.permute$total[1], net.permute$total[3])
    net.permute$increase[2] <- ifelse(net.permute$given[2] < 1, 0, net.permute$total[3]-net.permute$total[1])
    net.permute$decrease[2] <- ifelse(net.permute$given[2] > 1, 0, net.permute$total[1]-net.permute$total[3])
    for (i in 4:nrow(net.permute)) {
      if(net.permute$variable[i] %in% param.names){ # calc increase and decrease
        net.permute$decrease[i] <- ifelse(net.permute$a[i] < 0,
                                          net.permute$a[i]*(-1),
                                          0)
        net.permute$increase[i] <- ifelse(net.permute$a[i] > 0,
                                          net.permute$a[i],
                                          0)
        net.permute$base[i] <- min(net.permute$base[i-1] +
                                     ifelse(is.na(net.permute$increase[i-1]),0,net.permute$increase[i-1]),
                                   net.permute$base[i-1] +
                                     ifelse(is.na(net.permute$increase[i-1]),0,
                                            net.permute$increase[i-1]) + net.permute$a[i])
      } else if(net.permute$variable[i]=="NTG.XP"){
        NTG_mult <- 0.5*((NTG.eval/NTG.report)-1)*(2*NTG.report*gross.report + NTG.report*sum(df$value))
        net.permute$decrease[i] <- ifelse(NTG_mult < 0, NTG_mult * (-1), 0)
        net.permute$increase[i] <- ifelse(NTG_mult > 0, NTG_mult, 0)
        net.permute$base[i] <- min(net.permute$base[i-1], net.permute$base[i-1] + NTG_mult)
      } else if(net.permute$variable[i]=="Net.XP"){
        net.permute$total[i] <- net.permute$total[3]+sum(dfp$a)+ NTG_mult
      }
    }
    net.permute$total[which(net.permute$variable %ni% totalvars)] <-  NA
    net.permute$base[which(net.permute$variable %in% totalvars)] <-  NA
    net.permute$a <- NULL
    net.permute$variable[net.permute$variable == "NTG.XP"] <- "NTG.RR"
 # }

  # if(NTG.report==1 & NTG.eval==1){
  #   hybrid.permute <- gross.permute  #if NTG is 1, no permutation necessary!
  # }else{
    dfp <- df %>%
      mutate(a = (1/3) * value * (1+ NTG.eval + 0.5 *(NTG.report + NTG.eval/NTG.report)))
    hybrid.permute <- filter(givendf, variable %ni% "Gross.XP")  # remove vars
    hybrid.permute <- left_join(hybrid.permute, select(dfp, variable = params, a)) %>%
      mutate(total = ifelse(is.na(a), given, NA))
    # set up empty table
    hybrid.permute$decrease <- hybrid.permute$increase <- hybrid.permute$base <- NA
    # calculate Net.XA
    NTG_multxa <- (1/3)*(NTG.report-1)*
          ((3/2)*gross.report * (1+NTG.eval/NTG.report) + sum(df$value) * (.5 + NTG.eval/NTG.report))
    hybrid.permute$total[3] <-  hybrid.permute$base[3]<- hybrid.permute$total[1] + NTG_multxa
    hybrid.permute$base[2]  <- min(hybrid.permute$total[1], hybrid.permute$total[3])
    hybrid.permute$increase[2] <- ifelse(NTG.report < 1, 0, hybrid.permute$total[3]-hybrid.permute$total[1])
    hybrid.permute$decrease[2] <- ifelse(NTG.report > 1, 0, hybrid.permute$total[1]-hybrid.permute$total[3])
    for (i in 4:nrow(hybrid.permute)) {
      if(hybrid.permute$variable[i] %in% param.names){ # calc increase and decrease
        hybrid.permute$decrease[i] <- ifelse(hybrid.permute$a[i] < 0,
                                          hybrid.permute$a[i]*(-1),
                                          0)
        hybrid.permute$increase[i] <- ifelse(hybrid.permute$a[i] > 0,
                                          hybrid.permute$a[i],
                                          0)
        hybrid.permute$base[i] <- min(hybrid.permute$base[i-1] +
                                     ifelse(is.na(hybrid.permute$increase[i-1]),0,hybrid.permute$increase[i-1]),
                                   hybrid.permute$base[i-1] +
                                     ifelse(is.na(hybrid.permute$increase[i-1]),0,
                                            hybrid.permute$increase[i-1]) + hybrid.permute$a[i])
      } else if(hybrid.permute$variable[i]=="NTG.XP"){
        NTG_mult <- (1/3)*((NTG.eval/NTG.report)-1)*
          ((3/2)*gross.report * (1+NTG.report) + sum(df$value) * (.5 + NTG.report))
        hybrid.permute$decrease[i] <- ifelse(NTG_mult < 0, NTG_mult * (-1), 0)
        hybrid.permute$increase[i] <- ifelse(NTG_mult > 0, NTG_mult, 0)
        hybrid.permute$base[i] <- min(hybrid.permute$base[i-1], hybrid.permute$base[i-1] + NTG_mult)
      } else if(hybrid.permute$variable[i]=="Net.XP"){
        hybrid.permute$total[i] <- hybrid.permute$total[1]+sum(dfp$a)+ NTG_mult + NTG_multxa
      }
    }
    hybrid.permute$total[which(hybrid.permute$variable %ni% totalvars)] <-  NA
    hybrid.permute$base[which(hybrid.permute$variable %in% totalvars)] <-  NA
    hybrid.permute$a <- NULL
  #}
    # get rid of Net XA bar and Gross.XP (if present), not shown in Hybrid
    netvars <- c("Net.XA", "Gross.XP")
    hybrid.permute <- hybrid.permute[which(hybrid.permute$variable %ni% netvars),]
    hybrid.permute$variable[hybrid.permute$variable == "NTG.XP"] <- "NTG.RR"

  # make pretty variable titles
  niceTblLbl <- function(df) {
    df$variable[df$variable == "Gross.XA"] <- "Ex Ante Gross"
    df$variable[df$variable == "NTG.XA"] <- "Ex Ante NTG"
    df$variable[df$variable == "Net.XA"] <- "Ex Ante Net"
    df$variable[df$variable == "Gross.XP"] <- "Ex Post Gross"
    df$variable[df$variable == "NTG.XP"] <- "Ex Post NTG"
    df$variable[df$variable == "Net.XP"] <- "Ex Post Net"
    df$variable[df$variable == "NTG.RR"] <- "RR NTG"
    return(df)
  }
  none <- niceTblLbl(none)
  gross.permute <- niceTblLbl(gross.permute)
  net.permute <- niceTblLbl(net.permute)
  hybrid.permute <- niceTblLbl(hybrid.permute) #todo

  # determine what to output from function
  if(output=="all"){
    return(list("No Permutatation" = none,
                "Gross Waterfall" = gross.permute,
                "Net Waterfall" = net.permute,
                "Hybrid Waterfall" = hybrid.permute))
  } else if(output=="none"){
    return(none)
  } else if(output=="gross"){
    return(gross.permute)
  } else if(output=="net"){
    return(net.permute)
  } else if(output=="hybrid"){
    return(hybrid.permute)
  }
} # end of function
