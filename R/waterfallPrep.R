#' Prepares mulitplicative data for a waterfall plot
#'
#'waterfallPrep() : a function that takes the data on the reported and evaluated savings values and returns the average permuted values of the multiplicative factors and prepare a data frame that is ready for the waterfall plot in R or Excel
#'
#' @param df a dataframe with columns 'params' (character) and 'value' (numeric); column names are immaterial. This is the order dependent value.
#' @param gross.report is the reported gross (ex ante) savings for the program, default is 100
#' @param NTG.report is the percentage of gross assumed net for program, default is 1
#' @param NTG.eval is the percentage of gross evaluated net for program, default is 1
#' @param altparamnames is an optional vector of names for the parameters, default is the names in df[,1]; if supplied, ensure same order as df
#' @param output is an optional request for table, takes "none","gross","net","all", default is "all"
#' @return if output is "all", returns a list of 3 dataframes. Else, if output is "gross", or "net", returns a dataframe with columns variable (character), given, total, calc, decrease, increase, and base (numeric). If output is "none" returns the same, less "calc".
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
waterfallPrep <- function(df, gross.report=100, NTG.report=1, NTG.eval=1,
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

  ########################################
  #
  # now permute the params with internal function wParamPermute()
  #
  ########################################

  #g <- wParamPermute(param.names = param.names, values = df[,2])
  # g has the order independent values for each parameter!!!!
  # as of Feb 19, 2017, include ntg.rr in permutation with parameters
  gx <- wParamPermute(param.names = c(param.names,"NTG.RR"), values = c(df[,2], NTG.eval/NTG.report))
  gh <- wParamPermute(param.names = c("NTG.XA",param.names,"NTG.RR"),
                      values = c(NTG.report, df[,2], NTG.eval/NTG.report))

  ########################################
  #
  # Put the permuted DF together
  #
  ########################################

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
  none <- filter(givendf, variable %ni% nonedropvars) %>% # remove vars
    mutate(total = given) # get the first var, others will be overwritten
  none$decrease <- none$increase <- none$base <- NA
  for (i in 2:nrow(none)) {
    none$total[i] <- ifelse(is.na(none$given[i]), none$total[i-1],
                            none$given[i]*none$total[i-1])
    none$decrease[i] <- ifelse(none$given[i] >=1, 0, none$total[i-1] - none$total[i])
    none$increase[i] <- ifelse(none$given[i] < 1, 0,none$total[i] - none$total[i-1])
    # excel requires no change in base for positive change
    none$base[i] <- ifelse(none$variable[i] %in% totalvars, NA,
                           ifelse(none$decrease[i] == 0, none$total[i-1],
                                  none$total[i]))
  }
  none$total <- ifelse(none$variable %in% totalvars,
                                none$total,
                                NA)
  # make gross.permute table for EXCEL Waterfall
  # columns: total base decrease increase
  gross.permute <- filter(givendf, variable %ni% nonedropvars) %>% # remove vars
    mutate(total = given) # get the first var, others will be overwritten
    gross.permute$calc <- gross.permute$decrease <- gross.permute$increase <- gross.permute$base <- NA

  for (i in 2:nrow(gross.permute)) {
    if(gross.permute$variable[i] %in% param.names){ # calc increase and decrease
      gross.permute$calc[i] <- as.numeric(g$avg.xx[i-1])# in same order but no gross
      gross.permute$decrease[i] <- ifelse(gross.permute$calc[i] < 0,
                                  gross.permute$given[1]*gross.permute$calc[i]*(-1),
                                          0)
      gross.permute$increase[i] <- ifelse(gross.permute$calc[i] > 0,
                                  gross.permute$given[1]*gross.permute$calc[i],
                                          0)
    } else if(gross.permute$variable[i]=="NTG.XP"){
      gross.permute$decrease[i] <- ifelse(gross.permute$given[i] < 1,
                                  gross.permute$total[i-2]*
                                    (1-gross.permute$given[i]),
                                          0)
      gross.permute$increase[i] <- ifelse(gross.permute$given[i] > 1,
                                  gross.permute$total[i-2]*
                                    (gross.permute$given[i]-1),
                                          0)
    } else {
      gross.permute$calc[i] <- NA
      gross.permute$increase[i] <- NA
      gross.permute$decrease[i] <- NA
    }
    if(gross.permute$variable[i] %in% param.names){
      gross.permute$total[i] <- gross.permute$total[i-1]-
        gross.permute$decrease[i] + gross.permute$increase[i]
      } else if(gross.permute$variable[i]=="NTG.XP"){
      gross.permute$total[i] <- gross.permute$total[i-2]-
        gross.permute$decrease[i] + gross.permute$increase[i]
        } else if(gross.permute$variable[i] %in% c("Gross.XP","Net.XP")){
      gross.permute$total[i] <- gross.permute$total[i-1]
    }
    # excel requires no change in base for positive change
    gross.permute$base[i] <- ifelse(gross.permute$variable[i] %in% totalvars, NA,
                                    ifelse(gross.permute$decrease[i] == 0,
                                           gross.permute$total[i-1],
                                           gross.permute$total[i]))
  }
  gross.permute$total <- ifelse(gross.permute$variable %in% totalvars,
                                gross.permute$total,
                                NA)

  # make net.permute table for EXCEL Waterfall
  # columns: total base decrease increase
  net.permute <-
    filter(givendf, variable %ni% "Gross.XP") %>% # remove vars
    mutate(total = given) # get the first var, others will be overwritten

  net.permute$total[3] <- net.permute$total[1] *
    net.permute$total[2]
  net.permute$calc <- NA
  net.permute$decrease <- net.permute$increase <- net.permute$base <- NA

  net.permute$calc[net.permute$variable %in% param.names] <-
    as.numeric(gx$avg.xx[which(gx$param.names != "NTG.RR")])
    #as.numeric(g2$avg.p)
  net.permute$calc[net.permute$variable == "NTG.XP"] <-
    as.numeric(gx$avg.xx[which(gx$param.names == "NTG.RR")])
  net.permute$decrease <- ifelse(net.permute$calc < 0,
                                 net.permute$total[3] * net.permute$calc *
                                   (-1),
                                 0)
  net.permute$increase <- ifelse(net.permute$calc > 0,
                                 net.permute$total[3] * net.permute$calc,
                                 0)
  net.permute$total[4] <- ifelse(net.permute$decrease[4]==0,#an increase
                                 net.permute$total[3],
                                 net.permute$total[3]*(1+net.permute$calc[4]))
  for (i in 5:nrow(net.permute)) {
    if (net.permute$variable[i] %in% c(param.names,"NTG.XP")) {
      net.permute$total[i] <- ifelse(
        net.permute$decrease[i] == 0, # an increase
        net.permute$total[3] -
          sum(net.permute$decrease[4:(i - 1)], na.rm=TRUE) +
          sum(net.permute$increase[4:(i - 1)], na.rm=TRUE),
        (net.permute$total[3] -
          sum(net.permute$decrease[4:(i - 1)], na.rm=TRUE) +
          sum(net.permute$increase[4:(i - 1)], na.rm=TRUE))+
          net.permute$total[3]*net.permute$calc[i] #net+dif+this dif
      )
        } else if(net.permute$variable[i] %in% c("Net.XP")){
      net.permute$total[i] <- ifelse(net.permute$calc[i-1] < 0,
                                net.permute$total[i-1],
                                net.permute$total[i-1]+net.permute$increase[i-1])
        }
  }
    # excel requires no change in base for positive change
  net.permute$base <-
    ifelse(net.permute$variable %in% c(totalvars,"NTG.XA"),
           NA,
           net.permute$total)
  net.permute$total <- ifelse(net.permute$variable %in% totalvars,
                              net.permute$total,
                              NA)
  net.permute$base[net.permute$variable == "NTG.XA"] <-
    net.permute$total[1] * net.permute$given[2]
  net.permute$decrease[net.permute$variable == "NTG.XA"]  <-
    ifelse(net.permute$given[net.permute$variable == "NTG.XA"] < 1,
           net.permute$total[1] *
             (1 - net.permute$given[net.permute$variable == "NTG.XA"]),
           0)
  net.permute$increase[net.permute$variable == "NTG.XA"]  <-
    ifelse(net.permute$given[net.permute$variable == "NTG.XA"] > 1,
           net.permute$total[1] *
             (net.permute$given[net.permute$variable == "NTG.XA"] - 1) ,
           0)
  net.permute$variable[net.permute$variable == "NTG.XP"] <- "NTG.RR"

  # make hybrid.permute table for EXCEL Waterfall
  # columns: total base decrease increase
  hybrid.permute <-
    filter(givendf, variable %ni% c("Gross.XP", "Net.XA")) %>% # remove vars
    mutate(total = given) # get the first var, others will be overwritten

  hybrid.permute$calc <- hybrid.permute$decrease <- hybrid.permute$increase <- hybrid.permute$base <- NA
  hybrid.permute$calc[hybrid.permute$variable == "NTG.XA"] <-
    #(-1)*(1-hybrid.permute$given[2])
    as.numeric(gh$avg.xx[which(gh$param.names == "NTG.XA")])
  hybrid.permute$calc[hybrid.permute$variable %in% param.names] <-
    as.numeric(gh$avg.xx[which(gh$param.names %ni% c("NTG.XA","NTG.RR"))])
  hybrid.permute$calc[hybrid.permute$variable == "NTG.XP"] <-
    as.numeric(gh$avg.xx[which(gh$param.names == "NTG.RR")])
  hybrid.permute$decrease <- ifelse(hybrid.permute$calc < 0,
                                 hybrid.permute$total[1] * hybrid.permute$calc *
                                   (-1),
                                 0)
  hybrid.permute$increase <- ifelse(hybrid.permute$calc > 0,
                                 hybrid.permute$total[1] * hybrid.permute$calc,
                                 0)
  hybrid.permute$base[2] <- ifelse(hybrid.permute$decrease[2]==0,#an increase
                                 hybrid.permute$total[1], # start with gross
                                 hybrid.permute$total[1]-hybrid.permute$decrease[2])

  for (i in 3:nrow(hybrid.permute)) {
    if (hybrid.permute$variable[i] %in% c(param.names,"NTG.XP")) {
      hybrid.permute$base[i] <- min(hybrid.permute$base[i-1] +
                                     ifelse(is.na(hybrid.permute$increase[i-1]),0,hybrid.permute$increase[i-1]),
                                   hybrid.permute$base[i-1] +
                                     ifelse(is.na(hybrid.permute$increase[i-1]),0,
                                            hybrid.permute$increase[i-1]) +
                                     hybrid.permute$total[1] * hybrid.permute$calc[i])
    }
     }
  hybrid.permute$total[which(hybrid.permute$variable == "Net.XP")] <-
    hybrid.permute$total[1] +sum(hybrid.permute$increase, (-1)*hybrid.permute$decrease, na.rm=TRUE)
  hybrid.permute$total <- ifelse(hybrid.permute$variable %in% totalvars,
                              hybrid.permute$total,
                              NA)
  hybrid.permute$variable[hybrid.permute$variable == "NTG.XP"] <- "NTG.RR"

  ##################################
  #
  # make pretty variable titles
  #
  ###################################
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
  hybrid.permute <- none # FOR NOW
  none <- niceTblLbl(none)
  gross.permute <- niceTblLbl(gross.permute)
  net.permute <- niceTblLbl(net.permute)
  hybrid.permute <- niceTblLbl(hybrid.permute) #todo

  # determine what to output from function
  if(output=="all"){
    return(list("No Permutatation" = none,
              "Gross Waterfall" = gross.permute,
              "Net Waterfall" = net.permute,
              "Hyrbrid Waterfall" = hybrid.permute))
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
