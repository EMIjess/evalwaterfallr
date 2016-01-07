#' Prepares data for a waterfall plot
#'
#'waterfallPrep() : a function that takes the data on the reported and evaluated savings values and returns the average permuted values of the multiplicative factors and prepare a data frame that is ready for the waterfall plot in R or Excel
#'
#' @param df a dataframe with columns 'params' (character) and 'value' (numeric); column names are immaterial. This is the order dependent value.
#' @param gross.report is the reported gross savings for the program
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
#' @return a data frame object
waterfallPrep <- function(df, gross.report, NTG.report=1, NTG.eval=1,
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

  g <- wParamPermute(param.names = param.names, values = df[,2])
  # g has the order independent values for each parameter!!!!

  ########################################
  #
  # now permute the net/gross
  #
  ########################################
  sum.g <- sum(as.numeric(g$avg.xx))
  net.tab.1 <- data.frame(
    variable = c("GRR","NTG.RR"),
    a = c(1+sum.g,NTG.eval/NTG.report)
  )
  net.tab.2 <- data.frame(
    variable = c("NTG.RR", "GRR"),
    a = c(NTG.eval/NTG.report,1+sum.g)
  )
  permutenet <- function(df){
    df$b[1] <- df$a[1]
    df$b[2] <- df$a[2]*df$b[1]
    df$c[1] <- df$b[1]-1
    df$c[2] <- df$b[2]-df$b[1]
    return(df)
  }
  net.tab.1 <- permutenet(net.tab.1)
  net.tab.2 <- permutenet(net.tab.2)
  net.tab.tot <- data.frame(
    variable = c("GRR","NTG.RR"),
    a = c(1+sum.g,NTG.eval/NTG.report),
    avg.p = c((net.tab.1$c[1]+net.tab.2$c[2])/2,(net.tab.1$c[2]+net.tab.2$c[1])/2)
  )
  if (sum.g != 0){
    g2 <- data.frame(
      param.names = param.names,
      avg.p = as.numeric(g$avg.xx)*net.tab.tot$avg.p[1]/sum.g
    )
  } else {
    g2 <- data.frame(
      param.names = param.names,
      avg.p = as.numeric(g$avg.xx)*1/NTG.report
    )
  }
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
  for (i in 2:nrow(none)) {
    none$total[i] <- ifelse(is.na(none$given[i]), none$total[i-1],
                            none$given[i]*none$total[i-1])
    none$decrease[i] <- ifelse(none$total[i-1] - none$total[i] < 0, 0,
                               none$total[i-1]  -none$total[i])
    none$increase[i] <- ifelse(none$total[i] - none$total[i-1] < 0, 0,
                               none$total[i] - none$total[i-1])
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
  net.permute$calc[net.permute$variable %in% param.names] <-
    as.numeric(g2$avg.p)
  net.permute$calc[net.permute$variable == "NTG.XP"] <-
    as.numeric(net.tab.tot$avg.p[2])
  net.permute$decrease <- ifelse(net.permute$calc < 0,
                                 net.permute$total[3] * net.permute$calc *
                                   (-1),
                                 0)
  net.permute$increase <- ifelse(net.permute$calc > 0,
                                 net.permute$total[3] * net.permute$calc,
                                 0)
  for (i in 4:nrow(net.permute)) {
    if (net.permute$variable[i] %in% c(param.names,"NTG.XP")) {
      net.permute$total[i] <- ifelse(
        net.permute$decrease[i] == 0,
        net.permute$total[3] -
          sum(net.permute$decrease[4:(i - 1)]) +
          sum(net.permute$increase[4:(i - 1)]),
        net.permute$total[i - 1] -
          net.permute$decrease[i] + net.permute$increase[i]
      )
      #        } else if(net.permute$variable[i]=="NTG.XP"){
      #        net.permute$total[i] <- ifelse(net.permute$decrease[i] == 0,
      #                                      net.permute$total[i-1],
      #                                      net.permute$total[i-1]-
      #         net.permute$decrease[i] + net.permute$increase[i])
    } else if (net.permute$variable[i] %in% c("Net.XP")) {
      net.permute$total[i] <- net.permute$total[i - 1] +
        net.permute$decrease[i - 1] +
        net.permute$increase[i - 1]
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
  # determine what to output from function
  if(output=="all"){
    return(list("No Permutatation" = none,
              "Gross Waterfall" = gross.permute,
              "Net Waterfall" = net.permute))
  } else if(output=="none"){
    return(none)
  } else if(output=="gross"){
    return(gross.permute)
  } else if(output=="net"){
    return(net.permute)
  }
} # end of function
