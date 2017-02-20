#' Prepares additive data for a waterfall plot
#'
#'addwaterfallPrep() : a function that takes the data on the reported and evaluated savings values and returns the average permuted values of the additive factors and prepare a data frame that is ready for the waterfall plot in R or Excel
#'
#' @param df a dataframe with columns 'params' (character) and 'value' (numeric); column names are immaterial. This is the order dependent value.
#' @param gross.report is the reported gross (ex ante) savings for the program, default is 100
#' @param NTG.report is the percentage of gross assumed net for program, default is 1
#' @param NTG.eval is the percentage of gross evaluated net for program, default is 1
#' @param altparamnames is an optional vector of names for the parameters, default is the names in df[,1]; if supplied, ensure same order as df
#' @param output is an optional request for table, takes "none","gross","net","hybrid","all", default is "all"
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
#' @return a data frame object
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
