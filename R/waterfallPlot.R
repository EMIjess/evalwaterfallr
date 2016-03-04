#' Makes a waterfall plot
#'
#'waterfallPlot() : a function that takes a dataframe and creates a waterfall plot as a ggplot2 object which can be further modified
#'
#' @param df a dataframe with columns 'variable' (character), 'total' (numeric),  'base' (numeric), 'increase' (numeric), and 'decrease' (numeric). can have additional columns, ignored. Assumed to have starting value as first row and ending value as last row.
#' @param palette, default is c("#d7191c","#2b83ba")
#' @param xlab title the x axis, default is ""
#' @param ylab title the y axis, default is ""
#' @param xfactors label the x axis; must equal the number of variables, default is df$variable
#' @param xtextangle changes the angle of text on the x axis, default is 90
#' @param offset which sets the width of the floating segments, default is "0.3"
#' @import dplyr ggplot2 scales
#' @export
#'
#' @examples
#'
#' rrdf <- data.frame( # made up example
#'         variable = c("Start","Factor 1","Factor 2","Factor 3","End"),
#'         total = c(100, rep(NA, 3), 75),
#'         base = c(NA, 75, 50, 50,NA),
#'         increase = c(NA, 0, 0, 25, NA),
#'         decrease = c(NA, 25, 25, 0, NA))
#'  waterfallPlot(rrdf)
#'  # With another color palette. Note that totals stay grey.
#'  waterfallPlot(rrdf, palette=c("green","purple"))
#'
#' # without intermediate tables, print the gross.permute plot
#' rawparamdf <- data.frame( # lighting example
#'                          params = c("ISR","deltaWatts","HOU","x"),
#'                          value = c(.5, 1.7, .2, 1.5),
#'                          stringsAsFactors = FALSE
#'                         )
#' waterfallPlot(waterfallPrep(rawparamdf, 200, .6, .8, output="gross"))
#'
#' @return a ggplot2 object
waterfallPlot <- function(df,
                          palette=c("#d7191c","#2b83ba"),
                          xlab="" , ylab="",
                          xfactors=NULL,
                          offset=0.3,
                          xtextangle = 90) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("scales needed for this function to work. Please install it.",
         call. = FALSE)
  }
  options(scipen=999) # avoid scientific notation
  ## Add the order column to the raw data frame and order appropriately
  df <- cbind(df, order = as.numeric(rownames(df)))
  df <- transform(df, offset=offset)

  ## Create the lines data frame to link the bars
  df <- df %>%
    group_by(order) %>%
    mutate( min =  ifelse(is.na(base), total,
                          min(base, total, na.rm=TRUE)),
            max = ifelse(is.na(base), total,
                         max(base+increase, base+decrease, na.rm=TRUE)))

  df$dline <- NA
  for(i in 2:nrow(df)){
    if(!is.na(df$total[i])){ # the total bars
      df$dline[i] <- df$total[i]
    } else if(isTRUE(all.equal(df$max[i],df$max[i-1],df$min[i],df$min[i-1], tol=0.0001))){
      df$dline[i] <- ifelse(df$increase[i]==0, df$max[i], df$min[i])
      # this deals with the very special case that 2 parameters cancel out
    } else if(isTRUE(all.equal(df$max[i],df$max[i-1], tol=0.001))){
      df$dline[i] <- df$max[i]
    } else if(isTRUE(all.equal(df$max[i],df$min[i-1], tol=0.001))){
      df$dline[i] <- df$max[i]
    } else if(isTRUE(all.equal(df$min[i],df$min[i-1], tol=0.001))){
      df$dline[i] <- df$min[i]
    } else {df$dline[i] <- df$min[i]
    }}
  lines <- with(df, data.frame(x=head(order, -1),
                               xend=tail(order, -1),
                               y=tail(dline, -1)))
  lines$y[nrow(lines)] <- df$max[nrow(df)]
  lines$yend <- lines$y
  ## end lines

  ## update the xfactors for myxlabels
  if(!is.null(xfactors)){
    if(length(xfactors)==nrow(df)){
      myxlabels <- xfactors
    } else {myxlabels=df$variable} #ignore if not the right length.
  } else {myxlabels=df$variable}
  ## end xfactors for labels
  ## REQUEST (not implemented) for parameters, add their given VALUES to the label

  ## create the labelfill categories
  df$labelfill <- ifelse(is.na(df$decrease), "Total",
                         ifelse(df$decrease==0, "Increase",
                                ifelse(df$decrease>=0, "Decrease",
                                       NA)))
  colnoplot <- length(unique(df$labelfill))-1 # right now this is always 2
  myPalette = palette # there is a default or provided by user
  thisPalette <- c(colorRampPalette(myPalette)(colnoplot), "dark grey")
  maxvalue <- max(c(as.numeric(df$total),
                    as.numeric(df$base)+as.numeric(df$increase)), na.rm=TRUE)
  minvalue <- min(c(0,as.numeric(df$base)), na.rm=TRUE) #base or 0 will be minimum

  # make labels for the total bars
  # determine digits
  mydigits <- ifelse(maxvalue<=1, 2,
                     ifelse(maxvalue<=10,1,0))
  mydftext <- df %>%
    group_by(variable) %>%
    mutate(
      myneg = ifelse(total == max(total, increase, decrease, na.rm=TRUE), "",
                     ifelse(decrease == max(total, increase, decrease, na.rm=TRUE),
                     "-","")),
      mytext = paste0(myneg,round(max(total, increase, decrease, na.rm=TRUE),
                     digits=mydigits)),
      myy = max + 0.05*maxvalue
    ) %>%
    select(variable, order, mytext, myy)

  ## update the x axis text angle for myxlabels
  if(!is.null(xtextangle)){
    myxangle = as.numeric(xtextangle)
    } else {myxangle = 90}

  #plot
  gg <- ggplot() +
    geom_bar(data = df, aes(x=order, y=total),
             fill = "dark grey", stat="identity") +
    geom_rect(data=df, aes(xmin=order - offset,
                           xmax=order + offset,
                           ymin=base,
                           ymax=base+increase+decrease, fill=labelfill)) +
    geom_segment(data=lines, aes(x=x, y=y, xend=xend, yend=yend),
                 linetype="dashed")  +
    geom_text(data=mydftext, aes(x=order, y=myy, label=mytext), size=2)+
    scale_fill_manual("",values=thisPalette)+
    scale_x_continuous(breaks=unique(df$order), labels=myxlabels)+
    scale_y_continuous(labels = comma, limits=range(c(pretty(minvalue),
                                                      pretty(maxvalue)*1.05)))+
    labs(x=xlab, y=ylab) +
    theme_minimal()+
    theme(text=element_text(size=18, family="ProximaNova-Regular"),
          axis.ticks = element_blank(),
          axis.title.y=element_text(vjust=1),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          axis.text.x = element_text(angle = myxangle, vjust = 0.5, hjust=1),
          legend.position="none",
          aspect.ratio=7/10) # add an aspect ratio maintainer

  return(gg)
} # end of function
