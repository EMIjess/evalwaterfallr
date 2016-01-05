#' Makes a waterfall plot
#'
#'waterfallPlot() : a function that takes a dataframe and creates a waterfall plot as a ggplot2 object which can be further modified
#'
#' @param df a dataframe with columns 'variable' (character), 'total' (numeric),  'base' (numeric), 'increase' (numeric), and 'decrease' (numeric). can have additional columns, ignored. Assumed to have starting value as first row and ending value as last row.
#' @param palette, default is c("#d7191c","#fdae61","#abdda4","#2b83ba")
#' @param xlab, default is ""
#' @param ylab, default is ""
#' @param offset, default is "0.3"
#' @export
#'
#' @examples
#'
#'rrdf <- data.frame( # made up example
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
#'                          value = c(0.5, 0.7, 1.2, 1.5),
#'                          stringsAsFactors = FALSE
#'                         )
#' waterfallPlot(waterfallPrep(rawparamdf, 200, .6, .8, output="gross"))
#'
#' @return a ggplot2 object
waterfallPlot <- function(df,
                          palette=c("#d7191c","#fdae61","#abdda4","#2b83ba"),
                          xlab="" ,ylab="",offset=0.3) {
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
  # create lines
  df <- df %>%
    group_by(order) %>%
     mutate( min =  min(base, total, na.rm=TRUE),
             max = max(base+increase, base+decrease, na.rm=TRUE))
  for(i in 2:nrow(df)){
    if(df$max[i]==-Inf){
      df$max[i]=df$max[i-1]
      } else {df$max[i]=df$min[i+1]}
  }
  ## Create the lines data frame to link the bars
    lines <- df %>%
      group_by(order) %>%
      mutate(liv = max(min, max, na.rm=TRUE))
    lines <- with(lines, data.frame(x=head(order, -1),
                                    xend=tail(order, -1),
                                    y=head(liv, -1),
                                    yend=head(liv, -1)))

  ## create the labelfill categories
  df$labelfill <- ifelse(is.na(df$decrease), "Total",
                         ifelse(df$decrease==0, "Increase",
                         ifelse(df$decrease>=0, "Decrease",
                                NA)))
  colnoplot <- length(unique(df$labelfill))-1
  myPalette = palette # there is a default or provided by user
  thisPalette <- c(colorRampPalette(myPalette)(colnoplot), "dark grey")
  maxvalue <- max(df$total, na.rm=TRUE)
  minvalue <- min(c(0,df$base), na.rm=TRUE) #base or 0 will be minimum
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
    geom_text(data=df, aes(x=order, y=total+.02*maxvalue, label=total))+
    scale_fill_manual("",values=thisPalette)+
    scale_x_continuous(breaks=unique(df$order), labels=df$variable)+
    scale_y_continuous(labels = comma, limits=range(c(pretty(minvalue),
                                                      pretty(maxvalue*1.02))))+
    labs(x=xlab, y=ylab) +
    theme_minimal()+
        theme(text=element_text(size=18, family="ProximaNova-Regular"),
          axis.ticks = element_blank(),
          axis.title.y=element_text(vjust=1),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="none")
  return(gg)
} # end of function
