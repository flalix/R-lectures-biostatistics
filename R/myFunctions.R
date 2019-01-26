library(grid)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

print_histogram <- function (df, title, xlab, col, filename, bins=15, xlimits=c(-10,10), density=F, printPlot=NA, ret=F, is.log=F) {
  
  num.class = length(names(table(df$class)))
  
  if (is.log) {
    df[,1] = log2( df[,1])
    xlab = paste("log2(", xlab,")")
  }
  
  if (density) {
    fnc = geom_density
  } else {
    fnc = geom_histogram
  }
  if (num.class == 1) {
    p1 <- ggplot(df, aes(x = value)) +  xlab(xlab) + ggtitle(title)
    p1 <- p1 + fnc(fill=col, size=.2, alpha = 0.2)  # aes(fill=factor(class))
    p1
  } else {
    p1 <- ggplot(df, aes(x=value)) +  xlab(xlab) + ggtitle(title)
    p1 <- p1 + fnc(aes(fill=color), size=.2, alpha = 0.2)
    p1 <- p1 + scale_fill_manual(labels = levels(df$class), values=col) 
    p1
  }
  if (length(xlimits) == 2) p1  <- p1 +  xlim(xlimits)
  
  if (!ret) multiplot(p1)  
  
  if (!is.na(printPlot) && filename!="") {
    filename = paste(dir_result, filename, sep="")
    ggsave(filename, dpi=dpi)
  }
  
  if (ret) return (p1)
}
