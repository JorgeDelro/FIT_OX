
plot.functional_ANOVA_fit <- function(res, 
                                      colors_n_treatments = NULL, 
                                      legend_position = NULL, 
                                      ylim = NULL, 
                                      ylab = NULL,
                                      xlab = NULL,
                                      ...) {
  
  #################################################### Check libraries
  if (!require(randomcoloR)) {
    install.packages("randomcoloR")
    library(randomcoloR)
  }
  
  plotdata <- res$plotdata
  treatments <- res$treatments
  drop_from_columns <- rep(c("_fub","_fhat","_flb"), times = length(res$treatments))
  
  # y lim
  if(is.null(ylim)) {
    ylim_values <- get_ylim(res$plotdata)
  }
  
  # TO DO - legend postion 
  if(is.null(legend_position)) {
    legend_position <- "topleft"
  } else {
    legend_position <- legend_position
  }
  
  # Get colors to plot different lines
  if(is.null(colors_n_treatments)) {
  colors_n_treatments <- set_colors_n_treatments(n_treatments = length(res$treatments), 
                                                labels_treatments = res$treatments)
  } else {
    colors_n_treatments <- colors_n_treatments
  }
  
  # Create a base plot
  plot(x = res$plotdata$time, 
       y = pull(eval(parse(text = paste("res$plotdata[",2,"]", sep = "")))) , 
       type = "l", 
       ylim = c(ylim_values$ymin, ylim_values$ymax), 
       col = colors_n_treatments[[gsub("_fhat","",colnames(res$plotdata)[2])]], 
       lwd = 3, 
       ylab = ylab, 
       xlab = xlab, 
       #xaxt="n",
       bty="n")
  
  
  # Add lines for the different treatments
  for (i in 3:dim(res$plotdata)[2]) {
    lines(x = res$plotdata$time, 
          y = pull(eval(parse(text = paste("res$plotdata[",i,"]", sep = "")))), 
          col = colors_n_treatments[[gsub(drop_from_columns[i],"",colnames(res$plotdata)[i])]], 
          lwd = 3)
  }
  
  # If the user specify xlab and ylab arguments
  #if(!is.null(xlab)){
  #mtext(side=1,text="x value",line=2.5)
  #}
  #if(!is.null(ylab)){
  #mtext(side=2,text="y value",line=2.5)
  #}
  
  
  legend(legend_position, 
         legend =  res$treatments, 
         col = unlist(colors_n_treatments) , 
         bty = "n", lty=1, lwd = 3, horiz = FALSE, inset = c(0.05, 0.05))
  
  
  # TO DO x-axis labels
  #axis(1, at= labels_grafico, labels = c("00:00","05:00","10:00","15:00",
  #                                       "20:00","25:00"))

}
