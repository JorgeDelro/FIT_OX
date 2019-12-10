# main effects function

plot_main_effect <- function(res, 
                             main_effect = NULL,
                             control_var = NULL,
                             colors_lines = NULL, 
                             xlab = NULL, 
                             ylab = NULL, 
                             legend_position = NULL,
                             legend_label_main = NULL,
                             legend_label_control = NULL,
                             ylim = NULL, 
                             ...) {
  
  if(is.null(main_effect) || is.null(control_var)) {
    stop("Please, specifiy a main effect or / and a control variable")
  }
  
  #################################################### Check libraries
  if (!require(scales)) {
    install.packages("scales")
    library(scales)
  }
  
  #if(is.null(name_plot)){
  #  name_plot <- paste("main_effect_",main_effect,"_plot",sep = "")
  #}
  
  if(is.null(ylim)) {
    ylim_values <- get_ylim(res$plotdata)
  }
  
  # TO DO - legend postion 
  if(is.null(legend_position)) {
    legend_position <- "topleft"
  } else {
    legend_position <- legend_position
  }
    
  # Get columns to plot from plotdata
  main_effect_data <- res$plotdata %>%
                    select("time",
                           starts_with(main_effect), 
                           starts_with(control_var))
  
  # Create Polygons Data
  polygons_data <- create_polygons(res$res.exc[[main_effect]]$G)
  
  # Colors for lines
  # Colors for lines
  if(is.null(colors_lines)) {
    colors_lines_mean <- c("red", "blue")
    colors_lines_CI <- c("red", "blue", "red", "blue")
  } else {
    colors_lines_mean <- c(colors_lines$main_effect, colors_lines$control)
    colors_lines_CI <- c(colors_lines$main_effect, colors_lines$control, colors_lines$main_effect, colors_lines$control)
  }
  
  # Create a base plot
  plot(x = main_effect_data$time, 
       y = pull(eval(parse(text = paste("main_effect_data[",2,"]", sep = "")))), 
       type = "l", 
       ylim = c(ylim_values$ymin, ylim_values$ymax),
       xlim = c(0, max(main_effect_data$time)),
       col = colors_lines_mean[1], 
       lwd = 2, 
       ylab = "", 
       xlab = "",
       
       #xaxt="n",
       bty="n")
  
  # Draw Polygons using polygons_data
  for (i in length(polygons_data)) {
            polygon(c(main_effect_data[polygons_data[[i]]$min_value:polygons_data[[i]]$max_value,"time"]$time,
                      rev(main_effect_data[polygons_data[[i]]$min_value:polygons_data[[i]]$max_value,"time"]$time)),
            c(rep(ylim_values$ymin, (polygons_data[[i]]$max_value - polygons_data[[i]]$min_value)+1),
              rep(ylim_values$ymax, (polygons_data[[i]]$max_value - polygons_data[[i]]$min_value)+1)),
            col="gray80",
            border = NA)
  }
  
  # Add lines mean
  main_effect_data_mean <- main_effect_data %>% select(ends_with("fhat"))
  for (i in 1:dim(main_effect_data_mean)[2]) {
    lines(x = main_effect_data$time,
          y = pull(eval(parse(text = paste("main_effect_data_mean[",i,"]", sep = "")))), 
          type = "l",
          lwd = 2,
          col = colors_lines_mean[i], 0.7)
  }
  
  # Add lines CI
  main_effect_data_CI <- main_effect_data %>% select(ends_with("flb"),
                                                      ends_with("fub"))
  for (i in 1:dim(main_effect_data_CI)[2]) {
    lines(x = main_effect_data$time,
          y = pull(eval(parse(text = paste("main_effect_data_CI[",i,"]", sep = "")))), 
          type = "l",
          lwd = 2,
          lty = 2,
          col = alpha(colors_lines_CI[i], 0.7))
  }
  
  for (i in length(polygons_data)) {
  # min
  text(main_effect_data[polygons_data[[i]]$min_value,"time"], 
       y = ylim_values$ymin + 0.01, 
       labels = toString(main_effect_data[polygons_data[[i]]$min_value,"time"]))
  
  # max
  text(main_effect_data[polygons_data[[i]]$max_value,"time"], 
       y = ylim_values$ymin + 0.01, 
       labels = toString(main_effect_data[polygons_data[[i]]$max_value,"time"]))
  }
  
  # If the user specify xlab and ylab arguments
  if(!is.null(xlab)){
    mtext(side=1,text="x value",line=2.5)
  }
  if(!is.null(ylab)){
    mtext(side=2,text="y value",line=2.5)
  }
  
  if(is.null(legend_label_main)) {
    legend_label_main <- "main"
  } else {
    legend_label_main <- legend_label_main
  }
  
  if(is.null(legend_label_control)) {
    legend_label_control <- "control"
  } else {
    legend_label_control <- legend_label_control
  }
  
  legend(legend_position, 
         legend =  c(legend_label_main, legend_label_control), 
         col = c(colors_lines_mean[1], colors_lines_mean[2]) , 
         bty = "n", lty=1, lwd = 3, horiz = FALSE, inset = c(0.05, 0.05))
  
  
}
