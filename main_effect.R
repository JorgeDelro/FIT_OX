# main effects function

plot_main_effect <- function(res, 
                             main_effect = NULL,
                             control_var = NULL,
                             color_lines = NULL, 
                             xlab = NULL, 
                             ylab = NULL, 
                             legend_position = NULL, 
                             ylim = NULL, 
                             ...) {
  
  if(is.null(main_effect) || is.null(control_var)) {
    stop("Please, specifiy a main effect or / and a control variable")
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
  polygons_data <- create_polygons(res.exc[[main_effect]]$G)
  
  # Colors for lines
  if(is.null(color_lines)) {
  colors_lines <- c("","red","red","red","blue","blue","blue")
  } else {
    colors_lines <- c("",rep(colors_lines$main_effect,3),rep(colors_lines$control,3))
  }
  
  # Create a base plot
  plot(x = main_effect_data$time, 
       y = pull(eval(parse(text = paste("main_effect_data[",2,"]", sep = "")))), 
       type = "l", 
       ylim = c(ylim_values$ymin, ylim_values$ymax), 
       col = colors_lines[2], 
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
  
  # Add lines
  for (i in 2:dim(main_effect_data)[2]) {
    lines(x = main_effect_data$time,
          y = pull(eval(parse(text = paste("main_effect_data[",i,"]", sep = "")))), 
          type = "l",
          lwd = 2,
          col = colors_lines[i])
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
  
  legend(legend_position, 
         legend =  c(main_effect, "control"), 
         col = c(colors_lines[2], colors_lines[5]) , 
         bty = "n", lty=1, lwd = 3, horiz = FALSE, inset = c(0.05, 0.05))
  
  
}
