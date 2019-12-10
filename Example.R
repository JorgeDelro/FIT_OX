# Example

prueba_id <- seq(1:dim(VO2)[1])
duplicated(prueba_id)[1]
length(unique(prueba_id))

length(unique(prueba_treatment))
prueba_treatment <- rep(c(1,2),6)

prueba <- functional_ANOVA(datFrm = VO2,
                           x_axis = NULL,
                           treatments = prueba_treatment,
                           id = prueba_id)


prueba_main_effect <- plot_main_effect(prueba,
                                       main_effect = "1",
                                       control_var = "2")


####################################################################

prueba$res.exc[['1']]$G
polygons_data_prueba <- create_polygons(prueba$res.exc[['1']]$G)

# main_effect_data
main_effect_data_prueba <- prueba$plotdata %>%
  select("time",
         starts_with('1'), 
         starts_with('2'))

if(is.null(ylim)) {
  ylim_values <- get_ylim(prueba$plotdata)
}

# TO DO - legend postion 
if(is.null(legend_position)) {
  legend_position <- "topleft"
} else {
  legend_position <- legend_position
}

# Colors for lines
if(is.null(colors_lines)) {
  colors_lines_mean <- c("red", "blue")
  colors_lines_CI <- c("red", "blue", "red", "blue")
} else {
  colors_lines_mean <- c(colors_lines$main_effect, colors_lines$control)
  colors_lines_CI <- c(colors_lines$main_effect, colors_lines$control, colors_lines$main_effect, colors_lines$control)
}

# Create Polygons Data
polygons_data <- create_polygons(prueba$res.exc[["1"]]$G)


# Create a base plot
plot(x = main_effect_data_prueba$time, 
     y = pull(eval(parse(text = paste("main_effect_data_prueba[",2,"]", sep = "")))), 
     type = "l", 
     ylim = c(ylim_values$ymin, ylim_values$ymax),
     xlim = c(0, max(main_effect_data_prueba$time)),
     col = colors_lines_mean[1], 
     lwd = 2, 
     ylab = "", 
     xlab = "",
     #xaxt="n",
     bty="n")

# Draw Polygons using polygons_data
for (i in length(polygons_data)) {
  polygon(c(main_effect_data_prueba[polygons_data[[i]]$min_value:polygons_data[[i]]$max_value,"time"]$time,
            rev(main_effect_data_prueba[polygons_data[[i]]$min_value:polygons_data[[i]]$max_value,"time"]$time)),
          c(rep(ylim_values$ymin, (polygons_data[[i]]$max_value - polygons_data[[i]]$min_value)+1),
            rep(ylim_values$ymax, (polygons_data[[i]]$max_value - polygons_data[[i]]$min_value)+1)),
          col="gray80",
          border = NA)
}




# Add lines mean
main_effect_data_prueba_mean <- main_effect_data_prueba_mean %>% select(ends_with("fhat"))
for (i in 1:dim(main_effect_data_prueba_mean)[2]) {
  lines(x = main_effect_data_prueba$time,
        y = pull(eval(parse(text = paste("main_effect_data_prueba_mean[",i,"]", sep = "")))), 
        type = "l",
        lwd = 2,
        col = colors_lines_mean[i], 0.7)
}

# Add lines CI
main_effect_data_prueba_CI <- main_effect_data_prueba %>% select(ends_with("flb"),
                                                                 ends_with("fub"))
for (i in 1:dim(main_effect_data_prueba_CI)[2]) {
  lines(x = main_effect_data_prueba$time,
        y = pull(eval(parse(text = paste("main_effect_data_prueba_CI[",i,"]", sep = "")))), 
        type = "l",
        lwd = 2,
        lty = 2,
        col = alpha(colors_lines_CI[i], 0.7))
}

for (i in length(polygons_data)) {
  # min
  text(main_effect_data_prueba[polygons_data[[i]]$min_value,"time"], 
       y = ylim_values$ymin + 0.01, 
       labels = toString(main_effect_data_prueba[polygons_data[[i]]$min_value,"time"]))
  
  # max
  text(main_effect_data_prueba[polygons_data[[i]]$max_value,"time"], 
       y = ylim_values$ymin + 0.01, 
       labels = toString(main_effect_data_prueba[polygons_data[[i]]$max_value,"time"]))
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




