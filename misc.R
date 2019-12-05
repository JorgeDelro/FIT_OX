### Miscellaneous functions

# Create a x-value for x-axis plot if the user do not specify one
create_x <- function(plotdata) {
  x <- tibble(seq(1:dim(plotdata)[1]))
  x <- x %>%
    rename(time = "seq(1:dim(plotdata)[1])")
  return(x)
}

# Create columns names for plotdata tibble based on treatment labels
create_labels_plotdata <- function(labels_treatments, n){
  
  labels_plotdata <- vector(mode = "character", n)
  labels_ <- vector(mode = "character", 3)
  
  for(i in seq_along(labels_treatments)) {
    for (j in 1:3) {
      if(j==1) {
        labels_ <- rep(paste(labels_treatments[i],"_fhat",sep = ""), times = 1, each=1)
      } else if(j==2){
        labels_ <- c(labels_, rep(paste(labels_treatments[i],"_flb",sep = ""), times = 1, each=1))
      } else 
        labels_ <- c(labels_, rep(paste(labels_treatments[i],"_fub",sep = ""), times = 1, each=1))
    }
    if(i == 1) {
      labels_plotdata <- c(labels_)
    } else {
      labels_plotdata <- c(labels_plotdata, labels_)
    }
  }
  
  return(labels_plotdata)
}

# Create polygons for main effect plot
create_polygons <- function(G) {
  # La función grep devuelve la posición donde se
  # encuentra el primer argumento en el vector
  polygons <- split(grep(TRUE, G), cumsum(c(TRUE, diff(grep(TRUE, G))!=1)))
  #n_polygons <- length(polygons)
  
  polygons_data <- list()
  j <- 1
  for (i in polygons) {
    polygons_data[[toString(j)]] <- list(min_value = min(i), max_value = max(i))
    j <- j + 1
  }
  
  return(polygons_data)
}

# Set random colors for each treatment at plotdata
set_colors_n_treatments <- function(n_treatments, labels_treatments) {
  
  colors_R <- distinctColorPalette(n_treatments)
  colors_n_treatments <- list()
  for (i in 1:n_treatments) {
    colors_n_treatments[labels_treatments[i]] <- colors_R[i]
  }
  return(colors_n_treatments)
}

# Establish a y lim for plot
get_ylim <- function(plotdata) {
  # Get minimum values
  min_values <- plotdata %>%
    select(2:dim(plotdata)[2]) %>%
    map_dbl(.,min)  %>%
    map_dbl(.,round)
  
  # Get maximum values
  max_values <- plotdata %>%
    select(2:dim(plotdata)[2]) %>%
    map_dbl(.,max) %>%
    map_dbl(.,round)
  
  # Make a list with just one min and max value 
  ylim_values <- list(ymin = min(min_values), ymax = max(max_values))
  return(ylim_values)
}

# Constructor function for class functional_ANOVA_fit
functional_ANOVA_fit <- function(x){
  class(x) <- append(class(x), "functional_ANOVA_fit" )
  return(x)
}


## Work in progress

plot_GGPLOT



