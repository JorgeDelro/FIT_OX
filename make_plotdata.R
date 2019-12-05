
make_plotdata <- function(x) UseMethod("make_plotdata", x)


#make_plotdata.default <- make_plotdata(x)


make_plotdata.functional_ANOVA_fit <- function(x,...) {
  
  result <- x$result
  ng <- x$ng
  ns <- x$ns
  treatments <- x$labels_treatments
  
  # Control data
  controldata <- bri.band.ggplot(result, name = 'mu', type = 'random')
  control_fhat <- controldata$data$fhat
  control_flb <- controldata$data$f.lb
  control_fub <- controldata$data$f.ub
  control_tibble <- tibble(control_fhat,control_flb,control_fub) #%>% 
  #                                                      gather() %>% 
  #                                  mutate(key_new = factor(rep("Control", nrow(.))))
  
  # Treatment data
  n_treatment <- ng -1 
  
  # loop over treatments to get data and rename columns
  for(i in 1:n_treatment) {
    treatment_data <- bri.band.ggplot(result, ind = 1:ns + (i-1)*ns, type = 'lincomb')
    treatment_fhat <-  treatment_data$data$fhat
    treatment_flb <-  treatment_data$data$f.lb
    treatment_fub <-  treatment_data$data$f.ub
    treatment <- tibble(treatment_fhat, treatment_flb, treatment_fub)
    if(i == 1) {
      treatment_tibble <- treatment
    } else {
      treatment_tibble <- bind_cols(treatment_tibble, treatment)
    }
  }
  
  plotdata <- bind_cols(control_tibble, treatment_tibble)
  
  labels_plotdata <- create_labels_plotdata(treatments, dim(plotdata)[2])
  
  colnames(plotdata) <- labels_plotdata
  
  return(plotdata)
  
}



