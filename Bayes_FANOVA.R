### Functional ANOVA

# Y. Yu, D. Bolin, H. Rue, X. Wang. Bayesian generalized two-way ANOVA modeling for functional data using INLA. 
# Stat. Sin. (2018), 10.5705/ss.202016.0055

# X. Wang, Y. Yu, J. Faraway
# Bayesian Regression Modeling with INLA
# Chapman and Hall/CRC, Boca Raton, FL (2018)


# add 2 arguments: alpha_level / excursion_method
functional_ANOVA <- function(datFrm, 
                             x_axis = NULL, 
                             treatments, 
                             id, 
                             family = "gaussian",
                             alpha_level = 0.05, 
                             excursions_method = "NIQC") {
  
  
  #################################################### Check necessary values for INLA
  if(is.null(datFrm)){
    stop("Please, specifiy a respose variable")
  }
  
  if(is.null(treatments)){
    stop("Please, specifiy a treatments variable")
  }
  
  if(is.null(id)){
    stop("Please, specifiy a id variable")
  }
  
  #################################################### Check libraries
  if (!require(tidyverse)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  if (!require(brinla)) {
    install.packages("brinla")
    library(brinla)
  }
  
  ################################################### DATA
  
  # Response variable
  y <- as.vector(t(datFrm)) 
  # number time points
  ns <- dim(datFrm)[2]
  # number of levels in factor
  ng <- length(unique(treatments))
  # Labels of levels in factor
  labels_treatments <- as.character(unique(treatments))
  # number of subjects
  if(duplicated(id)[1] == T) {
    n <- length(unique(id))
  } else {
    n <- dim(datFrm)[1] / ng
  }
   
  
  ################################################### MATRICES
  
  # Create a diagonal matrix of class "ddiMatrix", with diagonal entries = x
  D1 <- Diagonal(n = ns, x = 1)
  # Construct a Matrix ng*n rows x 1 column of 1´s
  D2 <- Matrix(rep(1,ng*n),ng*n,1)
  # Computes the generalised kronecker product of D2 and D1: sparse Matrix of class "dgTMatrix"
  A.mu <- kronecker(D2, D1)
  
  # Create a diagonal matrix of class "ddiMatrix" with ns rows/columns, with diagonal entries = x
  D1 <- Diagonal(n = ns, x = 1)
  # Create a diagonal matrix of class "ddiMatrix" with ng-1 rows/columns, with diagonal entries = x
  D2 <- Diagonal(n = (ng-1), x = 1)
  # Construct a Matrix ng*n rows x 1 column of 1´s
  D3 <- Matrix(rep(0, ng-1), 1, ng-1)
  # Construct sparse Matrix of class "dgCMatrix"
  D4 <- kronecker(rbind(D2, D3), D1)
  # Computes the generalised kronecker product of a n x 1 Matrix and D4: sparse Matrix of class "dgTMatrix"
  A.a <- kronecker(Matrix(rep(1, n), n, 1), D4)
  
  # Construct A Matrix
  A <- cbind(A.mu, A.a)
  
  ################################################### INDEX VECTORS
  
  # Vector of length ns
  mu <- 1:ns
  # Repeated 1:ns ng-1 times
  alpha <- rep(1:ns, ng-1)
  alpha.rep <- rep(1:(ng-1), each = ns)
  
  # The length of mu, alpha and alpha.rep must match number columns of A
  mu2 <- c(mu, rep(NA, length(alpha)))
  alpha2 <- c(rep(NA, length(mu)), alpha)
  alpha2.rep <- c(rep(NA, length(mu)), alpha.rep)
  
  ################################################### FIT THE MODEL INLA
  
  # list to pass the data to INLA
  data.inla <- list(y=y, mu=mu2, alpha=alpha2, alpha.rep=alpha2.rep)
  # f{INLA} function used for defining of smooth and spatial terms within inla model formulae. 
  # The function does not evaluate anything - it exists purely to help set up a model. 
  # The function specifies one smooth function in the linear predictor.
  # Arguments
  # model =  second-order random walk prior RW2 - pg 171 -Bayesian Regression Modeling with INLA
  # constr =  set a sum to 0 constraint on the term
  # scale.model = scale RW2 so its variance is 1
  # replicate = 
  formula <- y ~ -1 + f(mu, 
                        model = 'rw2', 
                        constr = FALSE, 
                        scale.model = T) + 
    f(alpha, 
      model = 'rw2', 
      constr = FALSE, 
      scale.model = TRUE, 
      replicate = alpha.rep)
  
  # Linear Combinations
  A1.lc <- kronecker(Matrix(rep(1,ng-1),ng-1,1), Diagonal(n=ns, x=1))
  A2.lc <- Diagonal(n = (ng - 1)*ns, x = 1)
  lc <- inla.make.lincombs(mu = A1.lc, alpha = A2.lc)
  
  # inla performs a full Bayesian analysis of additive models using Integrated Nested Laplace approximation
  # Arguments
  # family = http://www.r-inla.org/models/likelihoods
  # control.predictor = compute = compute marginals for the linear predictor
  # control.compute = config =  internal GMRF approximations are stored
  result <- inla(formula, 
                 data = data.inla, 
                 family = family, 
                 control.predictor = list(A = A, compute = TRUE), 
                 control.compute = list(config = TRUE),
                 lincomb = lc)
  
  
  # Get the excursion for each comparison control VS treatment
  res.exc <- list()
  for(i in 1:(ng-1)) {
    res.exc[[labels_treatments[i]]] <- excursions.brinla(result, 
                                                         name = 'alpha', 
                                                         ind = 1:ns + (i-1)*ns, 
                                                         u = 0, 
                                                         type = '!=', 
                                                         alpha = alpha_level, 
                                                         method = excursions_method)
  }
  
  #################################################### Post-INLA   
  
  # Make a list with the data
  fANOVAdata <- list(result = result,ng = ng, ns = ns, labels_treatments = labels_treatments)
  
  # Add class
  class(fANOVAdata) <- append(class(fANOVAdata), "functional_ANOVA_fit" )
  
  # Get the data for ploting
  plotdata <- make_plotdata(fANOVAdata)
  
  # Checks about x value
  if(is.null(x_axis)) {
    cat("Creating x values")
    x_axis <- create_x(plotdata)
  } else if(dim(plotdata)[1] != dim(x_axis)[1]){
    cat("Creating x values to replace the one specified because x value has a different length")
    x_axis <- create_x(plotdata)
  } else if(dim(plotdata)[1] == dim(x_axis)[1] && !(is.tibble(x_axis))){
    x_axis <- tibble(time = x_axis)
  } else {
    x_axis <- x_axis %>%
      rename(time = names(x_axis))
  }
  plotdata <- bind_cols(x_axis,plotdata)
  
  
  # Return the result and the data for ploting
  res <- list(result = result, res.exc = res.exc, plotdata = plotdata, treatments = labels_treatments)
  class(res) <- "functional_ANOVA_fit"
  return(res)
}
