#'
#' Functional cost 1d display
#'
#' @description Displaying the functional cost at consecutive time
#' @param data1 Vector of data in dim 1
#' @param beta penalty positive number
#' @return Display the cost (in the plot window) and the present labels (in the console) at consecutive times
#' @examples
#' data <- data1(sigma = 1, n=20)
#' fpop1d(data,1)

fpop1d <- function(data1, beta){
  mt <- 0
  par(mfrow = c(1,1), mar=c(1,1,1,1))

  n <- length(data1) # number of points

  MIN_y <- min(data1)
  MAX_y <- max(data1)
  delta_y <- MAX_y - MIN_y + sqrt(beta)
  minbis_y <- MIN_y-sqrt(beta)/2

  vec <- seq(minbis_y,minbis_y+delta_y, length.out = 2000)
  quadratics <- rep(0,length(vec))
  tau <- rep(1,length(vec))
  #M_y <- M_x

  for (i in 1:n){
    ##################### mat = matrice des valeurs des quadratiques
    ## + gamma = quadratic function
    quadratics <- quadratics + (vec-data1[i])^2

    ##################### POSITION du min global et truncature
    minimum = min(quadratics)
    tau[quadratics > minimum + beta] <- i+1
    quadratics[quadratics > minimum + beta] <- minimum + beta
    max <- max(quadratics)
    min <- min(quadratics)
    print(sort(unique(tau)))

    for(j in 1:max(tau)){

      plot(vec[tau == j],quadratics[tau == j],xlim=c(minbis_y,minbis_y+delta_y), type = 'l',ylim = c(min,max), col=j,lwd = 2)
      par(new = TRUE)
    }
    abline(a =-data1[i]*10000, b=10000, col = "lightgray")
    par(new = FALSE)
  }


}



#remplacer plot par point -> avec   for(j in 1:length(vec))
