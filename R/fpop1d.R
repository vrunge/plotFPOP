#'
#' Functional cost 1d display
#'
#' @description Displaying the functional cost at consecutive times
#' @param data1 Vector of data
#' @param beta penalty coefficient, positive number
#' @param nb An integer : the number of elements in the vector for the x-axix of the plot
#' @param order a boolean. If true,it displays the labels on the real line from left to right
#' @return plots of the functional cost (in the plot window) and the present labels (in the console) at consecutive times. The vertical gray line shows the position of the current added data on the real line
#' @examples
#' data <- dataG1(sigma = 0.5, n=15)
#' fpop1d(data,1)

fpop1d <- function(data1, beta, nb = 2000, order = FALSE){
  mt <- 0
  par(mfrow = c(1,1), mar=c(1,1,1,1))

  n <- length(data1) # number of points

  MIN_y <- min(data1)
  MAX_y <- max(data1)
  delta_y <- MAX_y - MIN_y + sqrt(beta)
  minbis_y <- MIN_y-sqrt(beta)/2

  N <- nb
  vec <- seq(minbis_y,minbis_y+delta_y, length.out = N)
  quadratics <- rep(0,length(vec))
  tau <- rep(1,length(vec))
  #M_y <- M_x

  for (i in 1:n){
    ##################### quadratics = matrix of the functional cost
    ## + gamma = quadratic function
    quadratics <- quadratics + (vec-data1[i])^2

    ##################### global min and truncature
    minimum = min(quadratics)
    tau[quadratics > minimum + beta] <- i+1
    quadratics[quadratics > minimum + beta] <- minimum + beta
    max <- max(quadratics)
    min <- min(quadratics)
    if(order == FALSE){
      print(sort(unique(tau)))}else{
      print(tau[tau!=c(tau[-1], FALSE)])
    }
    tauPosition <- c(which(c(1,diff(tau))!=0),N+1)

    for(k in 1:(length(tauPosition)-1)){
      #cat(tauPosition[k],"  -- ", tau[tauPosition[k]], " -- ", tauPosition[k+1]-1, "\n")
      plot(vec[tauPosition[k]:(tauPosition[k+1]-1)],quadratics[tauPosition[k]:(tauPosition[k+1]-1)],xlim=c(minbis_y,minbis_y+delta_y), type = 'l',ylim = c(min,max), col=tau[tauPosition[k]],lwd = 2)
      par(new = TRUE)
    }
    abline(v =data1[i], col = "lightgray")
    par(new = FALSE)
  }

}
