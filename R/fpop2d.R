#'
#' Functional cost 2d display at level beta
#'
#' @description Displaying the functional cost at consecutive times
#' @param data2 matrix of data of dimension 2 x n
#' @param beta penalty coefficent, positive number
#' @param nb An integer : the number of rows in the matrix used for the plots
#' @param circle A boolean to decide to draw the circles of intersection (green if the region stays, red otherwise)
#' @param select An integer. Choose an index of data point and follow all its associated disks (even if the point is already pruned)
#' @return Plots of the functional cost (in the plot window) and the present labels (in the console) at consecutive times
#' @examples
#' data <- dataG2(mean1 = 0, mean2 = 0, tau = 1, sigma = 0.5, n=10)
#' fpop2d(data, n = 300, 1, circle = TRUE)
#'
#'
fpop2d <- function(data2, beta, nb = 300, circle = FALSE, select = 0){
  mt <- -beta

  par(mfrow = c(1,1), mar=c(1,1,1,1))
  n <- dim(data2)[2] # number of points

  ####  ####  ####  ####
  ####MATRIX CREATION###
  ####  ####  ####  ####

  MAX_x <- max(data2[1,])
  MAX_y <- max(data2[2,])
  MIN_x <- min(data2[1,])
  MIN_y <- min(data2[2,])
  delta_x <- MAX_x - MIN_x + 2*sqrt(beta)/2
  delta_y <- MAX_y - MIN_y + 2*sqrt(beta)/2

  minbis_x <- MIN_x-sqrt(beta)/2
  minbis_y <- MIN_y-sqrt(beta)/2

  M_x <- nb
  M_y <- floor(nb*delta_y/delta_x)
  #M_y <- M_x

  Mf <- matrix(0,M_x,M_y)
  Mt <- matrix(1,M_x,M_y)



  for (i in 1:n){

  ##################### matrix of the functional cost = Mf
    ## + gamma = quadratic function

    x <- seq(minbis_x, minbis_x + delta_x, length.out = M_x)
    y <- seq(minbis_y, minbis_y + delta_y, length.out = M_y)

    ### ADDING a point
    Mf <- Mf + (x-data2[1,i])^2
    Mf <- t(Mf) + (y-data2[2,i])^2
    Mf <- t(Mf)

    ####### vec2 = index of present changepoints (in increasing order)
    vec2 <- unique(c(Mt))
    vec2 <- rev(vec2[order(vec2)])

    ############## global min MIN_global and truncature of Mf
    MIN_global = min(Mf)
    Mf[Mf > MIN_global + beta] <- MIN_global + beta
    Mt[Mf == MIN_global + beta] <- i+1
    ##############

    ####### vec = index of present changepoints (after truncature)
    vec <- unique(c(Mt))
    vec <- rev(vec[order(vec)])
    vec <- vec[-1]
    vec <- rev(vec)

    ####global max MIN_global
    max_Mf = max(Mf)

    ####PTS = points des minima de chaque nouvelle quadratique
    n2 <- length(vec)
    PTS<-matrix(0,n2+1,2)

      for (j in 1:(n2)){
        Mmin <- -max_Mf*(Mt==vec[j])+Mf
        a <- which(Mmin == min(Mmin), arr.ind = TRUE)
        PTS[j,]<-a[1,] #position of min for a changeopoint
      }

    ### mt = vector or the consecutive global minimum
    mt <- c(mt,min(Mf))

    b <- which(Mf == min(Mf), arr.ind = TRUE)
    PTS[n2+1,]<-b[1,] #position of the global min

    cat("present chgpt:", vec, "\n")

    ##################### plot

    image2D(Mt, lwd = 2, colkey = FALSE, col=cm.colors(n),zlim=c(1,n))

    points(PTS[1:n2,1]/M_x,PTS[1:n2,2]/M_y, type = "p", pch = 20)
    points(PTS[n2+1,1]/M_x,PTS[n2+1,2]/M_y, type = "p", pch = 15)

    ##########################
    ### IF WE DRAW CIRCLES ###
    ##########################

    if(circle == TRUE){

      ####SELECT
      if(select > 0){if(select<=i){vec <- c(select)}else{vec <- NULL} }

      ### CENTERS AND RADII Cx, Cy, R2
      Cx <- cummean(rev(data2[1,1:i]))[i-vec+1]
      Cy <- cummean(rev(data2[2,1:i]))[i-vec+1]
      Dx <- cummean(rev(data2[1,1:i]*data2[1,1:i]))[i-vec+1]
      Dy <- cummean(rev(data2[2,1:i]*data2[2,1:i]))[i-vec+1]
      Dx <- Dx - Cx*Cx
      Dy <- Dy - Cy*Cy
      Cx <- (Cx - minbis_x)/delta_x
      Cy <- (Cy - minbis_y)/delta_y

    ###GREEN CIRCLES
    if(length(vec)!=0){
    R2 <- (mt[length(mt)]-mt[vec])/(i-vec+1) - Dx - Dy
     for(j in 1:length(Cx)){
       if(R2[j] > 0){points(Cx[j]+(sqrt(R2[j])/delta_x)*cos(0:100/100*2*pi),Cy[j]+(sqrt(R2[j])/delta_y)*sin(0:100/100*2*pi), type = "l", pch = 20, col = 3,cex = 0.2)}
     }
    }
    points(Cx,Cy, type = "p", pch = '+', col = 3)

      vec2 <- setdiff(vec2,vec)
      ####SELECT
      if(select > 0){vec2 <- NULL}

      Cx2 <- cummean(rev(data2[1,1:i]))[i-vec2+1]
      Cy2 <- cummean(rev(data2[2,1:i]))[i-vec2+1]
      Dx2 <- cummean(rev(data2[1,1:i]*data2[1,1:i]))[i-vec2+1]
      Dy2 <- cummean(rev(data2[2,1:i]*data2[2,1:i]))[i-vec2+1]
      Dx2 <- Dx2 - Cx2*Cx2
      Dy2 <- Dy2 - Cy2*Cy2
      Cx2 <- (Cx2 - minbis_x)/delta_x
      Cy2 <- (Cy2 - minbis_y)/delta_y

      ###RED CIRCLES
      if(length(vec2)!=0){
        R22 <- (mt[length(mt)]-mt[vec2])/(i-vec2+1) - Dx2 - Dy2
      for(j in 1:length(Cx2)){
        if(R22[j] > 0){points(Cx2[j]+(sqrt(R22[j])/delta_x)*cos(0:100/100*2*pi),Cy2[j]+(sqrt(R22[j])/delta_y)*sin(0:100/100*2*pi), type = "l", pch = 20, col = 2,cex = 0.2)}
        }
      }
      points(Cx2,Cy2, type = "p", pch = '+', col = 2)
    }

  }

}

