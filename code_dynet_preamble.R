#####################################################################
## Author: Joshua Cape (joshua.cape@pitt.edu)
## Script: DyNet paper code, preamble material and defined functions
#####################################################################

#####################################################################

  library(igraph)
  library(ggplot2)
  library(irlba)
  library(matrixcalc)
  library(matpow)
  library(mvtnorm)
    
  library(plotrix)
  library(xtable)
  library(knitr)
  library(distill)

  library(patchwork)

  library(mclust)
  library(MASS)

  library(lubridate)
  library(readr)
  library(dplyr)

#####################################################################
#####################################################################

    sym <- function(s){
      s[lower.tri(s)] = t(s)[lower.tri(s)];
      s
    }
    
    ttinf <- function(mtx.data){
      return(max(apply(mtx.data, 1, function(x) norm(x, "2"))))
    }
    
#####################################################################
#####################################################################
 
    
    circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
      r = diameter / 2
      tt <- seq(0, 2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    
    rot_mtx <- function(theta){
      return(rbind(c(cos(theta), -sin(theta)),
                   c(sin(theta), cos(theta))))
    }

    unit_func <- function(vector){
      return(vector/norm(vector,"2"))
    }    
    
    
#####################################################################
#####################################################################
    
    mtx_print <- function(mtx){
      return(
        print(xtable(mtx, align=rep("",ncol(mtx)+1), digits=7),
              tabular.environment="bmatrix",
              include.rownames=FALSE,
              include.colnames=FALSE,
              floating=FALSE,
              hline.after=NULL,
              timestamp=NULL,
              comment=FALSE)
      )
    }
    
    func_slope <- function(vec) return(vec[2]/vec[1])

#####################################################################    
#####################################################################