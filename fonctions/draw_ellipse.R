# http://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/
# https://stats.stackexchange.com/questions/9898/how-to-plot-an-ellipse-from-eigenvalues-and-eigenvectors-in-r
draw_ellipse <- function(cov, mu, add, confidence=0.95, col="green", lty=1)
{
  angles <- seq(0, 2*pi, length.out=200)  
  
  eigVal  <- eigen(cov)$values
  eigVec  <- eigen(cov)$vectors
  eigScl  <- eigVec %*% diag(sqrt(eigVal)) 
  xMat    <- rbind(mu[1] + eigScl[1, ], mu[1] - eigScl[1, ])
  yMat    <- rbind(mu[2] + eigScl[2, ], mu[2] - eigScl[2, ])
  coef = sqrt(qchisq(confidence, df=7))
  ellBase <- cbind(sqrt(eigVal[1])*coef*cos(angles),
                   sqrt(eigVal[2])*coef*sin(angles)) 
  ellRot  <- eigVec %*% t(ellBase)
  if(!add)
  {
    plot((ellRot+mu)[1, ], (ellRot+mu)[2, ], asp=1, type="l", lty=lty, lwd=1.5, col=col)
  }
  else
  {
    lines((ellRot+mu)[1, ], (ellRot+mu)[2, ], asp=1, lty=lty, lwd=1.5, col=col)
  }
}