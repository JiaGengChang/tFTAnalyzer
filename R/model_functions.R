#' One Step Maturation Model (for sfGFP)
#'
#' @param t char vector of numbers, t represents time in seconds
#' @param state a char vector of initial state: Nd for number of unfolded
#' protein, Nm for number of folded protein
#' @param parameters p, m, k being the per second rate of synthesis, 
#' rate of maturation, and rate of degradation of protein
#'
#' @return a list, whose only element is a vector of the computed 
#' derivatives dNd and dNm. It is the input for the deSolve::ode function
#' @export
#'
#' @examples 
#' state1 <- c(Nd = 0, Nm = 0)
#' parameters1 <- c(p = 100, m = 5, k = 5)
#' time1 <- seq(0, 1, by = 0.01)
#' out1 <- ode(y = state1, time = time1, func = OneStep, parms = parameters1)
#' plot(out1)
OneStep<- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dNd <-  p - m*Nd - k*Nd
    dNm <-  m*Nd - k*Nm
    list(c(dNd,dNm))
  })
}

#' Two Step Maturation Model (for mCherry)
#'
#' @param t char vector of numbers, t represents time in seconds
#' @param state a char vector of initial state: Nd for number of unfolded
#' protein, Ni for number of intermediate protein, Nm for number of 
#' folded protein
#' @param parameters p, m, k being the per second rate of synthesis, 
#' rate of conversion into intermediate, rate of maturation, and rate 
#' of degradation of protein
#'
#' @return a list, whose only element is a vector of the computed 
#' derivatives dNd, dNi, and dNm. It is the input for the deSolve::ode function
#' @export
#'
#' @examples
#' state2 <- c(Nd = 0, Ni = 0, Nm = 0)
#' parameters2 <- c(p = 100, m1 = 5, m2 = 5, k = 5)
#' time2 <- seq(0, 1, by = 0.01)
#' out2 <- ode(y = state2, time = time2, func = TwoStep, parms = parameters2)
#' plot(out2)
TwoStep <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dNd <-  p - m1*Nd - k*Nd
    dNi <- m1*Nd - m2*Ni - k*Ni 
    dNm <-  m2*Ni - k*Nm
    list(c(dNd,dNi,dNm))
  })
}

#' Plot 3 curves at in one picture: green, red, and red:green ratio
#'
#' @param fastFP 
#' @param slowFP 
#' @param time 
#'
#' @return
#' @export
#'
#' @examples
#' parameters <- c(p = 100, m = 4.2, m1 = 7.83548, m2 = 4.4401, a = 2.07944/2, b = 2*pi/24, c = pi, d = 2.07944)
#' time <- seq(0, 48, by = 0.1)
#' stateFastFP <- c(Nd = 0, Nm = 0)
#' stateSlowFP <- c(Nd = 0, Ni = 0, Nm = 0)
#' fastFP <- ode(y = stateFastFP, time = time, func = OneStepKVar, parms = parameters)
#' slowFP <- ode(y = stateSlowFP, time = time, func = TwoStepKVar, parms = parameters)
#' PlotTandemFPKinetics(fastFP, slowFP, time)
PlotTandemFPKinetics <- function(fastFP, slowFP, time){
  mx <- as.matrix(x = cbind(fastFP = fastFP[,3],
                            slowFP = slowFP[,4],
                            ratio = slowFP[,4]/fastFP[,3]
  ))
  mx[1,'ratio']<-0 # avoid NaN by setting to 0. alternative is to remove row 1
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(x=time,y=mx[,'fastFP'],type='l',col='#118002',lwd=2, 
       ylab='number of fluorescing FPs',xlab="time (h)")
  lines(x=time,y=mx[,'slowFP'],type='l',col='#FB0207',lwd=2, xlab="",ylab="")
  par(new=TRUE)
  plot(x=time,y=mx[,'ratio'],type='l', axes = FALSE, xlab="",ylab="",lty=2)
  axis(side=4, at = pretty(range(mx[,'ratio'])))
  mtext("slowFP:fastFP", side=4, line=3)
  legend(x=18,y=0.2,legend=c('fastFP','slowFP','ratio'),
         col=c('#118002','#FB0207','black'),
         lty=c(1,1,2))
  #title('[add title here]')
}

#' One step maturation model with sinusoidal k
#' 
#'
#' @param t 
#' @param state 
#' @param parameters 
#'
#' @return
#' @export
#'
#' @examples
OneStepKVar<- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    k <- a*cos(b*t-c)+d
    dNd <-  p - m*Nd - k*Nd
    dNm <-  m*Nd - k*Nm
    list(c(dNd,dNm))
  })
}

#' Two step maturation model with sinusoidal k
#'
#' @param t 
#' @param state 
#' @param parameters 
#'
#' @return
#' @export
#'
#' @examples
TwoStepKVar <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    k <- a*cos(b*t-c)+d
    dNd <-  p - m1*Nd - k*Nd
    dNi <- m1*Nd - m2*Ni - k*Ni 
    dNm <-  m2*Ni - k*Nm
    list(c(dNd,dNi,dNm))
  })
}

NmTwoStepTimeDependent <- function(p, k, m1, m2, t){
  stopifnot(m1!=m2) # singularity
  val <- p/k * (m1*m2/((k+m1)*(k+m2))-exp(-k*t)) + 
    (p/(m1-m2)) * (m1/(k+m2)*exp(-(k+m2)*t)-m2/(k+m1)*exp(-(k+m1)*t))
  return(val)
}

NmTwoStepSteadyState <- function(p, k, m1, m2){
  val <- p*m1*m2/(k*(k+m1)*(k+m2))
  return(val)
}

NmOneStepTimeDependent <- function(p, k, m, t){
  val <- p/(k+m)*(m/k + exp(-(k+m)*t)) - p/k*exp*(-k*t)
  return(val)
}

NmOneStepSteadyState <- function(p, k, m){
  val <- p*m/(k*(k+m))
  return(val)
}

cosplot <- function(time, parameters){
  with(as.list(parameters),{
    out <- cbind(time,a*cos(b*time-c)+d)
    plot(out, 
         xlab = 'time in hours', 
         ylab = expression('k'['deg']),
         main = expression('k'['deg']*' ~ acos(bt - c) + d'),
         type = 'l',
         sub = paste('Parameters: a=',a,',b=',b,',c=',c,',d=',d),
         ylim = c(d-2*a,d+2*a))
  })
}