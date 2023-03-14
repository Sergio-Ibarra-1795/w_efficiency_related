## Setup ----------------------------------------------------------------------

library(R6)
library(mvtnorm)

theme_set(theme_linedraw(base_size = 25))

## Para el tema de ggplot
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())


### Muestreador Metropolis-Hastings -------------------------------------------
crea_metropolis_hastings_previa <- function(objetivo, muestreo){
  ## Muestreador MH con propuestas de la previa
  function(niter){
    ## Empezamos en algun lugar
    estado <- c(58.23, 2.72, 1.64)
    ndim <- length(estado) 
    muestras <- matrix(nrow = niter, ncol = ndim + 1)
    muestras[1,2:(ndim+1)] <- estado
    muestras[1,1] <- 1
    for (ii in 2:niter){
      propuesta   <- muestreo$sample()
      log_pi_propuesta <- objetivo$logdensity(propuesta)
      log_pi_estado    <- objetivo$logdensity(estado)
      log_alpha <- log_pi_propuesta - log_pi_estado

      if (log(runif(1)) < log_alpha) {
        muestras[ii, 1] <- 1 ## Aceptamos
        muestras[ii, 2:(ndim+1)] <- propuesta
      } else {
        muestras[ii, 1] <- 0 ## Rechazamos
        muestras[ii, 2:(ndim+1)] <- estado
      }
      estado <- muestras[ii, 2:(ndim+1)]
    }
    colnames(muestras) <- c("accept", "alpha", "beta", "sigma")
    muestras
  }
}

PriorModel <-
  R6Class("ProbabilityModel",
          list( 
            sample = function(nsamples = 1){
                samples <- as.matrix(cbind(
                    alpha = rnorm(nsamples, 0, 10), 
                    beta = rnorm(nsamples, 0, 1), 
                    sigma = rexp(nsamples, 1)))
                return(samples)
            }, 
            logdensity = function(theta, log = TRUE){
              # parametros ----------------------------------------------------
              alpha <- theta[1]; beta <- theta[2]; sigma <- theta[3]
              # logdensidad ---------------------------------------------------
              dnorm(alpha, 0, 10, log = TRUE) + 
                dnorm(beta, 0, 1, log = TRUE) + 
                dexp(sigma, 1, log = TRUE)
            }
          ))

LikelihoodModel <-
  R6Class("LikelihoodModel",
          list(
            data  = NA,
            initialize = function(data = NA){
              self$data  = data 
            }, 
            logdensity = function(theta, log = TRUE){
              # datos 
              height <- pull(self$data, height); 
              weight <- pull(self$data, weight);
              # parametros 
              alpha <- theta[1]; beta <- theta[2]; sigma <- theta[3]
              mu    <- alpha + beta * weight
              # logdensidad
              sum(dnorm(height, mean = mu, sd = sigma, log = TRUE))
            }
          ))


PosteriorModel <-
  R6Class("InferenceModel",
          list(
            likelihood = NA, 
            prior      = NA,
            initialize = function(verosimilitud, previa){
              self$likelihood <- verosimilitud
              self$prior      <- previa
            }, 
            logdensity = function(theta, log = TRUE){
                if(theta[3] < 0 ){
                    return(-Inf)
                }
                else{
                    return(self$likelihood$logdensity(theta) + 
                           self$prior$logdensity(theta)
                    )}}
          ))


## Caminata aleatoria ---------------------------------------------------------

crea_metropolis_hastings <- function(objetivo, muestreo){
  ## Muestreador MH 
  function(niter){
    ## Empezamos en algun lugar
    estado <- c(58.23, 2.72, 1.64)
    ndim <- length(estado) 
    muestras <- matrix(nrow = niter, ncol = ndim + 1)
    muestras[1,2:(ndim+1)] <- estado
    muestras[1,1] <- 1
    for (ii in 2:niter){
      propuesta   <- estado + muestreo$sample()
      log_pi_propuesta <- objetivo$logdensity(propuesta)
      log_pi_estado    <- objetivo$logdensity(estado)
      log_alpha <- log_pi_propuesta - log_pi_estado
      if (log(runif(1)) < log_alpha) {
        muestras[ii, 1] <- 1 ## Aceptamos
        muestras[ii, 2:(ndim+1)] <- propuesta
      } else {
        muestras[ii, 1] <- 0 ## Rechazamos
        muestras[ii, 2:(ndim+1)] <- estado
      }
      estado <- muestras[ii, 2:(ndim+1)]
    }
    colnames(muestras) <- c("accept", "alpha", "beta", "sigma")
    muestras
  }
}

ModeloNormalMultivariado <-
  R6Class("ProbabilityModel",
          list(
            mean = NA,
            cov  = NA, 
            initialize = function(mu = 0, sigma = 1){
              self$mean = mu
              self$cov  = sigma |> as.matrix()
            }, 
            sample = function(n = 1){
              rmvnorm(n, mean = self$mean, sigma = self$cov)              
            },
            density = function(x, log = TRUE){
              dmvnorm(x, self$mean, self$cov, log = log)              
            }           
          ))
