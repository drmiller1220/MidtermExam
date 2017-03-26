#' Calculating a prior for Rasch estimation
#'
#' Calculates the prior for Rasch estimation given a proposed ability parameter.  The prior
#' probability distribution is fixed as a normal distribution with mean 0 and standard
#' deviation 3
#' 
#' @param theta A proposed ability parameter
#'
#' @return The prior probability of the proposed ability parameter
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschPrior
#' @export
setGeneric(name="raschPrior",
           def=function(theta,...)
           {standardGeneric("raschPrior")}
           )

#' @export
setMethod(f="raschPrior",
          definition= function(theta,...){
            priorprob <- dnorm(theta, mean=0, sd=3) #function to yield a prior probability for the 
                                          # proposed ability parameter, which is the height
                                          # of the normal distribution at the proposal
            return(priorprob)
          }
          )