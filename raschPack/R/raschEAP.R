#' Calculating the expected a posteriori ability parameter value
#'
#' Calculates the expected a psoteriori value for the ability parameter for a student given
#' their answers and the difficulty of the questions by integrating over the product of the
#' likelihood and the prior within set limits of integration
#' 
#' @param raschObj An object of class Rasch
#' @param lower The lower limit of integration; default is -6
#' @param upper The upper limit of integration; default is -6
#'
#' @return The expected a posteriori ability parameter 
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschEAP
#' @export
setGeneric(name="raschEAP",
           def=function(raschObj,lower=-6,upper=6,...)
           {standardGeneric("raschEAP")}
           )

#' @export
setMethod(f="raschEAP",
          definition= function(raschObj,lower=-6 ,upper=6, ...){
            # creating EAP function; takes in student object
            # and integration limits, defaulted to -6 and 6
            integrand <- function(x) raschLik(raschObj, x) * raschPrior(x) 
            # specifying function to integrate, we want to integrate the likelihood
            # times the prior, given our data
            posterior <- integrate(Vectorize(integrand), lower=lower, upper=upper) # we use 
            # the integrand in the integrate function to yield
            # our expected a posteriori value initially, the integrate
            # function threw a warning that the object lengths of the
            # inputs did not match; the help file suggested using
            # Vectorize() to get the function in the correct form
            return(posterior)
          }
          )