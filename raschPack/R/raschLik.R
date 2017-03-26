#' Calculating the likelihood of a proposed ability parameter
#'
#' Calculates the likelihood of a proposed ability parameter given the responses we observe
#' 
#' @param raschObj An object of class Rasch
#' @param theta A proposed ability parameter
#'
#' @return The likelihood of the proposed theta
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschLik
#' @export
setGeneric(name="raschLik",
           def=function(raschObj,theta,...)
           {standardGeneric("raschLik")}
           )

#' @export
setMethod(f="raschLik", signature="Rasch",
          definition= function(raschObj,theta,...){
            student_prob <- raschProb(raschObj, theta) 
            #we need the information yielded from the
            # probability function, so we feed the inputs
            # into that function
            likelihood <- prod(student_prob$prob_obs) 
            #the likelihood is the product of the observed
            # probabilities; i.e. if the student answers
            # correctly, y_ij=1 and thus we use P_ij; if
            # not, y_ij=0, and thus we use Q_ij
            return(likelihood) # we return the calculated likelihood
          }
          )