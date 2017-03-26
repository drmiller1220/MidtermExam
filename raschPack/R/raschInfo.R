#' Calculating the Fisher's Information for each question
#'
#' To calculate the amount of Fisher's Information we obtain for a student with a given
#' ability parameter for each question item, we evaluate the negative second derivative of
#' the likelihood.
#'
#' @param raschObj An object of class Rasch
#' @param theta A proposed ability parameter
#'
#' @return A vector of the Fisher's Information for each question
#' @author David R. Miller <\email{drmiller@@wustl.edu}>
#' 
#' @rdname raschInfo
#' @export
setGeneric(name="raschInfo",
           def=function(raschObj,theta,...)
           {standardGeneric("raschInfo")}
           )

#' @export
setMethod(f="raschInfo",
          definition= function(raschObj,theta,...){
            prob_correct <- raschProb(raschObj,theta)$prob_corr # obtain P_ij given inputs
            fish_inf <- prob_correct*(1-prob_correct) # multiply vectorwise P_ij and Q_ij
            return(fish_inf)
          }
          )