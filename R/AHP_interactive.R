#' @title Analytic Hierarchy Process
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#'
#' @description
#' Provides for interactive input of subjective user comparisons and suppsequently applies
#' the Analytic Hierarchy Process (AHP) method of T. L. Saaty in order to obtain ranking
#' scores for the alternatives.
#'
#' IMPORTANT NOTE: To simplify input and avoid rounding errors, instead of the reciprocal of a number, insert the negative of the number!
#' That is, instead of 1/x or a decimal approximationof it,  insert -x, for x in {1,2,3,4,5,6,7,8,9}.
#'
#' @details
#' The function requires the package \code{tcltk} to pop up interactive messages and the
#' package \code{MCDA} which provides implementation of the \code{AHP} function.
#'
#' @param{criteria} { a vector of character elements naming the criteria under consideration}
#' @param{alternatives} { a vector of character elements naming the alternatives to be evaluated under the specified criteria}
#'
#'
#' @return A list containing:
#' \itemize{
#' \item{a matrix} {which corresponds to the subjective  pairwise criteria comparisons as provided by the user}
#' \item{a list} {which contains one matrix for each criterion; the matrix provides the subjective pairwise comparisons of the alternatives for the specific criterion as specified by the user }
#' \item{a vector} {which for each alternative provides the AHP scores computed by the process}
#' }
#'
#' @references
#'  Saaty, T. L. (1980)  The Analytic Hierarchy Process: Planning, Priority Setting, Resource Allocation; McGraw-Hill
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' ## -- Example -- ##
#' crit <- c("housing", "salary", "career", "recreation")
#' alt <-c("Stockholm", "Lulea", "Boden", "Malmoe", "Gothenburg")
#' AHP_interactive(criteria=crit, alternatives=alt)
#' }
### if(!require("tcltk")){ install.packages("tcltk") }
### if(!require("MCDA")){ install.packages("MCDA") }
#'
#'
#' @export
AHP_interactive <- function(criteria, alternatives){

  ### Subjective pairwise comparison of the criteria

  lc <- length(criteria)
  if( lc <= 1 ){ return(NULL) }

  tcltk::tkmessageBox(title="AHP Message", message="You are requested to provide\nthe pairwise comparisons of the criteria.\nInsert values only above the diagonal (above the 1s)!\n\nClick OK to proceed!")

  pairwise.criteria.comparison <- matrix(nrow=lc, ncol=lc)

  for(i in 1:lc){ pairwise.criteria.comparison[i,i] = 1 }

  #dimnames(paiwise.criteria.comparison) <- list(criteria, criteria)
  rownames(pairwise.criteria.comparison) <- criteria
  colnames(pairwise.criteria.comparison) <- criteria

  pairwise.criteria.comparison <- edit(pairwise.criteria.comparison)

  for(i in 1:(lc-1)){
    if((i+1) > lc) {break}
    for(j in (i+1):lc){
      if(pairwise.criteria.comparison[i,j] >= 1 && pairwise.criteria.comparison[i,j] <= 9){
         pairwise.criteria.comparison[j,i] = 1 / pairwise.criteria.comparison[i,j]
      } else
           if(pairwise.criteria.comparison[i,j] <= -1 && pairwise.criteria.comparison[i,j] >= -9){
             pairwise.criteria.comparison[i,j] = 1/ abs(pairwise.criteria.comparison[i,j])
             pairwise.criteria.comparison[j,i] = 1 / pairwise.criteria.comparison[i,j]
           } else { message("*** ERROR: Given values do not comply to required scale! ***")
                   return(NA)}
    }
  }

  # print(pairwise.criteria.comparison)

  ### Subjective pairwise comparison of the alternatives with respect to each criterion

  la <- length(alternatives)
  if( la <= 1 ){ return(NULL) }

  comparisons.list <- list()


  pairwise.alternatives.comparison <- matrix(nrow=la, ncol=la)

  rownames(pairwise.alternatives.comparison) <- alternatives
  colnames(pairwise.alternatives.comparison) <- alternatives

  tcltk::tkmessageBox(title="AHP Message", message="You are requested to provide\nthe pairwise comparisons of the alternatives with respect to each criterion.\nThus, several matrices need to be filled in. Insert values only above the diagonal (above the 1s)!\n\nClick OK to proceed!")

  for(c in 1:lc){

    msg <- paste("Fill in comparisons of the alternatives for the criterion of ", criteria[c],"\n\nClick OK to proceed")
    tcltk::tkmessageBox(title=criteria[c], message=msg)

    for(i in 1:la){ pairwise.alternatives.comparison[i,i] = 1 }

    pairwise.alternatives.comparison <- edit(pairwise.alternatives.comparison)

    for(i in 1:(la-1)){
      if((i+1) > la) {break}
      for(j in (i+1):la){
        if(pairwise.alternatives.comparison[i,j] >= 1 && pairwise.alternatives.comparison[i,j] <= 9){
          pairwise.alternatives.comparison[j,i] = 1 / pairwise.alternatives.comparison[i,j]
        } else
          if(pairwise.alternatives.comparison[i,j] <= -1 && pairwise.alternatives.comparison[i,j] >= -9){
            pairwise.alternatives.comparison[i,j] = 1 / abs( pairwise.alternatives.comparison[i,j])
            pairwise.alternatives.comparison[j,i] = 1 / pairwise.alternatives.comparison[i,j]
          } else { message("*** ERROR: Given values do not comply to required scale! ***")
            return(NA)}
      }
    }

    # print(pairwise.alternatives.comparison)

    comparisons.list[[c]] <- pairwise.alternatives.comparison

    pairwise.alternatives.comparison[,] <- NaN
  }

  names(comparisons.list) <- criteria

  ahp_res <- MCDA::AHP(criteriaWeightsPairwiseComparisons=pairwise.criteria.comparison, alternativesPairwiseComparisonsList=comparisons.list)

 return(list(pairwise.criteria.comparison, comparisons.list, ahp_res))
}
