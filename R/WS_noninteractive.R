#' @title The simple Weighted Sum method
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#
#'
#' @description
#' Provides for input of subjective user judgements and subsequently applies
#' the simple Weighted Sum method in order to obtain ranking scores for the alternatives.
#'
#' @details
#' The function requires the
#' package \code{MCDA} which provides implementation of the \code{weightedSum} function.
#'
#'
#' @param{criteria.weights} {a numerical vector with named elements correspoding to the criteria
#' which provides the user's preference weights of the criteria (in any common scale)}
#' @param{alternatives.performance} { a numerical matrix with row names corresponding to the alternatives that are to be evaluated
#' and column names corresponding to the criteria which for each alternative provides a value (in any common scale) for its performance with respect to each criterion}
#'
#'
#'
#' @return A list containing:
#' \itemize{
#' \item\code{Weights} {a vector which contains subjective preference weights of the criteria as specified by the user}
#' \item\code{Table} {a matrix which corresponds to the subjective performance estimation of the alternatives with respect to the criteria as provided by the user and, in addition, contains the criteria weights as provided by the user, and the scores computed by the weighted sum heuristic for all alternatives}
#' \item\code{Scores} {a vector which for each alternative provides the weighted sum scores for the alternatives as computed by the process}
#' \item\code{Order} {a vector which contains the numbering (index) of the alternatives in decreasing order}
#' }
#'
#'
#' @examples
#' \dontrun{
#'
#'## Specify names of the criteria
#'
#'crit <- c("housing", "salary", "career", "recreation")
#'
#'## Specify names of the alternatives to be evaluated
#'
#'alt <-c("Stockholm", "Lulea", "Boden", "Malmoe", "Gothenburg")
#'
#'## Assign  (subjective) preference weights to criteria
#'
#'criteria.weights <- c(4, 10, 7, 3)
#'names(criteria.weights) <- crit
#'
#'## Create matrix to insert (subjective) performance measure of each alternative w.r.t. each criterion
#'
#'alternatives.performance <- matrix(nrow = length(alt), ncol = length(crit))
#'colnames(alternatives.performance) <- crit
#'rownames(alternatives.performance) <- alt
#'alternatives.performance[,] <- NaN
#'alternatives.performance <- fix(alternatives.performance)
#'
#'## Afte?? inserting (subjective) values, it should look something like this:
#'
#'
#' alternatives.performance
#'               housing salary career recreation
#' Stockholm        4      8      7          9
#' Lulea            5      9      6          5
#' Boden            8      9      6          3
#' Malmoe           3      7      8          8
#' Gothenburg       5      7      7          7
#'
#'## Evaluate using Weighted Sum heuristic
#'
#'res <- WS_noninteractive(criteria.weights, alternatives.performance)
#'
#'## View the runking result
#'
#'res$Scores
#' Stockholm      Lulea      Boden     Malmoe Gothenburg
#'     172        167        173        162        160
#'
#'res$Order
#'3 1 2 4 5
#'
#'## Sort the alternatives according to this order
#'
#'alt[res$Order]
#'"Boden"      "Stockholm"  "Lulea"      "Malmoe"     "Gothenburg"
#'
#'## or sort as follows
#'
#'res$Scores[res$Order]
#'Boden  Stockholm      Lulea     Malmoe Gothenburg
#' 173        172        167        162        160
#'}
#'
#'
#' @export
WS_noninteractive <- function(criteria.weights, alternatives.performance){

#  criteria <- names(criteria.weights) <- criteria
#  alternatives <- rownames(alternatives.performance)


  res <- MCDA::weightedSum(performanceTable=alternatives.performance,
                           criteriaWeights=criteria.weights)

  ind <- 1:length(res)
  ord <- ind[order(res,decreasing =TRUE)]

  alternatives.performance <- rbind(Weights=criteria.weights,alternatives.performance)
  alternatives.performance <- cbind(alternatives.performance,Scores=c(NaN,res))

  return(list(Weights=criteria.weights, Table=alternatives.performance, Scores=res, Order=ord))

}
