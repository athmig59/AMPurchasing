#' @title Data Envelopment Analysis (DEA)
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#
#'
#' @description
#' The function provides an interface for "easy" input to and "structured" output from DEA.
#' Data Envelopment Analysis (DEA) is a Linear Programming based methodology used to measure
#' the productive efficiency of decision making units (DMUs).  DEA's assumptions are
#' relatively simple which makes it broadly applicable, particularly, as it is also
#' able to benchmark multi-dimensional inputs and outputs.
#' Computationally it is an efficient and easy approach as it is completely based on solving
#' linear programming problems.
#' There are several variants of the approach; e.g., output or input oriented but also based
#' on the assumed "returns to scale" (RTS) which is a term that refers to the proportionality of changes
#' in output after the amounts of all inputs in production have been changed by the same factor.
#' There can be variable; increasing, decreasing, or constant returns to scale.
#' The function applies output oriented DEA with variable RTS.
#'
#'
#' @details
#' The function requires the package \code{rDEA} which is based on \code{glpk} and provides
#' implementation of the necessary functionality for the application of DEA. The GNU linear programming kit (\code{glpk})
#' is a Linear Programming solver (and much more) and is available to R through the package \code{Rglpk}.
#'
#'
#' @param{Input} { a data frame whose rows correspond to DMUs to be evaluated and the columns
#'  to the inputs with respect to which the evaluation is to be performed}
#' @param{Output} { a data frame whose rows correspond to DMUs, the exact same as those of the input and in the same order,
#'  to be evaluated and the columns
#'  to the outputs with respect to which the evaluation is to be performed}
#'
#' @return A list containing;
#' \itemize{
#' \item\code{Table} {is a data frame completely describing the result; the rows correspond to the evaluated
#'                    DMUs and the columns to \code{Theta}  and the DMUs. For an inefficient DMU, i.e., with \code{Theta} < 1,
#'                    non-zero values (\code{lambda}s) on its row indicate the benchmark DMUs.}
#' \item\code{Improvement} {is data frame which for each inefficient DMU and a corresponding benchmark DMU shows
#'                    the required improvement of each input in order for the inefficient DMU to reach the benchmark. }
#' }
#'
#'
#' @examples
#' \dontrun{
#'
#' ## -- Example 1 -- ##
#' ## Load example data
#'
#' data("Hamburger.Input")
#' data("Hamburger.Output")
#'
#' ## Apply DEA
#'
#' res <- DEA_noninteractive(Input = Hamburger.Input, Output = Hamburger.Output)
#'
#' ## View result
#'
#' res$Table
#'
#' ## View requested improvements for inefficient DMUs
#'
#' res$Improvement
#'
#' ## -- Example 2 -- ##
#' ## Data in a spreadheet or a dataframe containing both Inputs and Outputs
#' ## needs to be splitted
#'
#' Data <- data("Hamburger")
#'
#' ## Check the data
#'
#' head(Data)
#'
#' ## Split it accordingly
#'
#' Output <- Data[c("Profit","Satisfaction","Cleanliness")]
#' Input <- Data[c("Labor.Hrs","Op..Costs")]
#'
#' ## Apply DEA
#'
#' res <- DEA_noninteractive(Input = Input, Output = Output)
#'
#' ## View result
#'
#' res$Table
#'
#' ## View requested improvements for inefficient DMUs
#'
#' res$Improvement
#'
#' }
#'
#' @references
#' - Charnes, A., Cooper, W. W. & Rhodes, E. (1978) Measuring the Efficiency of Decision Making Units, European Journal of Operational Research 2 (6) 429–444
#' - Charnes, A., Cooper, W. W., Lewin, A. Y.  & Seiford, L. M. (1994) Data Envelopment Analysis: Theory, Methodology, and Applications, Springer
#' - Cooper, W. W., Seiford, L. M. &  Tone, K. (2007) Data Envelopment Analysis - A Comprehensive Text with Models, Applications, References and DEA-Solver Software, 2nd Edition, Springer
#'
#' @export
DEA_noninteractive <- function(Input, Output){

  n1 <- nrow(Input)
  if( n1 <= 1 ) {return(NULL)}
  n2 <- nrow(Output)
  if( n1 != n2 ) {return(NULL)}

  DMUs <- rownames(Input)
  m <- length(DMUs)

  Input.resources <- names(Input)
  Output.results <- names(Output)

  deamodel <- rDEA::dea(XREF=Input,YREF=Output,X=Input,Y=Output,model="output", RTS="variable")

  Table <- cbind(round(deamodel$thetaOpt,3),round(deamodel$lambda,3))
  row.names(Table)<-DMUs
  colnames(Table)<-c("Theta",DMUs)

  n <- ncol(Table)

  no <- ncol(Input)
  nr <- sum( Table[,"Theta"] < 1 )
  Improvement <- matrix(ncol =2 + no, nrow = nr)
  Improvement[,] <- NaN
  Improvement <- data.frame(Improvement)
  names(Improvement) <- c("DMU", "Benchmark", Input.resources)

  r <- 1
  for(i in 1:m){
    if( Table[i,"Theta"] < 1 ){
      row <- Table[i,2:n]
      benchmark <- which( row != 0)
      k <- length(benchmark)
      if(k >= 1){
        b <- benchmark[1]
        howmuch <- Input[DMUs[i],] - Input[DMUs[b],] * Table[DMUs[i],DMUs[b]]
        Improvement[r,"DMU"] <- DMUs[i]
        Improvement[r, "Benchmark"] <- DMUs[b]
        Improvement[r,Input.resources] <- howmuch
        r <- r + 1
        if(k >= 2){
          for(j in 2:k){
            b <- benchmark[j]
            howmuch <- Input[DMUs[i],] - Input[DMUs[b],] * Table[DMUs[i],DMUs[b]]
            Improvement[r,"DMU"] <- DMUs[i]
            Improvement[r, "Benchmark"] <- DMUs[b]
            Improvement[r,Input.resources] <- howmuch
            r <- r + 1
          }
        }
      }
    }
  }

  return(list(Table=Table, Improvement=Improvement))
}
