#' @title Likert Data Envelopment Analysis (LDEA)
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#
#'
#' @description
#' The function implements a Data Envelopment Analysis model/procedure suitable
#' for data in Likert scale. Likert Data Envelopment Analysis (LDEA) is
#' based on Mixed Integer Programming (MIP) methodology and is used to measure
#' the productive efficiency of decision making units (DMUs).
#'
#'
#' @details
#' The function requires the package \code{Rglpk} which is based on \code{glpk} and provides
#' implementation of the necessary functionality for the application of LDEA. The GNU linear programming kit (\code{glpk})
#' is a Linear and Mixed Integer Programming solver (and much more) and is available to R through the package \code{Rglpk}.
#'
#'
#'
#' @param{Input} { a matrix whose rows correspond to DMUs to be evaluated and the columns
#'  to the inputs with respect to which the evaluation is to be performed}
#' @param{Output} { a matrix whose rows correspond to DMUs, the exact same as those of the input and in the same order,
#'  to be evaluated and the columns
#'  to the outputs with respect to which the evaluation is to be performed}
#' @param{Likert} { maximum value of Likert scale in use. Default value is 5.}
#'
#'
#' @return A \code{Table} {which is a data frame completely describing the result; the rows correspond to the evaluated
#'                    DMUs and the columns to \code{Theta}  and the DMUs. For an inefficient DMU, i.e., with \code{Theta} < 1,
#'                    non-zero values (\code{Lamb}das) on its row indicate the benchmark DMUs. A final column \code{Status}
#'                    indicates with a \code{0}-value
#'                    whether the corresponding optimization problem has been solved.}
#'
#'
#' @examples
#' \dontrun{
#'
#' ## -- Example 1 -- ##
#' ## Load example data
#'
#' data("LikertInputs")
#' data("LikertOutputs")
#'
#' ## Apply Likert DEA
#'
#' table <- LDEA_noninteractive(Input = LikertInputs, Output = LikertOutputs, Likert=5)
#'
#' ## View result
#'
#' table
#'
#' ## View requested improvements for inefficient DMUs
#'
#' res$Improvement
#'
#' @references
#' - Charnes, A., Cooper, W. W. & Rhodes, E. (1978) Measuring the Efficiency of Decision Making Units, European Journal of Operational Research 2 (6) 429–444
#' - Charnes, A., Cooper, W. W., Lewin, A. Y.  & Seiford, L. M. (1994) Data Envelopment Analysis: Theory, Methodology, and Applications, Springer
#' - Cooper, W. W., Seiford, L. M. &  Tone, K. (2007) Data Envelopment Analysis - A Comprehensive Text with Models, Applications, References and DEA-Solver Software, 2nd Edition, Springer
#' - Chen, Y., Cook, W. D., Du, J., Hu, H-. & Zhu, J. (2017) Bounded and Discrete Data and Likert Scales in Data Envelopment Analysis: Application to Regional Energy Efficiency in China, Annals of Operations Research 255, 247-366
#'
#'
#' @export
LDEA_noninteractive <- function(Input, Output, Likert=5){

  n <- nrow(Input)                 # DMUs

  if( n <= 1 ) {return(NULL)}
  n2 <- nrow(Output)
  if( n != n2 ) {return(NULL)}

  m <- ncol(Input)                 # Inputs

  s <- ncol(Output)                # Outputs

  L <- Likert

  # DMUs <- rownames(Input)

  #
  # Function to solve the problem for a given DMU
  #

  LikertDEAint <- function(L, dmu, Input, Output){

  # Determine sizes

    n <- nrow(Input)    # DMUs
    m <- ncol(Input)    # Inputs
    s <- ncol(Output)   # Outputs

    # Form the constraint mattrix

    con <- matrix(nrow=m+s+s, ncol=n+s+s)
    con[1:m, 1:n] <- t(Input)
    con[1:m, (n+1):(n+s)] <- 0
    con[1:m, (n+s+1):(n+s+s)] <- 0

    con[(m+1):(m+s), 1:n] <- t(Output)
    con[(m+1):(m+s), (n+1):(n+s+s)] <- 0
    for(i in 1:s){ con[m+i,n+s+i] <- -1 }

    con[(m+s+1):(m+s+s),1:n] <- 0
    con[(m+s+1):(m+s+s),(n+1):(n+s)]<-0
    con[(m+s+1):(m+s+s), (n+s+1):(n+s+s)]<-0
    for(i in 1:s) { con[(m+s+i),(n+i)] <- -Output[dmu,i] # for DMU
                  con[(m+s+i),(n+s+i)] <- 1 }

    # Form the right-hand side

    rhs <- vector(length=m+s+s)
    rhs[1:m] <- Input[dmu,]     # for DMU
    rhs[(m+1):(m+s+s)] <- 0

    # Form the objective coefficients

    obj <- vector(length=n+s+s)
    obj[1:n] <- 0
    obj[(n+1):(n+s)] <- 1
    obj[(n+s+1):(n+s+s)] <- 0

    # Direction of the constraints

    dir <- c( rep("<=", m), rep(">=", (s+s)) )

    # Lower Bounds on the variables

    ind <- integer(length=n+s+s)
    ind <- 1:(n+s+s)

    val <- vector(length=n+s+s)
    val[1:n] <- 0
    val[(n+1):(n+s+s)] <-1

    lower <- list( ind = ind,  val = val )

    # Upper Bounds on the a-variables

    ind2 <- integer(length=s)
    val2 <- vector(length=s)

    ind2 <- (n+s+1):(n+s+s)
    val2[] <- L

    upper<- list( ind = ind2, val = val2)

    # Bounds

    bounds <- list( lower=lower, upper=upper )

    # Variable types

    vartyp <- c(rep('C',(n+s)), rep('I',s))

    # Solve the problem

    sol <- Rglpk_solve_LP( obj=obj/s, mat=con, dir=dir, rhs=rhs, bounds=bounds, types=vartyp, max=TRUE )

    return(sol)

  }

  #
  # Main procedure
  #

  # prepare output frame (efficiency and lambdas)

  Table <- data.frame( matrix(nrow=n, ncol=n+2) )
  colnames(Table) <- c("Theta",paste("Lamb",seq(1,n),sep=""),"Status")

  # loop over the dmus

  for(dmu in 1:n){
    sol <- LikertDEAint(L,dmu,Inputs,Outputs)
    Table$Theta[dmu] <- 1/sol$optimum
    Table[dmu,2:(n+1)] <- sol$solution[1:n]
    Table[dmu,n+2] <- sol$status
  }

  # write.xlsx(res,paste("glpkINTOUT",datafile,sep=""))

  return(Table)

}
