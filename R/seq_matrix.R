#' A Sequentially Numbered Matrix Creating Function
#'
#' This function creates matrices of identical size and name, with a numeric identifier
#' @param number: The number of matrices you wish to create
#' @param name: The common name of the matrices
#' @param nrow: The number of rows you want to be present in the matrices you create
#' @param ncol: The number of columns you want to be present in teh matrices you create
#' @keywords Matrix Multiple Icarus
#' @export
#' @examples
#' seq.matrix()

seq.matrix <- function(number, name, nrow, ncol){
  matrix.base <- matrix(NA, nrow, ncol) # Creates a matrix of the specified size
  for( i in 1:number ){
    matrix.name <- paste(name, i, sep="") # Creates Matrix name with appended number
    assign(matrix.name, matrix.base, envir = .GlobalEnv) # Assigns names to created matrices
  }
}
