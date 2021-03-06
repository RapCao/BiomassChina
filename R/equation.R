#' Calculating the equation.
#'
#' Based on the selected allometric equation, this function gives the calculation results of tree biomass
#' by substituting all constant and variable parameters into the equation.
#'
#' @param ifIncludeH Value of selected allometric equation (includes the parameter of tree height or not) to be used, Logical.
#' @param a Vector of the constant parameters 'a' of the equation, Numeric.
#' @param b Vector of the constant parameters 'b' of the equation, Numeric.
#' @param D Vector of the DBH of trees, Numeric.
#' @param H Vector of the height of trees, Numeric.
#'
#' @return Returns a vecotr of calculation results of the equation.
#'
#' @examples
#' # Load a database
#' data(testData)
#'
#' # Get constant parameters from a value set
#' valueSet = initValueSet()
#' VA = valueSet[[(1)]][[(1)]][[(1)]][[1]][1]
#' VB = valueSet[[(1)]][[(1)]][[(1)]][[2]][1]
#'
#' equation(ifIncludeH = TRUE, a = VA, b = VB, D = testData$DBH, H = testData$H)

equation <- function(ifIncludeH = FALSE, a = 0, b = 0, D = 0, H = 0) {

	if (ifIncludeH == FALSE) {
		W = a * (D ^ b)
		return (W)
	}
	else {
		W = a * (((D ^ 2) * H) ^ b)
		return (W)
	}
}
