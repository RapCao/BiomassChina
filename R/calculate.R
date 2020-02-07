#' Finding the constant parameters and substituting all parameters into the equation.
#'
#' From the numerically indexed province names and plant species, this function
#' finds the constant parameters of the corresponding equation in the value set
#' of all parameters, and then gives the calculation results the equation returned.
#'
#' @param ifIncludeH Value of selected allometric equation (includes the parameter of tree height or not) to be used, Logical.
#' @param valueSet List of all parameters, Numeric.
#' @param prov Vector of numerically indexed province names, Numeric.
#' @param spe Vector of numerically indexed plant species, Numeric.
#' @param D Vector of the DBH of trees, Numeric.
#' @param H Vector of the height of trees, Numeric.
#' @param resChoose Value of selected components of tree biomass to be calculated and returned, Numeric. To be chosen between:
#'   - `1`: Biomass of stem
#'   - `2`: Biomass of branch
#'   - `3`: Biomass of leaf
#'   - `4`: Biomass of root
#'
#' @return Returns a vecotr of calculation results of tree biomass.
#'
#' @examples
#' # Load a database
#' data(testData)
#'
#' # Initialize a value set of all parameters
#' valueSet = initValueSet()
#'
#' calculate(ifIncludeH = TRUE, valueSet = valueSet, prov = c(1, 2, 3),
#'           spe = c(1, 2, 3), D = testData$DBH, H = testData$H, resChoose = 1)

calculate <- function(ifIncludeH = FALSE, valueSet, prov = 0, spe = 0,
					  D = 0, H = 0, resChoose = 0) {

	if (resChoose != 0) {
		a = c()
		b = c()
		for (i in 1:length(prov)) {
			a = c(a, valueSet[[(prov[i])]][[(spe[i])]][[(ifIncludeH + 1)]][[1]][resChoose])
			b = c(b, valueSet[[(prov[i])]][[(spe[i])]][[(ifIncludeH + 1)]][[2]][resChoose])
		}
		output = equation(ifIncludeH, a, b, D, H)
		return (output)
	}
	else {stop("Result choosing wrong!")}
}
