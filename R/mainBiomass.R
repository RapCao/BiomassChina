#' The main function of BiomassChina package.
#'
#' From the location identified by province, plant species, DBH, (and height) of
#' trees, this function gives an estimation of biomass of the total tree (or a
#' specific organ).
#'
#' @param province Vector of province names, Character. To be chosen between:
#'   - `Shanghai`: Shanghai City
#'   - `Zhejiang`: Zhejiang Porvince
#'   - `Jiangsu`: Jiangsu Porvince
#'   - `Hunan`: Hunan Porvince
#'   - `Hubei`: Hubei Porvince
#'   - `Jiangxi`: Jiangxi Porvince
#'   - `Anhui`: Anhui Porvince
#'   - `Chongqing`: Chongqing City
#'   - `Sichuan`: Sichuan Porvince
#'
#' @param species Vector of plant species in Latin, Character.
#' @param ifIncludeH Value of selected allometric equation (includes the parameter of tree height or not) to be used, Logical.
#' @param D Vector of the DBH of trees, Numeric.
#' @param H Vector of the height of trees, Numeric.
#' @param organ Value of selected components of tree biomass to be calculated and returned, Numeric. To be chosen between:
#'   - `0`: Total biomass, by adding up biomass of stem, branch, leaf and root
#'   - `1`: Biomass of stem
#'   - `2`: Biomass of branch
#'   - `3`: Biomass of leaf
#'   - `4`: Biomass of root
#'
#' @return Returns a dataframe include column(s) of biomass calculation results, Numeric.
#' @export
#'
#' @examples
#' # Load a database
#' data(testData)
#'
#' BiomassResult <- mainBiomass(testData$Province, testData$Species,
#'                              ifIncludeH = TRUE, D = testData$DBH,
#'                              H = testData$H, organ = 0)

mainBiomass <- function(province, species, ifIncludeH = FALSE,
						D = 0, H = 0, organ = 0) {

	prov = c()
	spe = c()
	for (i in 1:length(province)) {

		if (province[i] %in% c("Shanghai", "上海", "上海市")) {
			prov = c(prov, 1)
			if (species[i] == "Metasequoia glyptostroboides") {spe = c(spe, 1)}
			else if (species[i] == "Cinnamomum camphora") {spe = c(spe, 2)}
			else if (species[i] == "Ginkgo biloba") {spe = c(spe, 3)}
			else if (species[i] == "Ligustrum lucidum") {spe = c(spe, 4)}
			else if (species[i] == "Koelreuteria bipinnata") {spe = c(spe, 5)}
			else if (species[i] == "Elaeocarpus decipiens") {spe = c(spe, 6)}
			else if (species[i] == "Magnolia grandiflora") {spe = c(spe, 7)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 8)}
			else if (species[i] == "Mix") {spe = c(spe, 9)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Zhejiang", "浙江", "浙江省")) {
			prov = c(prov, 2)
			if (species[i] == "Pinus massoniana") {spe = c(spe, 1)}
			else if (species[i] == "Cryptomeria fortunei") {spe = c(spe, 2)}
			else if (species[i] == "Schima superba") {spe = c(spe, 3)}
			else if (species[i] == "Cyclobalanopsis glauca") {spe = c(spe, 4)}
			else if (species[i] == "Lithocarpus glaber") {spe = c(spe, 5)}
			else if (species[i] == "Quercus aliena") {spe = c(spe, 6)}
			else if (species[i] == "Castanopsis eyrei") {spe = c(spe, 7)}
			else if (species[i] == "Alniphyllum fortunei") {spe = c(spe, 8)}
			else if (species[i] == "Conifer") {spe = c(spe, 9)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 10)}
			else if (species[i] == "Mix") {spe = c(spe, 11)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Jiangsu", "江苏", "江苏省")) {
			prov = c(prov, 3)
			if (species[i] == "Cunninghamia lanceolata") {spe = c(spe, 1)}
			else if (species[i] == "Populus L.") {spe = c(spe, 2)}
			else if (species[i] == "Robinia pseudoacacia") {spe = c(spe, 3)}
			else if (species[i] == "Conifer") {spe = c(spe, 4)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 5)}
			else if (species[i] == "Mix") {spe = c(spe, 6)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Hunan", "湖南", "湖南省")) {
			prov = c(prov, 4)
			if (species[i] == "Castanopsis fargesii") {spe = c(spe, 1)}
			else if (species[i] == "Cupressus funebris") {spe = c(spe, 2)}
			else if (species[i] == "Cinnamomum camphora") {spe = c(spe, 3)}
			else if (species[i] == "Sassafras tzumu") {spe = c(spe, 4)}
			else if (species[i] == "Pinus massoniana") {spe = c(spe, 5)}
			else if (species[i] == "Schima superba") {spe = c(spe, 6)}
			else if (species[i] == "Cyclobalanopsis glauca") {spe = c(spe, 7)}
			else if (species[i] == "Lithocarpus glaber") {spe = c(spe, 8)}
			else if (species[i] == "Populus L.") {spe = c(spe, 9)}
			else if (species[i] == "Conifer") {spe = c(spe, 10)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 11)}
			else if (species[i] == "Mix") {spe = c(spe, 12)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Hubei", "湖北", "湖北省")) {
			prov = c(prov, 5)
			if (species[i] == "Cyclobalanopsis glauca") {spe = c(spe, 1)}
			else if (species[i] == "Quercus aliena") {spe = c(spe, 2)}
			else if (species[i] == "Liquidambar formosana") {spe = c(spe, 3)}
			else if (species[i] == "Conifer") {spe = c(spe, 4)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 5)}
			else if (species[i] == "Mix") {spe = c(spe, 6)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Jiangxi", "江西", "江西省")) {
			prov = c(prov, 6)
			if (species[i] == "Cunninghamia lanceolata") {spe = c(spe, 1)}
			else if (species[i] == "Liquidambar formosana") {spe = c(spe, 2)}
			else if (species[i] == "Conifer") {spe = c(spe, 3)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 4)}
			else if (species[i] == "Mix") {spe = c(spe, 5)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Anhui", "安徽", "安徽省")) {
			prov = c(prov, 7)
			if (species[i] == "Castanopsis sclerophylla") {spe = c(spe, 1)}
			else if (species[i] == "Populus L.") {spe = c(spe, 2)}
			else if (species[i] == "Lithocarpus glaber") {spe = c(spe, 3)}
			else if (species[i] == "Conifer") {spe = c(spe, 4)}
			else if (species[i] == "Broadleaf") {spe = c(spe, 5)}
			else if (species[i] == "Mix") {spe = c(spe, 6)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Chongqing", "重庆", "重庆市")) {
			prov = c(prov, 8)
			if (species[i] == "Pinus massoniana") {spe = c(spe, 1)}
			else if (species[i] == "Lithocarpus glaber") {spe = c(spe, 2)}
			else if (species[i] == "Quercus variabilis") {spe = c(spe, 3)}
			else {stop("Species wrong!");}
		}

		else if (province[i] %in% c("Sichuan", "四川", "四川省")) {
			prov = c(prov, 9)
			if (species[i] == "Pinus massoniana") {spe = c(spe, 1)}
			else if (species[i] == "Picea asperata") {spe = c(spe, 2)}
			else if (species[i] == "Betula") {spe = c(spe, 3)}
			else if (species[i] == "Cupressus funebris") {spe = c(spe, 4)}
			else {stop("Species wrong!");}
		}

		else {stop(paste("Province wrong, row =",i));}
	}


	valueSet = initValueSet()


	if (organ == 1) {
		stemBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 1)
		return (data.frame(stemBiomass))
	}
	else if (organ == 2) {
		branchBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 2)
		return (data.frame(branchBiomass))
	}
	else if (organ == 3) {
		leafBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 3)
		return (data.frame(leafBiomass))
	}
	else if (organ == 4) {
		rootBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 4)
		return (data.frame(rootBiomass))
	}
	else if (organ == 0) {
		stemBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 1)
		branchBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 2)
		leafBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 3)
		rootBiomass = calculate(ifIncludeH, valueSet, prov, spe, D, H, 4)
		abovegroundBiomass = stemBiomass + branchBiomass + leafBiomass
		totalBiomass = abovegroundBiomass + rootBiomass
		return (data.frame(stemBiomass, branchBiomass, leafBiomass, rootBiomass,
						   abovegroundBiomass, totalBiomass))
	}
	else {stop("Organ wrong!");}
}
