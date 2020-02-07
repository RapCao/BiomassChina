##Set working directory.
#setwd("E:/Administrator/Documents/BiomassChina")


##Install package.
#library(devtools)
#devtools::install_github("RapCao/BiomassChina")


##Test examples.
#library(xlsx)
#library(BiomassChina)

testData <- read.xlsx("~/tests/testthat/testData.xlsx", sheetName="Sheet1", header=T)

result1 = BiomassChina::mainBiomass(testData$Province, testData$Species, ifIncludeH=TRUE, D = testData$DBH, H = testData$H, organ=0)
result2 = BiomassChina::mainBiomass(testData$Province, testData$Species, ifIncludeH=TRUE, D = testData$DBH, H = testData$H, organ=1)
result3 = BiomassChina::mainBiomass(testData$Province, testData$Species, ifIncludeH=FALSE, D = testData$DBH, H = testData$H, organ=0)
result4 = BiomassChina::mainBiomass(testData$Province, testData$Species, ifIncludeH=FALSE, D = testData$DBH, H = testData$H, organ=4)

write.xlsx2(result1, file="result.xlsx", sheetName="result1", append=TRUE)
write.xlsx2(result2, file="result.xlsx", sheetName="result2", append=TRUE)
write.xlsx2(result3, file="result.xlsx", sheetName="result3", append=TRUE)
write.xlsx2(result4, file="result.xlsx", sheetName="result4", append=TRUE)
