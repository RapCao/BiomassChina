#' Initializing the constant parameters of the allometric equations.
#'
#' From the parameters stored in code, this function builds and returns a list,
#' which is from top to bottom nested provinces, plant species, equations, and
#' vectors of two parameters identified by plant organs.
#'
#' @return Returns a list, with all parameters indexable, Numeric.
#' @references Zhou et al. _Carbon stocks of forest ecosystems in China: biomass equations_. Beijing: Science Press (2018): 48-52.
#' @export
#'
#' @examples
#' valueSet = initValueSet()

initValueSet <- function() {

    ##Shanghai
    #Metasequoia glyptostroboides
    a.1 <- c(0.0285, 0.0402, 0.1317, 0.0172)
    b.1 <- c(2.6604, 1.9898, 1.2972, 2.3939)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0146, 0.0243, 0.0949, 0.0102)
    b.2 <- c(0.9835, 0.7355, 0.4795, 0.8745)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Cinnamomum camphora
    a.1 <- c(0.1048, 0.0122, 0.0014, 0.0354)
    b.1 <- c(2.1408, 2.8242, 3.2254, 2.4069)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0914, 0.0099, 0.0011, 0.0298)
    b.2 <- c(0.7755, 1.0256, 1.1713, 0.8740)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Ginkgo biloba
    a.1 <- c(0.2791, 0.0495, 0.0379, 0.0948)
    b.1 <- c(1.9384, 1.9768, 1.9819, 1.8665)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1114, 0.0193, 0.0159, 0.0394)
    b.2 <- c(0.8538, 0.8715, 0.8598, 0.8208)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Ligustrum lucidum
    a.1 <- c(0.0671, 0.0447, 0.0915, 0.1071)
    b.1 <- c(2.2525, 2.2179, 1.6221, 1.6211)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1833, 0.0972, 0.1495, 0.1258)
    b.2 <- c(0.6504, 0.6441, 0.5085, 0.5647)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Koelreuteria bipinnata
    a.1 <- c(0.0542, 0.0035, 0.0002, 0.0471)
    b.1 <- c(2.4947, 3.1747, 3.5250, 2.3304)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.011, 0.0005, 0.00003, 0.0103)
    b.2 <- c(1.0866, 1.3656, 1.5140, 1.0205)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Elaeocarpus decipiens
    a.1 <- c(0.0632, 0.0064, 0.0013, 0.0865)
    b.1 <- c(2.2990, 2.8926, 2.8681, 1.7492)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0085, 0.0005, 0.0001, 0.0185)
    b.2 <- c(1.0909, 1.3817, 1.3770, 0.8328)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    #Magnolia grandiflora
    a.1 <- c(0.0825, 0.0529, 0.0360, 0.1047)
    b.1 <- c(2.1663, 1.7864, 2.2691, 1.8103)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0640, 0.0431, 0.0254, 0.0885)
    b.2 <- c(0.8131, 0.6697, 0.8701, 0.6713)
    s.2 <- list(a.2, b.2)
    s7 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.0752, 0.0088, 0.0001, 0.0229)
    b.1 <- c(2.2630, 2.9250, 3.2927, 2.5317)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0642, 0.0071, 0.0001, 0.0192)
    b.2 <- c(0.8160, 1.0547, 1.1872, 0.9129)
    s.2 <- list(a.2, b.2)
    s8 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.0834, 0.0165, 0.0144, 0.0473)
    b.1 <- c(2.2540, 2.5231, 2.2725, 2.1610)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0568, 0.0107, 0.0097, 0.0327)
    b.2 <- c(0.8262, 0.9254, 0.8342, 0.7923)
    s.2 <- list(a.2, b.2)
    s9 <- list(s.1, s.2)

    p1 <- list(s1, s2, s3, s4, s5, s6, s7, s8, s9)


    ##Zhejiang
    #Pinus massoniana
    a.1 <- c(0.2931, 0.0048, 0.0670, 0.0532)
    b.1 <- c(1.9700, 2.6300, 1.4600, 1.9800)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0811, 0.0025, 0.0274, 0.0395)
    b.2 <- c(0.8497, 0.9814, 0.6188, 0.7311)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Cryptomeria fortunei
    a.1 <- c(0.2923, 0.0054, 0.1839, 0.0969)
    b.1 <- c(1.8469, 2.5421, 1.5090, 1.9252)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1220, 0.0016, 0.0901, 0.0390)
    b.2 <- c(0.7668, 1.0555, 0.6265, 0.7993)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Schima superba
    a.1 <- c(0.2567, 0.1043, 0.0393, 0.0811)
    b.1 <- c(1.9900, 1.7400, 1.6500, 1.9300)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1674, 0.0719, 0.0276, 0.0537)
    b.2 <- c(0.7539, 0.6582, 0.6223, 0.7280)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Cyclobalanopsis glauca
    a.1 <- c(0.2861, 0.2059, 0.0340, 0.1330)
    b.1 <- c(1.9800, 1.5600, 1.6900, 1.8600)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1594, 0.0717, 0.0131, 0.0767)
    b.2 <- c(0.7646, 0.7296, 0.7214, 0.7192)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Lithocarpus glaber
    a.1 <- c(0.1870, 0.0735, 0.0230, 0.0750)
    b.1 <- c(2.0310, 1.9800, 1.9700, 2.0100)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0534, 0.0219, 0.0093, 0.0376)
    b.2 <- c(0.8873, 0.8644, 0.8220, 0.8103)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Quercus aliena
    a.1 <- c(0.0283, 0.0409, 0.0093, 0.0183)
    b.1 <- c(2.7720, 2.3479, 2.4441, 2.5465)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0116, 0.0190, 0.0042, 0.0081)
    b.2 <- c(1.0424, 0.8838, 0.9194, 0.9576)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    #Castanopsis eyrei
    a.1 <- c(0.0686, 0.0788, 0.0182, 0.0436)
    b.1 <- c(2.2830, 1.9990, 2.1764, 2.1659)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0249, 0.0341, 0.0070, 0.0222)
    b.2 <- c(0.9447, 0.8208, 0.9000, 0.8543)
    s.2 <- list(a.2, b.2)
    s7 <- list(s.1, s.2)

    #Alniphyllum fortunei
    a.1 <- c(1.2825, 0.3015, 0.8596, 0.3907)
    b.1 <- c(1.3725, 1.4613, 0.8125, 1.2656)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.8170, 0.1865, 0.6580, 0.2587)
    b.2 <- c(0.5233, 0.5571, 0.3098, 0.4825)
    s.2 <- list(a.2, b.2)
    s8 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.0407, 0.0083, 0.1517, 0.0305)
    b.1 <- c(2.6178, 2.3895, 1.537, 2.063)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.028, 0.006, 0.123, 0.023)
    b.2 <- c(0.96, 0.876, 0.564, 0.757)
    s.2 <- list(a.2, b.2)
    s9 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.2562, 0.1187, 0.0161, 0.1525)
    b.1 <- c(1.9908, 1.7757, 2.0046, 1.8958)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.119, 0.059, 0.007, 0.0847)
    b.2 <- c(0.87, 0.776, 0.876, 0.7794)
    s.2 <- list(a.2, b.2)
    s10 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.0401, 0.0168, 0.0482, 0.0734)
    b.1 <- c(2.6307, 2.1726, 1.8939, 1.831)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.02, 0.009, 0.03, 0.046)
    b.2 <- c(0.997, 0.823, 0.718, 0.694)
    s.2 <- list(a.2, b.2)
    s11 <- list(s.1, s.2)

    p2 <- list(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11)


    ##Jiangsu
    #Cunninghamia lanceolata
    a.1 <- c(0.1738, 0.0633, 1.0590, 0.0984)
    b.1 <- c(1.8304, 1.5827, 0.4512, 1.5396)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1083, 0.0477, 0.9680, 0.0733)
    b.2 <- c(0.6895, 0.5778, 0.1661, 0.5649)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Populus L.
    a.1 <- c(0.2770, 0.2198, 0.1633, 0.0558)
    b.1 <- c(1.9617, 1.6440, 1.5640, 1.9226)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0310, 0.0607, 0.0314, 0.0184)
    b.2 <- c(0.8859, 0.6745, 0.6540, 0.7550)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Robinia pseudoacacia
    a.1 <- c(0.1993, 0.4828, 0.0080, 0.0228)
    b.1 <- c(2.3867, 1.5623, 2.7510, 2.6026)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.2382, 0.6379, 0.0122, 0.0274)
    b.2 <- c(0.8312, 0.5219, 0.9292, 0.9077)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.085, 0.032, 0.218, 0.003)
    b.1 <- c(2.231, 2.001, 1.293, 3.239)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.076, 0.041, 0.22, 0.051)
    b.2 <- c(0.777, 0.65, 0.44, 0.817)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.461, 0.395, 0.092, 0.088)
    b.1 <- c(1.849, 1.484, 1.774, 1.835)
    s.1 <- list(a.1, b.1)
    a.2 <- c(1.075, 0.755, 0.138, 0.22)
    b.2 <- c(0.521, 0.4224, 0.549, 0.509)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.094, 0.033, 0.101, 0.063)
    b.1 <- c(2.3, 2.177, 1.667, 2.028)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.13, 0.046, 0.116, 0.115)
    b.2 <- c(0.739, 0.693, 0.548, 0.613)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    p3 <- list(s1, s2, s3, s4, s5, s6)


    ##Hunan
    #Castanopsis fargesii
    a.1 <- c(0.4070, 1.2560, 0.0640, 0.3204)
    b.1 <- c(1.8930, 1.2030, 1.6820, 1.6618)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1810, 0.7390, 0.0310, 0.1694)
    b.2 <- c(0.7620, 0.4860, 0.6780, 0.6584)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Cupressus funebris
    a.1 <- c(0.0730, 0.0030, 0.0750, 0.0240)
    b.1 <- c(2.3160, 2.8960, 1.4180, 2.4100)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0630, 0.0030, 0.0740, 0.0220)
    b.2 <- c(0.8150, 1.0150, 0.4890, 0.8400)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Cinnamomum camphora
    a.1 <- c(0.0250, 0.0020, 0.0070, 0.0070)
    b.1 <- c(2.7300, 3.0880, 2.4470, 2.8190)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0190, 0.0020, 0.0060, 0.0050)
    b.2 <- c(0.9810, 1.1100, 0.8780, 1.0130)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Sassafras tzumu
    a.1 <- c(0.0361, 0.0261, 0.0128, 0.0819)
    b.1 <- c(2.598, 2.2236, 1.8296, 2.0898)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0325, 0.0192, 0.0139, 0.0711)
    b.2 <- c(0.9242, 0.8152, 0.634, 0.7497)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Pinus massoniana
    a.1 <- c(0.0345, 0.0719, 0.1103, 0.0079)
    b.1 <- c(2.6917, 1.9864, 1.4658, 2.6849)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0249, 0.0618, 0.1029, 0.0061)
    b.2 <- c(0.9567, 0.6955, 0.5083, 0.9465)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Schima superba
    a.1 <- c(0.0213, 0.027, 0.201, 0.0671)
    b.1 <- c(2.8347, 2.3431, 1.267, 2.2193)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0122, 0.0191, 0.1876, 0.0481)
    b.2 <- c(1.0692, 0.8705, 0.4578, 0.8253)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    #Cyclobalanopsis glauca
    a.1 <- c(0.1314, 0.0513, 0.0251, 0.1036)
    b.1 <- c(2.2118, 2.2016, 1.8056, 2.0491)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.1258, 0.6106, 0.087, 0.1142)
    b.2 <- c(0.802, 0.4547, 0.5059, 0.7268)
    s.2 <- list(a.2, b.2)
    s7 <- list(s.1, s.2)

    #Lithocarpus glaber
    a.1 <- c(0.0892, 0.4608, 0.0992, 0.1689)
    b.1 <- c(2.4531, 1.4938, 1.4945, 1.9376)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0209, 0.3953, 0.1013, 0.0638)
    b.2 <- c(1.025, 0.5294, 0.4984, 0.7901)
    s.2 <- list(a.2, b.2)
    s8 <- list(s.1, s.2)

    #Populus L.
    a.1 <- c(0.0975, 0.1656, 0.0685, 0.3952)
    b.1 <- c(2.2746, 1.6194, 1.3251, 1.5116)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0381, 0.0259, 0.0288, 0.0281)
    b.2 <- c(0.8898, 0.7621, 0.4602, 0.8095)
    s.2 <- list(a.2, b.2)
    s9 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.0356, 0.0699, 0.1338, 0.0102)
    b.1 <- c(2.6824, 1.9937, 1.4202, 2.6152)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.029, 0.06, 0.121, 0.008)
    b.2 <- c(0.975, 0.724, 0.516, 0.95)
    s.2 <- list(a.2, b.2)
    s10 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.067, 0.029, 0.058, 0.011)
    b.1 <- c(2.442, 2.324, 1.37, 2.822)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.045, 0.019, 0.046, 0.007)
    b.2 <- c(0.894, 0.851, 0.501, 1.033)
    s.2 <- list(a.2, b.2)
    s11 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.066, 0.03, 0.054, 0.011)
    b.1 <- c(2.449, 2.315, 1.456, 2.801)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.027, 0.013, 0.032, 0.004)
    b.2 <- c(0.928, 0.878, 0.552, 1.062)
    s.2 <- list(a.2, b.2)
    s12 <- list(s.1, s.2)

    p4 <- list(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)


    ##Hubei
    #Cyclobalanopsis glauca
    a.1 <- c(0.1366, 0.0069, 0.0011, 0.0434)
    b.1 <- c(2.2845, 2.9094, 3.0210, 2.4476)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0373, 0.0013, 0.0002, 0.0141)
    b.2 <- c(0.9227, 1.1776, 1.2247, 0.9883)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Quercus aliena
    a.1 <- c(0.0283, 0.0409, 0.0093, 0.0183)
    b.1 <- c(2.7720, 2.3479, 2.4441, 2.5465)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0116, 0.0190, 0.0042, 0.0081)
    b.2 <- c(1.0424, 0.8838, 0.9194, 0.9576)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Liquidambar formosana
    a.1 <- c(0.1997, 0.1537, 1.3336, 0.2450)
    b.1 <- c(2.0511, 1.6627, 0.5549, 1.6905)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0927, 0.0825, 1.0836, 0.1456)
    b.2 <- c(0.8006, 0.6490, 0.2166, 0.6435)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.197, 0.028, 0.085, 0.058)
    b.1 <- c(1.929, 2.521, 1.844, 2.119)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.118, 0.014, 0.045, 0.031)
    b.2 <- c(0.732, 0.963, 0.72, 0.812)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.112, 0.027, 0.02, 0.039)
    b.1 <- c(2.323, 2.408, 2.047, 2.413)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.046, 0.011, 0.0112, 0.013)
    b.2 <- c(0.888, 0.912, 0.7571, 0.97)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.122, 0.027, 0.024, 0.041)
    b.1 <- c(2.263, 2.428, 2.029, 2.361)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.052, 0.012, 0.0149, 0.015)
    b.2 <- c(0.868, 0.915, 0.7427, 0.917)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    p5 <- list(s1, s2, s3, s4, s5, s6)


    ##Jiangxi
    #Cunninghamia lanceolata
    a.1 <- c(0.2206, 0.0035, 0.0449, 0.1358)
    b.1 <- c(2.0335, 2.8231, 1.9430, 1.8017)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0500, 0.0002, 0.0138, 0.0362)
    b.2 <- c(0.8886, 1.3056, 0.8107, 0.7883)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Liquidambar formosana
    a.1 <- c(0.0555, 0.0400, 0.2277, 0.0371)
    b.1 <- c(2.4146, 2.2539, 1.1843, 2.1989)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0251, 0.0191, 0.1565, 0.0191)
    b.2 <- c(0.9232, 0.8616, 0.4507, 0.8321)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.176, 0.003, 0.039, 0.077)
    b.1 <- c(2.109, 2.8, 2.033, 2.039)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.083, 0.001, 0.019, 0.037)
    b.2 <- c(0.798, 1.059, 0.769, 0.771)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Broadleaf from Hunan
    a.1 <- c(0.067, 0.029, 0.058, 0.011)
    b.1 <- c(2.442, 2.324, 1.37, 2.822)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.045, 0.019, 0.046, 0.007)
    b.2 <- c(0.894, 0.851, 0.501, 1.033)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Mix from Hunan
    a.1 <- c(0.066, 0.03, 0.054, 0.011)
    b.1 <- c(2.449, 2.315, 1.456, 2.801)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.027, 0.013, 0.032, 0.004)
    b.2 <- c(0.928, 0.878, 0.552, 1.062)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    p6 <- list(s1, s2, s3, s4, s5)


    ##Anhui
    #Castanopsis sclerophylla
    a.1 <- c(0.0308, 0.2147, 0.0778, 0.0333)
    b.1 <- c(2.6487, 1.4664, 1.4334, 2.2546)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0201, 0.0410, 0.0617, 0.0281)
    b.2 <- c(0.9719, 0.6620, 0.5259, 0.8011)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Populus L.
    a.1 <- c(0.3293, 0.5433, 0.1688, 0.0666)
    b.1 <- c(1.8592, 1.2801, 1.2758, 2.0114)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0635, 0.1710, 0.0568, 0.0118)
    b.2 <- c(0.8227, 0.5689, 0.5600, 0.8848)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Lithocarpus glaber
    a.1 <- c(0.0521, 0.0011, 0.0053, 0.0260)
    b.1 <- c(2.5440, 3.5797, 2.2119, 2.4283)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0235, 0.0004, 0.0028, 0.0125)
    b.2 <- c(0.9792, 1.3585, 0.8444, 0.9308)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Conifer
    a.1 <- c(0.104, 0.025, 0.012, 0.04)
    b.1 <- c(2.218, 2.206, 2.358, 2.062)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.018, 0.013, 0.005, 0.021)
    b.2 <- c(1.089, 0.857, 0.929, 0.809)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    #Broadleaf
    a.1 <- c(0.082, 0.037, 0.018, 0.019)
    b.1 <- c(2.32, 2.223, 2.055, 2.562)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.045, 0.02, 0.01, 0.009)
    b.2 <- c(0.874, 0.839, 0.78, 0.974)
    s.2 <- list(a.2, b.2)
    s5 <- list(s.1, s.2)

    #Mix
    a.1 <- c(0.097, 0.026, 0.014, 0.03)
    b.1 <- c(2.249, 2.249, 2.242, 2.237)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.049, 0.013, 0.007, 0.014)
    b.2 <- c(0.87, 0.867, 0.86, 0.874)
    s.2 <- list(a.2, b.2)
    s6 <- list(s.1, s.2)

    p7 <- list(s1, s2, s3, s4, s5, s6)


    ##Chongqing
    #Pinus massoniana
    a.1 <- c(0.0611, 0.0136, 0.0067, 0.0069)
    b.1 <- c(2.4913, 2.5646, 2.4086, 2.7064)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0548, 0.0111, 0.0054, 0.0056)
    b.2 <- c(0.8545, 0.8909, 0.8406, 0.9395)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Lithocarpus glaber
    a.1 <- c(0.0544, 0.0320, 0.0170, 0.0287)
    b.1 <- c(2.6859, 2.3399, 2.0835, 2.5826)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0414, 0.0427, 0.0138, 0.0245)
    b.2 <- c(0.9354, 0.7462, 0.7256, 0.8858)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Quercus variabilis
    a.1 <- c(0.0554, 0.0141, 0.0120, 0.0261)
    b.1 <- c(1.7229, 1.5988, 1.6030, 1.6906)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0461, 0.0119, 0.0101, 0.0218)
    b.2 <- c(0.6109, 0.5669, 0.5684, 0.5994)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    p8 <- list(s1, s2, s3)


    ##Sichuan
    #Pinus massoniana
    a.1 <- c(0.0858, 0.0119, 0.0126, 0.0170)
    b.1 <- c(2.1890, 2.6330, 2.2875, 2.4248)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0409, 0.0049, 0.0058, 0.0075)
    b.2 <- c(0.8402, 1.0106, 0.8780, 0.9307)
    s.2 <- list(a.2, b.2)
    s1 <- list(s.1, s.2)

    #Picea asperata
    a.1 <- c(0.0347, 0.0094, 0.0063, 0.0043)
    b.1 <- c(2.5176, 2.5071, 2.4363, 2.7008)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0353, 0.0093, 0.0060, 0.0041)
    b.2 <- c(0.8942, 0.8953, 0.8760, 0.9711)
    s.2 <- list(a.2, b.2)
    s2 <- list(s.1, s.2)

    #Betula
    a.1 <- c(0.0863, 0.0185, 0.0114, 0.0281)
    b.1 <- c(2.3497, 2.3761, 1.9727, 2.2028)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0471, 0.0101, 0.0070, 0.0158)
    b.2 <- c(0.8894, 0.8980, 0.7427, 0.8347)
    s.2 <- list(a.2, b.2)
    s3 <- list(s.1, s.2)

    #Cupressus funebris
    a.1 <- c(0.1098, 0.0462, 0.0502, 0.2299)
    b.1 <- c(2.1139, 2.2721, 1.9178, 1.8478)
    s.1 <- list(a.1, b.1)
    a.2 <- c(0.0239, 0.0089, 0.0126, 0.0616)
    b.2 <- c(0.9716, 1.0449, 0.8808, 0.8471)
    s.2 <- list(a.2, b.2)
    s4 <- list(s.1, s.2)

    p9 <- list(s1, s2, s3, s4)


    valueSet <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
    return (valueSet)
}
