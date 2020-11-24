library(psych)
library(GPArotation)
library(factoextra)

setwd("C:/study/mba/Trimester 5/Marketing Analytics/Class 11")
factor_data = read.csv("Survey_Responses.csv", header = TRUE)

summary(factor_data)

colnames(factor_data)[1] <- "Price"
colnames(factor_data)[2] <- "Value for Money"
colnames(factor_data)[3] <- "Brand Trust"
colnames(factor_data)[4] <- "Quality"
colnames(factor_data)[5] <- "Packaging"
colnames(factor_data)[6] <- "Advertising"
colnames(factor_data)[7] <- "Claim to kill germs"
colnames(factor_data)[8] <- "Reusable mask"
colnames(factor_data)[9] <- "Offers and Discounts"
colnames(factor_data)[10] <- "Convenience of buying"
colnames(factor_data)[11] <- "Time utility"
colnames(factor_data)[12] <- "Variety"
colnames(factor_data)[13] <- "Comfort level"
colnames(factor_data)[14] <- "Product Availability"
colnames(factor_data)[15] <- "Certifications"

names(factor_data)

library(corrplot)
corrplot(cor(factor_data), method = "color")

fit <- princomp(factor_data, cor=TRUE)
screeplot(fit, type = 'line', main =  'Scree Plot')

twofactor <- fa(factor_data, nfactors = 2, rotate = "varimax", fm = "minres")
print(twofactor$loadings, cutoff = 0.3)

threefactor <- fa(factor_data, nfactors = 3, rotate = "varimax", fm = "minres")
print(threefactor$loadings, cutoff = 0.3)

fourfactor <- fa(factor_data, nfactors = 4, rotate = "varimax", fm = "minres")
print(fourfactor$loadings, cutoff = 0.3)

fivefactor <- fa(factor_data, nfactors = 5, rotate = "varimax", fm = "minres")
print(fivefactor$loadings, cutoff = 0.3)

sixfactor <- fa(factor_data, nfactors = 6, rotate = "varimax", fm = "minres")
print(sixfactor$loadings, cutoff = 0.39)



fa.diagram(fivefactor)
