a1 <- read.csv("game.data.csv", sep = ",", header = TRUE)
View(a1)
library(dplyr)
a1 <- dplyr::select(a1, -id)
plot(a1)
library(corrplot)
cor <- cor(a1)
corrplot(cor, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
a2 <- dplyr::select(a1, -score, -levels)
a2 <- scale(a2, center = TRUE)
pca <- prcomp(a2, scale = TRUE)
plot(pca, type = "lines")
a3 <- as.data.frame(pca$x)
a4 <- cbind(a3, as.data.frame(a1$score))
pca$rotation
loadings <- abs(pca$rotation)
sweep(loadings, 2, colSums(loadings), "/")
names(a4) <- c("affinity", "adorability", "badcharm", "tensity", "score")
cor2 <- cor(a4)
corrplot(cor2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
#Charles reminded me that I need to make sure that new PCs have linear correlation before calculating their correlation coefficients
#So I added a plot below.
plot(a4)
#According to the plot, however, they are not linealy corralated, so the coclusion I made in class is not valid.