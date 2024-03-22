library(ggbiplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(MASS)
theme_set(theme_classic())

# get morph data
setwd("~/Dropbox/School_Research/Publications/2020_NTV_Sayornis/code")
morph <- read.csv("../data/morph_data.csv", header=T)

# average each of the measurements within individuals 
morph <- transform(morph, bill_width = rowMeans(morph[,7:9]))
morph <- transform(morph, bill_depth = rowMeans(morph[,10:12]))
morph <- transform(morph, bill_length = rowMeans(morph[,13:15]))
morph <- transform(morph, tarsus = rowMeans(morph[,16:18]))
morph <- transform(morph, primary_proj = rowMeans(morph[,19:22]))
morph <- transform(morph, wing_chord = rowMeans(morph[,22:24]))
morph <- transform(morph, S1_length = rowMeans(morph[,25:27]))

# calculate HWI
#morph <- transform(morph, hand_wing = ((wing_chord - S1_length)/wing_chord)*100)

# get new dataframe
keep <- c("species","weight","bill_width","bill_depth","bill_length","tarsus","wing_chord")
morph <- morph[,(names(morph) %in% keep)]
View(morph)
write.csv(morph, "../data/morph_data_averaged.csv" )

#preparing variables
v <- var(morph)  
diag(v)           # extracts the diagonal, i.e., the variances.

log.morph <- log(morph[ ,2:7])
log.v <- var(log.morph)  
diag(log.v)           # extracts the diagonal, i.e., the variances.

#Principal components analysis
z <- prcomp(log.morph, scale = TRUE)      # shortcut, indicating columns

#biplot
ggbiplot(z, ellipse = TRUE, choices = c(1,2), obs.scale = 1, var.scale = 1, groups = morph$species)
pdf(file="morpho-2.pdf", width = 6, height = 4.5)
dev.off()

#eigvalues/variances
get_eigenvalue(z)
fviz_eig(z, addlabels = TRUE, ylim = c(0, 50))

# Contributions of variables to PC1
fviz_contrib(z, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(z, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(z, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(z, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(z, choice = "var", axes = 5, top = 10)
# Contributions of variables to PC6
fviz_contrib(z, choice = "var", axes = 6, top = 10)

#BILL ONLY 
# get new dataframe
keep_BILL <- c("species","bill_width","bill_depth","bill_length")
morph_BILL <- morph[,(names(morph) %in% keep_BILL)]
View(morph_BILL)
#preparing variables
v_BILL <- var(morph_BILL)  
diag(v_BILL)           # extracts the diagonal, i.e., the variances.

log.morph_BILL <- log(morph_BILL[ ,2:4])
log.v_BILL<- var(log.morph_BILL)  
diag(log.v_BILL)           # extracts the diagonal, i.e., the variances.

#Principal components analysis
z_BILL <- prcomp(log.morph_BILL, scale = TRUE)      # shortcut, indicating columns

#biplot
ggbiplot(z_BILL, ellipse = TRUE, choices = c(1,2), obs.scale = 1, var.scale = 1, groups = morph_BILL$species)
pdf(file="morpho-bill-only.pdf", width = 6, height = 4.5)
dev.off()

#eigvalues/variances
get_eigenvalue(z)
fviz_eig(z, addlabels = TRUE, ylim = c(0, 50))

# Contributions of variables to PC1
fviz_contrib(z, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(z, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(z, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(z, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(z, choice = "var", axes = 5, top = 10)
# Contributions of variables to PC6
fviz_contrib(z, choice = "var", axes = 6, top = 10)



