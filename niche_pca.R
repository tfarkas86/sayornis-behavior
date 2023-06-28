library(devtools)
library(ggbiplot)
library(factoextra)
theme_set(theme_classic())

# import Sayornis foraging niche dataset
niche.dataset <- read.csv("../data/niche_data_coded.csv")

# extract characters from niche dataset
niche.characters <- niche.dataset[, 2:5]

#preparing variables
v <- var(niche.characters)  
diag(v)           # extracts the diagonal, i.e., the variances.

niche.characters[,2] <- log(niche.characters[,2])

v <- var(niche.characters)  
diag(v)           # extracts the diagonal, i.e., the variances.

#log.niche.characters <- (niche.characters)^1/4
#log.v <- var(log.niche.characters)  
#diag(log.v)           # extracts the diagonal, i.e., the variances.

# run PCA
z <- prcomp(niche.characters, scale=TRUE)

#biplot
pdf(file="../figures/niche-final-v2.pdf", width = 6, height = 4.5)
p <- ggbiplot(z, ellipse = TRUE, choices = c(1,2), group = niche.dataset$Species)
p <- p + xlim(-2, 4) + ylim(-2, 5)
print(p)

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

# plot individual observations
fviz_pca_ind(z, col.ind = niche.dataset$Species, addEllipses = FALSE, legend.title = "Groups", repel = TRUE)
