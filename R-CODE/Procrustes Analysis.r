library(vegan)
env <- read.delim('proteinV2.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
env_pca <- rda(env, scale = TRUE)
otu <- read.delim('metaV2.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu_hel <- decostand(otu, method = 'hellinger')
otu_pca <- rda(otu_hel, scale = FALSE)
par(mfrow = c(1, 2))
biplot(env_pca, choices = c(1, 2), scaling = 1, 
    main = '环境组成的PCA', col = c('red', 'blue'))
biplot(otu_pca, choices = c(1, 2), scaling = 1, 
    main = '物种组成的PCA', col = c('red', 'blue'))
site_env <- summary(env_pca, scaling = 1)$site
site_otu <- summary(otu_pca, scaling = 1)$site
proc <- procrustes(X = env_pca, Y = otu_pca, symmetric = TRUE)
summary(proc)
plot(proc, kind = 1, type = 'text')
names(proc)
head(proc$Yrot) 
head(proc$X) 
proc$ss  
proc$rotation 
plot(proc, kind = 2)
residuals(proc) 
set.seed(123)
prot <- protest(X = env_pca, Y = otu_pca, permutations = how(nperm = 999))
prot
names(prot)
prot$signif 
prot$ss 
library(ggplot2)
Y <- cbind(data.frame(proc$Yrot), data.frame(proc$X))
X <- data.frame(proc$rotation)
group <- read.delim('group2.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
Y$samples <- rownames(Y)
Y <- merge(Y, group, by ="samples") 
p <- ggplot(Y) +
geom_point(aes(X1, X2, color = group), size = 1.5, shape = 16) +
geom_point(aes(PC1, PC2, color = group), size = 1.5, shape = 1) +
scale_color_manual(values = c("#FF8066","#845EC2"), limits = c('APE', 'BPE')
                                                                          ) +
geom_segment(aes(x = X1, y = X2, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.1, 'cm')), 
    color = 'black', size = 0.3) +
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), 
    legend.key = element_rect(fill = 'transparent')) +
labs(x = 'Dimension 1', y = 'Dimension 2', color = '') + 
geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.3) +
geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.3) + 
annotate('text', label = sprintf('M^2 == 0.8882701'),
    x = -0.21, y = 0.42, size = 3, parse = TRUE) +
annotate('text', label = sprintf('P = 0.026'),
    x = -0.21, y = 0.38, size = 3, parse = TRUE) 
p
ggsave('procrustes.pdf', p, width = 6, height = 5)
ggsave('procrustes.png', p, width = 6, height = 5)

