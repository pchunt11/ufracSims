library(GUniFrac)
source('treemaker.R')

S <- 2^10
sample1  <- data.frame(obs = sample(1:S, size=S/2, replace=TRUE),
					   sample=rep('Sample 1', S/2))
sample2  <- data.frame(obs = sample(1:S, size=S/2, replace=TRUE),
					   sample=rep('Sample 2', S/2))

samples <- rbind(sample1, sample2)

t <- table(samples)
tab <- matrix(t, ncol=ncol(t),
			  dimnames=dimnames(t))


otu_tab  <- otu_table(t(tab),
					  taxa_are_rows=FALSE)

tree  <- binary_tree(1:S,rep(1,S*(S-1)))

obj <- phyloseq(otu_tab, tree)
n.edge <- nrow(phy_tree(obj)$edge)
phy_tree(obj)$edge.length  <- phy_tree(obj)$edge.length[1:(n.edge*2)]
unifrac <- UniFrac(obj)
