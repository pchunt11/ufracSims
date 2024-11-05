source('treemaker.R')
S  <- 2^10
lengths  <- rep(1,S+(S-1))

tree <- binary_tree(1:S)#, lengths)

simulate_unifrac  <- function(tree,
							  n,
							  non_overlap,
							  dist = 'uniform',
							  gate = 0){
		species  <- tree$tip.label
		S <- length(species)

		if(dist[1] == 'uniform'){
				dist  <- rep(
							 1/S,
							 S
				)
		}

		samples  <- data.frame(
						obs = c(sample(species[1:((1-non_overlap/2)*S)],
										 n, replace=TRUE,
										 prob = dist[1:((1-non_overlap/2)*S)]
										),
								sample(species[((non_overlap/2)*S + 1):S],
										n, replace=TRUE,
										prob = dist[((non_overlap/2)*S + 1):S]
										 )),
						sample = c(rep('sample1', n),
								rep('sample2', n))
		)
		tab <- table(samples)
		mat  <- matrix(tab,
					   nrow = nrow(tab),
					   dimnames = dimnames(tab))

		#if(gate == TRUE){
				mat  <- mat*(mat >= gate*n/S)
		#}

		otu_tab  <- otu_table(mat,
							  taxa_are_rows = TRUE
		)
		#print(otu_tab)
		obj  <- phyloseq(otu_tab, tree)
		#nedge  <- nrow(phy_tree(obj)$edge)*2-2
				#ncol(phy_tree(obj)$edge)
		#phy_tree(obj)$edge.length <- rep(1,nedge)
				#phy_tree(obj)$edge.length[1:nedge]
		#plot(phy_tree(obj))
		return(UniFrac(obj))
}

#simulate_unifrac(binary_tree(1:2^10), 2^9, 0.25)
mat  <- matrix(c(1:9), nrow =2)
mat*(mat >5)
