source('treemaker.R')
source('find_alpha.R')
S  <- 2^10
lengths  <- rep(1,S+(S-1))

tree <- binary_tree(1:S)

simulate_unifrac  <- function(tree,
							  n_sam,
							  non_overlap,
							  n_loc = 2,
							  n_treat = 1,
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
										 n_sam, replace=TRUE,
										 prob = dist[1:((1-non_overlap/2)*S)]
										),
								sample(species[((non_overlap/2)*S + 1):S],
										n_sam, replace=TRUE,
										prob = dist[((non_overlap/2)*S + 1):S]
										 )),
						sample = c(rep('sample1', n_sam),
								rep('sample2', n_sam))
		)

		tab <- table(samples)

		mat  <- matrix(tab,
					   nrow = nrow(tab),
					   dimnames = dimnames(tab))

		mat.g  <- rm_obs(mat, gate=gate)

		#print(mat.g)
		otu_tab  <- otu_table(mat.g,
							  taxa_are_rows = TRUE
		)
		#print(otu_tab)
		obj  <- phyloseq(otu_tab, tree)
		Uni  <- UniFrac(obj)
        #print(obj)
		return(Uni)
}

simulate_unifrac(binary_tree(1:2^10), 5.25*2^10, 0)
