# Rewiring

# init igraph library
library(igraph)

# Set working directory
setwd("C:/Users/pekpi/Dropbox/work/dropboxR/WinterCSS")


# Load example network file and store as 'matrix' object
# 'file.choose()' makes it easier to pick file, but can also specify file path here
matrix <- as.matrix(
  read.csv("data/little_league_ti.txt",
           sep="\t",
           header=TRUE,
           row.names=1,
           check.names = FALSE,
           na.strings = "")
)

# Convert matrix to igraph object.
# 'mode' either 'directed' or 'undirected'
# 'weighted' either 'NULL' (for binary) or 'TRUE' (for valued)
g <- graph.adjacency(matrix, mode="directed", weighted=NULL)

# Create a layout using Fruchterman-Reingold
coords=layout_with_fr(g)

# We visualize our graph using these coordinates
plot(g,layout=coords,edge.width=2)
degree(g)


# Now let's rewire - assume that individuals have other friends than what reported
# while keeping the number of friends that each has
g2 <- rewire(g,with = keeping_degseq(loops=FALSE,niter = 500))
plot(g2,layout=coords,edge.width=2)
degree(g2)

# A new network, yet same number of friends

# Rewiring thus an option for creating a plausible counterfactual social network
# (as long as theoretically plausible)