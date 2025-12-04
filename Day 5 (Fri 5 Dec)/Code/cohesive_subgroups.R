# Little league cliques etc

# EIES and assortativity coefficient

# init igraph library
library(igraph)

# Set working directory to where your /data/ folder is
#setwd("...")

# Load example network file and store as 'matrix' object
# Here we start with loading EIES friendship nominations
# Conference with 32 scholars on network analysis
# Friendship nominations at beginning of conference: Likert 1-4, directional
# Here: dichotomized: only including friendship ties 2 and above
# Dichotomized: no reciprocity needed: one-directional assumed symmetric
# Already prepared this datafile

matrix <- as.matrix(
  read.csv("data/little_league_ti.txt",
           sep="\t",
           header=TRUE,
           row.names=1,
           check.names = FALSE,
           na.strings = "")
)
g <- graph.adjacency(matrix, mode="undirected", weighted=NULL)

coords=layout_with_fr(g)

# Let's plot this:
plot(g, layout=coords, edge.width=1.5, edge.color="black")

# Let's get all max cliques
ll_cliques <- max_cliques(g)

# How many are there?
length(ll_cliques)
# 8
# Note that this contains triads, as long as they are not part of larger cliques
# So you won't find a clique with Jeff_7, Jay_8, and Sandy_9 in there, but you
# will find a clique that contain these 3 plus Tom_2

# Identify communities, various methods
par(mfrow=c(2,2))

# Different kinds of community detection methods!
c_eb <- cluster_edge_betweenness(g)
c_l <- cluster_louvain(g)
c_fg <- cluster_fast_greedy(g)
c_opt <- cluster_optimal(g)

# With modularity as a goodness-of-fit measure for communities, slightly different
modularity(c_eb)
modularity(c_l)
modularity(c_fg)
modularity(c_opt)

# And they indeed provide different results
plot(c_eb, g, layout=coords, main="Edge betweenness", edge.width=1.5)
plot(c_l, g, layout=coords, main="Louvain", edge.width=1.5)
plot(c_fg, g, layout=coords, main="Fast greedy", edge.width=1.5)
plot(c_opt, g, layout=coords, main="Optimal", edge.width=1.5)
