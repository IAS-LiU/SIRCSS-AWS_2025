# Macro-level statistics
library(igraph)

matrix <- as.matrix(
  read.csv(file.choose(),
           sep="\t",
           header=TRUE,
           row.names=1,
           check.names = FALSE,
           na.strings = "")
)

# Convert matrix to igraph object.
# 'mode' either 'directed' or 'undirected'
# 'weighted' either 'NULL' (for binary) or 'TRUE' (for valued)
gdir <- graph.adjacency(matrix, mode="directed", weighted=NULL)
gundir <- graph.adjacency(matrix, mode="undirected", weighted=NULL)

plot(gdir)
plot(gundir)


#Given igraph object g

# Nbr nodes, nbr edges
nbr_actors <- vcount(g)
nbr_edges <- ecount(g)

# Network density
# nbr edges / total nbr possible edges
edge_density(gdir)

# nbr components
count_components(gundir,mode="weak")
components(gdir,mode="strong")

# diameter (directed or not)
# length of largest shortest path
get_diameter(gundir)
plot(g)

# reciprocity of graphs (directional only)
reciprocity(gdir)
dyad.census(gdir)
transitivity(gundir)

