#Correlational circus map


# circus graph:
#http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html

#file format:
##edges=source_target:
#source|target
#g__lactobaillus|bCD4

##edges=source_target:
#e|nodeID
#g__lactobaillus|1
#bCD4|2
#install.packages("igraph")

library(igraph)
edges_file<-choose.files(caption = "Select edges (source_target)")
nodes_file<-choose.files(caption = "Select nodes (nodeID)")
edges <- read.csv(edges_file)
nodes <- read.csv(nodes_file)
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
V(g)$size <- log(strength(g)) * 4+ 3
#plot graph:
plot(g, layout=layout_in_circle, main="Circle")
#change text size:
V(g)$label.cex <- 0.4
plot(g, layout=layout_in_circle, main="Circle")



