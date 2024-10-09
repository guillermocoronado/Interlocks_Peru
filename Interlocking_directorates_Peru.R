
#Load libraries ----

library(rio)
library(tidyverse)
library(igraph)

#Load data ----

nodes1 <- import("empresas_nodos.xlsx", header = T, as.is = T)
links1 <- import("empresas_aristas.xlsx", header = T, as.is = T)

links1 <- aggregate(links1[,2], links1[,-2], sum) #We add the equal edges to form weighted links

links1 <- links1[order(links1$from, links1$to),]

colnames(links1)[3] <- "weight"

rownames(links1) <- NULL

Red1 <- graph_from_data_frame(d=links1, vertices=nodes1, directed=FALSE) #We create the igraph object

# Analysis----

## General description ----

### First plot----

bet1 <- betweenness(Red1, directed = F, normalized = T) #node size will be based on its betweeness centrality
V(Red1)$betweenness <- bet1
V(Red1)$size <- V(Red1)$betweennes*0.8

colors1 <- c("red", "green", "pink", "orange", "yellow",
             "blue", "skyblue")
V(Red1)$color <- colors1[V(Red1)$grupo] #node color will represent the economic group (Lima/foreig group or one of the six emergent groups)

E(Red1)$width <- E(Red1)$weight*0.5 #edges width will represent its weight

layout1 <- layout_with_fr(Red1) #we define the Fruchterman-Reingold layout

plot(Red1, edge.curved=0,vertex.color=V(Red1)$color, 
     vertex.frame.color="#555555",vertex.label=V(Red1)$empresa, 
     vertex.label.color="black", vertex.label.cex=.7,
     vertex.size=V(Red1)$size, edge.width=V(Red1)$width,
     layout = layout1) # We plot the network

legend(x=-1.5, y=-1.1, c("Lima/foreign origin", "Gloria Group",
                         "DC (Dyer Coriat) Group", "Flores Topitop Group",
                         "Añaños Family (AJE and ISM groups)", "Acuña Family",
                         "Huancaruna Family"), pch=21,
       col="#777777", pt.bg=colors1, pt.cex=2, cex=.8, bty="n", ncol=1) #We add the legend

### Main network statistics----

degree(Red1)
diameter(Red1)
edge_density(Red1, loops = FALSE)
components(Red1)
transitivity(Red1, type = "global")
mean_distance(Red1,weights = NULL, directed = FALSE,
  unconnected = TRUE,details = FALSE)
largest_component(Red1)

## Components and business groups----

Red2 <- delete.vertices(Red1, Isolated)

layout2 <- layout_with_fr(Red2)

plot(Red2, edge.curved=0,vertex.color=V(Red2)$color, 
     vertex.frame.color="#555555",vertex.label=V(Red2)$empresa, 
     vertex.label.color="black", vertex.label.cex=.7,
     vertex.size=V(Red2)$size, edge.width=V(Red1)$width,
     layout = layout2) # We recreate the same plot but without isolates to look better at the components

legend(x=-1.5, y=-1.1, c("Lima/foreign origin", "Gloria Group",
                         "DC (Dyer Coriat) Group", "Flores Topitop Group",
                         "Añaños Family (AJE and ISM groups)", "Acuña Family",
                         "Huancaruna Family"), pch=21,
       col="#777777", pt.bg=colors1, pt.cex=2, cex=.8, bty="n", ncol=1)



## Community detection -----

GN <- cluster_edge_betweenness (Red2) #We use the Girvan-Newman algorithm
plot(GN, Red2) #In this plot, each community is represented with the same color
membership(GN) 
  
## Density by groups------

###Density provinces-----

nodes2 <- import("empresas_nodos.xlsx", header = T, as.is = T)
links2 <- import("empresas_aristas.xlsx", header = T, as.is = T)

nodes2$origen_2 <- ifelse(nodes2$origen == "provincia", 1, 0)

links2 <- aggregate(links2[,2], links2[,-2], sum) 

links2 <- links2[order(links2$from, links2$to),]

colnames(links2)[3] <- "weight"

rownames(links2) <- NULL

Red3 <- graph_from_data_frame(d=links2, vertices=nodes2, directed=FALSE) 

Red3_1<- delete_vertices(Red3, V(Red3)$origen_2 > 0) #We delete the firms from Lima and foreign origin  

edge_density(Red3_1) #Finally, we calculate the density for this group

###Density Lima and foreign-----

Red3_2<- delete_vertices(Red3, V(Red3)$origen_2 < 1) #We now delete the firms from the provinces
edge_density(Red3_2)

##Network of business groups-----

#We now analyse a network where the nodes are not the firms but the business groups

nodes3 <- import("grupos_nodos.xlsx", header = T, as.is = T) #We load the data
links3 <- import("grupos_aristas.xlsx", header = T, as.is = T)

nodes3$origen_2 <- ifelse(nodes3$origen == "provincia", 1, 0)

links3 <- aggregate(links3[,2], links3[,-2], sum) 

links3 <- links3[order(links3$from, links3$to),]

colnames(links3)[3] <- "weight"

rownames(links3) <- NULL

Red4 <- graph_from_data_frame(d=links3, vertices=nodes3, directed=FALSE)

### Plot-----

bet2 <- betweenness(Red4, directed = F, normalized = T) #node size will be based on its betweeness centrality
V(Red4)$betweenness <- bet2
V(Red4)$size <- V(Red4)$betweennes*0.8

colors2 <- c("red", "yellow","blue")

V(Red4)$color <- colors2[V(Red4)$origen] #node color will represent the geographical origin (Lima, foreing or province)

E(Red4)$width <- E(Red4)$weight*0.5 #edges width will represent its weight

layout3 <- layout_with_fr(Red4) #we define the Fruchterman-Reingold layout

plot(Red4, edge.curved=0,vertex.color=V(Red4)$color, 
     vertex.frame.color="#555555",vertex.label=V(Red4)$grupo, 
     vertex.label.color="black", vertex.label.cex=.7,
     vertex.size=V(Red4)$size, edge.width=V(Red4)$width,
     layout = layout3) # We plot the network

legend(x=-1.5, y=-1.1, c("Foreign", "Lima", "Province"), pch=21,
       col="#777777", pt.bg=colors1, pt.cex=2, cex=.8, bty="n", ncol=1) #We add the legend

### Density by groups----

Red4_1<- delete_vertices(Red4, V(Red4)$origen_2 > 0)
Red4_2<- delete_vertices(Red4, V(Red4)$origen_2 < 1)

edge_density(Red4_1)
edge_density(Red4_2)

##Network of directors----

#We now analyse the network of directors. The goal is to detect the main brokers in the network and, after that describe their profiles

nodes4 <- import("directores_nodos.xlsx", header = T, as.is = T)
links4 <- import("directores_aristas.xlsx", header = T, as.is = T)

Red5 <- graph_from_data_frame(d=links4, vertices=nodes4, directed=FALSE)

###Plot----

bet3 <- betweenness(Red5, directed = F, normalized = T) #node size will be based on its betweeness centrality
V(Red5)$betweenness <- bet3
bet3 #This is our main interest

V(Red5)$size <- V(Red5)$betweennes*0.8

E(Red5)$width <- E(Red5)$weight*0.5

layout4 <- layout_with_fr(Red5)

plot(Red5, edge.curved=0,vertex.color=gray, 
     vertex.frame.color="#555555",vertex.label=V(Red5)$director, 
     vertex.label.color="black", vertex.label.cex=.7,
     vertex.size=V(Red5)$size, edge.width=V(Red5)$width,
     layout = layout4) 

###Main network statistics----
degree(Red5)
diameter(Red5)
edge_density(Red5, loops = FALSE)
components(Red5)
transitivity(Red5, type = "global")
mean_distance(Red5,weights = NULL, directed = FALSE,
              unconnected = TRUE,details = FALSE)
largest_component(Red5)
