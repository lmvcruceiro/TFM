setwd("C:/TFM")
library(data.table)
library(igraph)
library(dplyr)

stationsData <- fread("Data/stations.csv")
stationsDataName <- stationsData %>% select(id, name, latitude, longitude)

##--Cleaning Names--##
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Puerta del Sol", replacement = "Sol")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Puerta", replacement = "Pt")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Plaza", replacement = "Pl.")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Malasaï¿½a", replacement = "Malasaña")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Martï¿½nez", replacement = "Martinez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Marquï¿½s", replacement = "Marques")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Andrï¿½s", replacement = "Andres")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Rodrï¿½guez", replacement = "Rodriguez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Espaï¿½a", replacement = "España")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Quintï¿½n", replacement = "Quintin")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Cordï¿½n", replacement = "Cordon")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Antï¿½n Martï¿½n", replacement = "Anton Martin")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Santa", replacement = "St.")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Jesï¿½s y Marï¿½a", replacement = "Jesus y Maria")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Lavapiï¿½s", replacement = "Lavapies")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Barcelï¿½", replacement = "Barcelo")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Narvï¿½ez", replacement = "Narvaez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Almadï¿½n", replacement = "Almaden")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pt del ï¿½ngel Caï¿½do", replacement = "Pt del Angel")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Menï¿½ndez", replacement = "Menendez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Niï¿½o Jesï¿½s", replacement = "Niño Jesus")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pï¿½o", replacement = "Pio")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Peï¿½alver", replacement = "Peñalver")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pardiï¿½as", replacement = "Pardiñas")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Prï¿½ncipe", replacement = "Principe")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Colï¿½n", replacement = "Colon")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Castellï¿½", replacement = "Castella")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Alcalï¿½", replacement = "Alcala")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Alcï¿½ntara", replacement = "Alcantara")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Velï¿½zquez", replacement = "Velazquez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Mï¿½rtires", replacement = "Martires")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Mï¿½ndez", replacement = "Mendez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "ï¿½lvaro", replacement = "Alvaro")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Guzmï¿½n", replacement = "Guzman")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pirï¿½mides", replacement = "Piramides")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Germï¿½n", replacement = "German")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "ï¿½ngela", replacement = "Angela")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Perï¿½n", replacement = "Peron")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pinzï¿½n", replacement = "Pinzon")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Fernï¿½ndez", replacement = "Fernandez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pacï¿½fico", replacement = "Pacifico")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Marï¿½a", replacement = "Maria")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "ï¿½lvarez", replacement = "Alvarez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Catï¿½lico", replacement = "Catolico")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Martï¿½n", replacement = "Martin")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Josï¿½", replacement = "Jose")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Agustï¿½n", replacement = "Agustin")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Repï¿½blica", replacement = "Republica")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Gutiï¿½rrez", replacement = "Gutierrez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Pï¿½", replacement = "")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Bermï¿½dez", replacement = "Bermudez")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Leï¿½n", replacement = "Leon")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Sofï¿½a", replacement = "Sofia")
stationsDataName$name <- gsub(x = stationsDataName$name, pattern = "Leï¿½n", replacement = "Leon")

biciMadData <- fread("Data/BiciMad.csv")
biciMadData <- biciMadData %>% filter(user_type != 3)
biciMadData$unplug_hourTime <-as.Date(biciMadData$unplug_hourTime)  


#################################################################
##--ALL DEGREE--##
biciMad <- biciMadData %>% filter(unplug_hourTime >= "2018-07-01")
biciMad <- left_join(x = biciMad, y = stationsDataName, by = c("idunplug_station" = "id"))

#biciMad <- biciMad %>% filter(idunplug_station <= 150, idplug_station <= 150)
biciMad <- biciMad[, -c(1, 2, 4, 6, 7, 8)]
groups <- biciMad %>% group_by(idunplug_station, idplug_station) %>% summarise(Total = n())
joinData <- left_join(biciMad, groups, by = c("idunplug_station", "idplug_station"))
gNames <-joinData %>% group_by(idunplug_station) %>% summarise(StationNames = unique(name))
biciMad <- biciMad[c(2,1)]

g <- graph_from_data_frame(biciMad, directed = TRUE)
E(g)$weight = joinData$Total
vcount(g) == length(unique(gNames))
for(i in 1:length(V(g)$name)){
  for(j in 1:length(gNames$idunplug_station)){
    if(V(g)$name[i] == as.character(gNames$idunplug_station[j])){
      V(g)$name[i] <- gNames$StationNames[j]
    }else{
      next
    }
  }
}
V(g)$name


g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = list(weight = "sum", type = "ignore"))

deg <- degree(g, mode = "all")
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights = NA)$vector

gSimple <- simplify(g, remove.multiple = T, remove.loops = T)

#calculamos as compoñentes conexas
ccs <- clusters(gSimple)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
gmax <- induced_subgraph(gSimple, vids=inodes)
is.connected(gmax)

#plot(as.undirected(gmax),layout=layout_on_sphere, vertex.size = deg/(max(deg)/20), edge.arrow.mode = 0, edge.curved = .1)

hist(E(gmax)$weight)

mean(E(gmax)$weight)

gmaxSD <- sd(E(gmax)$weight)

cut.off <- mean(E(gmax)$weight) 

g.spAll <- delete_edges(gmax, E(gmax)[E(gmax)$weight<(cut.off + gmaxSD)])
#g.spAll <- gmax

ccs <- clusters(g.spAll)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
g.spAll<- induced_subgraph(g.spAll, vids=inodes)
is.connected(g.spAll)

#l <- layout_with_fr(net)#para  fixalos nodos
#layout_on_sphere
#layout_with_fr
#layout_with_kk
#layout_in_circle
#layout_as_star
#layout_on_grid
#layout_nicely
#ll <- layout_as_star(center = "Puerta de Toledo", graph = as.undirected(g.sp))
plot(as.undirected(g.spAll),layout=layout_on_sphere, vertex.size = deg/(max(deg)/20), 
     edge.arrow.mode = 0, edge.curved = .1, edge.width=E(g.spAll)$weight/80000, vertex.color = "orange", main = "All")
cfg <- cluster_fast_greedy(as.undirected(g.spAll))
plot(cfg, as.undirected(g.spAll), main = "All")

names(which(membership(cfg) == 1))

################################################
################################################
##---IN DEGREE--##
biciMad <- biciMadData %>% filter(unplug_hourTime >= "2018-07-01")
biciMad <- left_join(x = biciMad, y = stationsDataName, by = c("idplug_station" = "id"))

#biciMad <- biciMad %>% filter(idunplug_station <= 150, idplug_station <= 150)
biciMad <- biciMad[, -c(1, 2, 4, 6, 7, 8)]
groups <- biciMad %>% group_by(idplug_station, idunplug_station) %>% summarise(Total = n())
joinData <- left_join(biciMad, groups, by = c("idplug_station", "idunplug_station"))
gNames <- joinData %>% group_by(idplug_station) %>% summarise(StationNames = unique(name))
biciMadGraph <- biciMad[c(2,1)]


head(biciMad)

g <- graph_from_data_frame(biciMadGraph, directed = TRUE)
E(g)$weight = joinData$Total
vcount(g) == length(unique(gNames))


for(i in 1:length(V(g)$name)){
  for(j in 1:length(gNames$idplug_station)){
    if(V(g)$name[i] == as.character(gNames$idplug_station[j])){
      V(g)$name[i] <- gNames$StationNames[j]
    }else{
      next
    }
  }
}
V(g)$name

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = list(weight = "sum", type = "ignore"))

deg <- degree(g, mode = "in")
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights = NA)$vector

gSimple <- simplify(g, remove.multiple = T, remove.loops = T)
#plot(as.undirected(gSimple), vertex.size = deg/(max(deg)/20) , 
#     edge.width=E(g)$weight/40, edge.arrow.mode = 0, edge.curved = .1)

#calculamos as compoñentes conexas
ccs <- clusters(gSimple)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
gmax <- induced_subgraph(gSimple, vids=inodes)
is.connected(gmax)

#plot(as.undirected(gmax),layout=layout_on_sphere, vertex.size = deg/(max(deg)/20), edge.arrow.mode = 0, edge.curved = .1)

hist(E(gmax)$weight)

mean(E(gmax)$weight)

gmaxSD <- sd(E(gmax)$weight)

cut.off <- mean(E(gmax)$weight) 

g.spIn <- delete_edges(gmax, E(gmax)[E(gmax)$weight<(cut.off + 7*gmaxSD)])
ccs <- clusters(g.spIn)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
g.spIn <- induced_subgraph(g.spIn, vids=inodes)
is.connected(g.spIn)

#l <- layout_with_fr(net)#para  fixalos nodos
#layout_on_sphere
#layout_with_fr
#layout_with_kk
#layout_in_circle
#layout_as_star
#layout_on_grid
#layout_nicely
#ll <- layout_as_star(center = "Puerta de Toledo", graph = as.undirected(g.sp))
plot(as.undirected(g.spIn),layout=layout_with_lgl, vertex.size = deg/(max(deg)/20), 
     edge.arrow.mode = 0, edge.curved = .1, edge.width=E(g.spIn)$weight/80000, vertex.color = "orange", main = "In")
cfg <- cluster_fast_greedy(as.undirected(g.spIn))
plot(cfg, as.undirected(g.spIn), main = "In")


##--OUT DEGREE--##
biciMad <- biciMadData %>% filter(unplug_hourTime >= "2018-07-01")
biciMad <- left_join(x = biciMad, y = stationsDataName, by = c("idunplug_station" = "id"))

biciMad <- biciMad %>% filter(idunplug_station <= 150, idplug_station <= 150)
biciMad <- biciMad[, -c(1, 2, 4, 6, 7, 8)]
groups <- biciMad %>% group_by(idunplug_station, idplug_station) %>% summarise(Total = n())
joinData <- left_join(biciMad, groups, by = c("idunplug_station", "idplug_station"))
gNames <- joinData %>% group_by(idunplug_station) %>% summarise(StationNames = unique(name))

biciMadGraph <- biciMad[c(2,1)]

head(biciMad)

g <- graph_from_data_frame(biciMadGraph, directed = TRUE)
E(g)$weight = joinData$Total
vcount(g) == length(unique(gNames))


for(i in 1:length(V(g)$name)){
  for(j in 1:length(gNames$idunplug_station)){
    if(V(g)$name[i] == as.character(gNames$idunplug_station[j])){
      V(g)$name[i] <- gNames$StationNames[j]
    }else{
      next
    }
  }
}
V(g)$name

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = list(weight = "sum", type = "ignore"))

deg <- degree(g, mode = "out")
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights = NA)$vector

gSimple <- simplify(g, remove.multiple = T, remove.loops = T)
#plot(as.undirected(gSimple), vertex.size = deg/(max(deg)/20) , 
#     edge.width=E(g)$weight/40, edge.arrow.mode = 0, edge.curved = .1)

#calculamos as compoñentes conexas
ccs <- clusters(gSimple)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
gmax <- induced_subgraph(gSimple, vids=inodes)
is.connected(gmax)

plot(as.undirected(gmax),layout=layout_on_sphere, vertex.size = deg/(max(deg)/20), edge.arrow.mode = 0, edge.curved = .1)

hist(E(gmax)$weight)

mean(E(gmax)$weight)

gmaxSD <- sd(E(gmax)$weight)

cut.off <- mean(E(gmax)$weight) 

g.spOut <- delete_edges(gmax, E(gmax)[E(gmax)$weight<(cut.off + 6*gmaxSD)])
ccs <- clusters(g.spOut)

imax <- which.max(ccs$csize)

#que nodos pertencen á compoñente máis grande
inodes <- which(ccs$membership == imax)

#subgrafo da maior compoñente conexa
g.spOut <- induced_subgraph(g.spOut, vids=inodes)
is.connected(g.spOut)

#l <- layout_with_fr(net)#para  fixalos nodos
#layout_on_sphere
#layout_with_fr
#layout_with_kk
#layout_in_circle
#layout_as_star
#layout_on_grid
#layout_nicely
#ll <- layout_as_star(center = "Puerta de Toledo", graph = as.undirected(g.sp))
plot(as.undirected(g.spOut),layout=layout_with_gem, vertex.size = deg/(max(deg)/20), 
     edge.arrow.mode = 0, edge.curved = .1, edge.width=E(g.spOut)$weight/80000, vertex.color = "orange", main = "Out")
cfg <- cluster_fast_greedy(as.undirected(g.spOut))
plot(cfg, as.undirected(g.spOut), main = "Out")





