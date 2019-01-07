library(leaflet)


##-- Cluster 1 --##
solA <- c(40.4169473, -3.7035284999999476)
jacintoBenavente <- c(40.4143301, -3.7032868000000008)
palacioDeOriente <- c(40.417955, -3.7143120000000636)
plazaSanFrancisco <- c(40.41074750000001, -3.714488299999971)
plazaDeLaProvincia <- c(40.414799, -3.7060954999999467)
segovia45 <- c(40.4136841, -3.717536300000006)


##- Cluster 2 --##
jesusYMaria <- c(40.4112154, -3.703640500000006)
santaMariaDeLaCabeza <- c(40.3957595, -3.7041702000000214)
palosDeLaFrontera <- c(40.4039957, -3.699952899999971)
plazaDeLavapies <- c(40.4088039, -3.7012909000000036)


##-- Cluster 3 --##
puertaDeGranada <- c(40.4122218, -3.676975900000002)
puertaDoceOctubre <- c(40.3791146, -3.6986971999999696)
hospitalDoceOctubre <- c(40.3762126, -3.6986884000000373)
atocha <- c(40.4051583, -3.6885069999999587)
plazaFelipeII <- c(40.4239222, -3.6741997999999967)


##-- Cluster 4 --##
metroPiramidesMadrid <- c(40.4025129, -3.7113828999999896)
plazaDeLaCebada <- c(40.4107817, -3.7096773000000667)
embajadores <- c(40.4081286, -3.707738699999936)
puertaDeToledo <- c(40.4069778, -3.7114971999999398)
entradaMatadero <- c(40.3928787, -3.69932689999996)
paseoDeLaFlorida <- c(40.4226321, -3.72275680000007)

m <- leaflet()%>%
  addTiles()%>%
  setView(lng = -3.68, lat = 40.43, zoom = 12.5) %>%
  addPolygons(lat = c(
                puertaDeGranada[1], puertaDoceOctubre[1], hospitalDoceOctubre[1], atocha[1], plazaFelipeII[1]),
              lng = c(
                puertaDeGranada[2], puertaDoceOctubre[2], hospitalDoceOctubre[2], atocha[2], plazaFelipeII[2]),
                weight = 2, fillColor = T, color = "aqua"
              )%>%
  addPolygons(lat = c(
    jesusYMaria[1], santaMariaDeLaCabeza[1], palosDeLaFrontera[1], plazaDeLavapies[1]
  ), lng = c(
    jesusYMaria[2], santaMariaDeLaCabeza[2], palosDeLaFrontera[2], plazaDeLavapies[2]
  ),weight = 2, fillColor = T, color = "green"
  )%>%
  addPolygons(lat = c(
    solA[1], jacintoBenavente[1], plazaDeLaProvincia[1], plazaSanFrancisco[1], segovia45[1], palacioDeOriente[1]
  ), lng = c(
    solA[2], jacintoBenavente[2], plazaDeLaProvincia[2], plazaSanFrancisco[2], segovia45[2], palacioDeOriente[2]
  ),weight = 2, fillColor = T, color = "red"
  )%>%
  addPolygons(lat = c(
    paseoDeLaFlorida[1], plazaDeLaCebada[1],  embajadores[1],  entradaMatadero[1], metroPiramidesMadrid[1], puertaDeToledo[1]
  ), lng = c(
    paseoDeLaFlorida[2], plazaDeLaCebada[2], embajadores[2],  entradaMatadero[2], metroPiramidesMadrid[2], puertaDeToledo[2]
  ),weight = 2, fillColor = T, color = "violet"
  )
m 
