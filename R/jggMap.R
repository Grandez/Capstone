require(leaflet,quietly=T, warn.conflicts=F)

eq_map <- function(dt, anot_col = "DATE") {
  if (!(anot_col %in% dt)) {
    stop(paste(anot_col, "is not a column of data"))
  }

  lat = mean(dt$LATITUDE, na.rm = T)
  lng = mean(dt$LONGITUDE, na.rm = T)


  dt$POP = dt[[anot_col]]


  leaflet(data=dt) %>% setView(lng = lng, lat = lat, zoom = 4) %>%
                       addTiles() %>%
                       addCircleMarkers(~LONGITUDE, ~LATITUDE, radius=~EQ_PRIMARY ^ 2, popup = ~DATE,
                                        label = ~POP)
}

eq_create_label <- function(a1, a2, ...) {
  message("entra")
  browser()
  p<- substitute(a1)
  print(p)
  class(a1)
  str(a2)
}
