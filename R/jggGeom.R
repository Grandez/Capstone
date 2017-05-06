require(ggplot2)
require(grid)
require(RColorBrewer)

# Terremoto:
#   Sitio
#   Fecha        -> X
#   Intensidad
#   Muertos

#' export
geom_timeline  <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = TRUE,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,

    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' export
geom_timeline_label  <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = TRUE,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,

    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomTimeline <- ggproto("GeomTimeline", GeomPoint,
                     required_aes = c("x"),
                     non_missing_aes = c("size", "shape", "colour"),
                     default_aes = aes(y=0,
                       shape = 21, colour = "gray", fill=NA, size = 1.5,
                       alpha = NA, stroke = 0.5
                     ),
                     draw_key = draw_key_point,
                     draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                       coords <- coord$transform(data, panel_params)
                              c1 <- pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = gpar(
                                  col = alpha(coords$colour, coords$alpha),
                                  fill = alpha(coords$fill, coords$alpha),
                                  # Stroke is added around the outside of the point
                                  fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                  lwd = coords$stroke * .stroke / 2
                                )
                              )

                               c2 <- linesGrob(
                                 y=unit(c(coords$y, coords$y),"npc"),
                                 gp = gpar(col="black", lwd = 2)
                               )
                        grobTree(c2, c1)
                     }
)


GeomTimelineLabel <- ggproto("GeomTimelineLabel", GeomPoint,
                             required_aes = c("x"),
                             non_missing_aes = c("size", "shape", "colour"),
                             default_aes = aes(y=0,
                                               shape = 21, colour = "gray", fill=NA, size = 1.5,
                                               alpha = NA, stroke = 0.5
                             ),
                             draw_key = draw_key_point,
                             draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                               coords <- coord$transform(data, panel_params)
                               c1 <- pointsGrob(
                                 coords$x, coords$y,
                                 pch = coords$shape,
                                 gp = gpar(
                                   col = alpha(coords$colour, coords$alpha),
                                   fill = alpha(coords$fill, coords$alpha),
                                   # Stroke is added around the outside of the point
                                   fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                   lwd = coords$stroke * .stroke / 2
                                 )
                               )

                               c2 <- linesGrob(
                                 y=unit(c(coords$y, coords$y),"npc"),
                                 gp = gpar(col="black", lwd = 2)
                               )
                               c3 <- linesGrob(
                                 x=unit(c(coords$x, coords$x),"npc"),
                                 y=unit(c(coords$y, coords$y + 0.5),"npc"),
                                 gp = gpar(col="black", lwd = 2)
                               )

                               grobTree(c2, c1, c3)
                             }
)
