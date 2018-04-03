#' @title Draw arc with opacity
#' @export
opac.arc <- function(x = 0.5,
                     y = 0.5,
                     r = 1,
                     fill = 'red',
                     alpha.fill = 0.5,
                     stroke.size = NULL,
                     stroke.col = 'blue',
                     alpha.stroke = 0.5,
                     start.angle = 0,
                     end.angle = 90
)
{
  ##################
  # color components
  if(is.null(fill)){
  }else{
    fill <- col2alpha(col = fill,
                      alpha = alpha.fill)
  }

  if(is.null(stroke.col)){
    stroke.col <- col2alpha(col = stroke.col,
                            alpha = alpha.stroke)
  }

  ##################
  # size components
  if( is.null(stroke.size)){
    stroke.size <- 0.1 * r
  }
  ##################
  # rescale size
  r_o <- scale.device(r + stroke.size)
  ##################
  # get coordinates
  coord_o <- get.xy(x = x,
                    y = y,
                    r.x = r_o$x_,
                    r.y = r_o$y_,
                    start.angle = start.angle,
                    end.angle = end.angle)
  ##################
  # size components
  r <- scale.device(r)
  # nor r has two components
  # x and y
  coord <- get.xy(x = x,
                  y = y,
                  r.x = r$x_,
                  r.y = r$y_,
                  start.angle = start.angle,
                  end.angle = end.angle)
  # draw fill area only
  graphics::polypath(x = c(x, # origin
                           coord$x),
                     y = c(y, # origin
                           coord$y),
                     border = NA,
                     col = fill)
  # draw stroke area
  graphics::polypath(x = c(coord$x, rev(coord_o$x)),
                     y = c(coord$y, rev(coord_o$y)),
                     border = NA,
                     col = stroke.col)

}
