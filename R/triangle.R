#' @title Draw triangle with opacity
#' @export
opac.triangle <- function(x = 0.5,
                          y = 0.5,
                          width = 3,
                          height = 3,
                          fill = 'red',
                          alpha.fill = 0.5,
                          stroke.size = 1,
                          stroke.col = 'blue',
                          alpha.stroke = 0.5,
                          rotate = 0

){
  ##################
  # color components
  fill <- col2alpha(col = fill,
                    alpha = alpha.fill)
  stroke.col <- col2alpha(col = stroke.col,
                          alpha = alpha.stroke)
  ######
  ######
  # for fill area
  # rescale based on width and height
  r_x <- scale.device(width*0.1)$x
  r_y <- scale.device(height*0.1)$y
  ######
  # get coordinates
  fillArea <- get.xy(
    x = x,
    y = y,
    r.x = r_x,
    r.y = r_y,
    type = 't',
    rotation = rotate
  )
  polypath(
    x = fillArea$x,
    y = fillArea$y,
    border = NA,
    col = fill
  )
  ######
  # for stroke area
  # rescale based on width and height
  r_x_outer <- scale.device(
    r = (width*0.1 +  stroke.size * 0.1)
  )$x
  r_y_outer <- scale.device(
    r = (height*0.1 +  stroke.size * 0.1)
  )$y
  ######
  # get coordinates
  strokeArea <- get.xy(
    x = x,
    y = y,
    r.x = r_x_outer,
    r.y = r_y_outer,
    type = 't',
    rotation = rotate
  )
  ######
  # draw stroke area
  polypath(
    x = c(fillArea$x, rev(strokeArea$x)),
    y = c(fillArea$y, rev(strokeArea$y)),
    border = NA,
    col = stroke.col
  )
}
