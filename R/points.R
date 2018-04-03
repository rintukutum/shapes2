# opac.points can be draw after we have a place to draw
# opac.ellipse
# opac.line
# opac.quad
# opac.rect
# opac.triangle

#' @title Draw a point with opacity
#' @description Point with overlap between fill and stroke
#' @param x x coordinate
#' @param y y coordinate
#' @param r radius or size of the point
#' @param fill color for fill area
#' @param stroke.col color for stroke area
#' @param stroke.width width of stroke area
#' @param alpha.fill fill alpha
#' @param alpha.stroke stroke alpha
#' @export
#' @examples
#' set.seed(1007)
#' demo.opac.point()
#' #----
#' # or
#' #----
#' plot.new()
#' opac.point(x=0.3, y=0.1, r = 0.5)
#' opac.point(x=0.3, y=0.9, r = 0.5, alpha.fill = 0.5, alpha.stroke = 0.5)
#' #------
#' opac.point(x=0.2, y=0.4, r = 0.5)
#' opac.point(x=0.2, y=0.45, r = 0.5, alpha.fill = 0.5, alpha.stroke = 0.5)
#' #------
#' opac.point(x=0.4, y=0.45, r = 0.5, alpha.fill = 0.5, alpha.stroke = 0.5)
#' opac.point(x=0.4, y=0.4, r = 0.5)
#' #------
#' opac.point(x=0.7, y=0.1, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1)
#' opac.point(x=0.7, y=0.9, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = 'orange3')
#' #---
#' opac.point(x=0.6, y=0.45, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = 'orange3')
#' opac.point(x=0.6, y=0.4, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1)
#' #---
#' opac.point(x=0.8, y=0.4, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1)
#' opac.point(x=0.8, y=0.45, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = 'orange3')

opac.point <- function(x=0.5,
                       y=0.5,
                       r = 1,
                       fill = 'skyblue',
                       stroke.col = 'black',
                       stroke.size = 0.1,
                       alpha.fill = 1,
                       alpha.stroke = 1
                       ){
  #----
  if(length(x) != length(y)){
    stop('length of x and y are not same')
  }
  ##################
  # color components
  fill <- col2alpha(col = fill,
                    alpha = alpha.fill)
  stroke.col <- col2alpha(col = stroke.col,
                    alpha = alpha.stroke)
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
                    r.y = r_o$y_)
  ##################
  # size components
  r <- scale.device(r)
  # nor r has two components
  # x and y
  coord <- get.xy(x = x,
                  y = y,
                  r.x = r$x_,
                  r.y = r$y_)
  # draw fill area only
  graphics::polypath(x = coord$x,
                     y = coord$y,
                     border = NA,
                     col = fill)
  # draw stroke area
  graphics::polypath(x = c(coord$x, rev(coord_o$x)),
                     y = c(coord$y, rev(coord_o$y)),
                     border = NA,
                     col = stroke.col)
}

#' @title Demo on points with opacity
#' @export
demo.opac.point <-  function(){
  plot.new()
  cols <- sample(paired.cols(), 4)
  colA <- cols[1]; colB <- cols[2]; colC <- cols[3]; colD <- cols[4]

  opac.point(x=0.3, y=0.1, r = 0.5, fill = colA)
  text(x=0.3, y=0.1,labels = 'A')
  opac.point(x=0.3, y=0.9, r = 0.5, alpha.fill = 0.5, fill = colB)
  text(x=0.3, y=0.9,labels = 'B')
  #------
  opac.point(x=0.2, y=0.4, r = 0.5, fill = colA)
  opac.point(x=0.2, y=0.45, r = 0.5, alpha.fill = 0.5, fill = colB, alpha.stroke = 0.5)
  #------
  opac.point(x=0.4, y=0.45, r = 0.5, alpha.fill = 0.5, fill = colB, alpha.stroke = 0.5)
  opac.point(x=0.4, y=0.4, r = 0.5, fill = colA)
  #------
  opac.point(x=0.7, y=0.1, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1, fill = colC)
  text(x=0.7, y=0.1,labels = 'C')
  opac.point(x=0.7, y=0.9, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = colD)
  text(x=0.7, y=0.9, labels = 'D')
  #---
  opac.point(x=0.6, y=0.45, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = colD)
  opac.point(x=0.6, y=0.4, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1, fill = colC)
  #---
  opac.point(x=0.8, y=0.4, r = 0.5, alpha.fill = 0.5, alpha.stroke = 1, fill = colC)
  opac.point(x=0.8, y=0.45, r = 0.5, alpha.fill = 1, alpha.stroke = 0.5, fill = colD)
}

#' @title Demo 2 on points with opacity
#' @export
demo.opac.point2 <-  function(n){
  plot.new()
  cols <- sample(paired.cols(), n,TRUE)
  pos <- rnorm(n, mean = 0.5, sd = .2)
  r <- sample(c(0.2,0.3,0.4,0.5), n, TRUE)
  alpha.val <- plyr::ldply(lapply(1:n,function(x)sample(seq(0.5,1.0,length.out = 4),2)))
  Points <- list(x = sample(pos, n),
                 y = sample(pos, n),
                 r = r,
                 fill = cols)

  invisible(mapply(opac.point,
         x = Points$x,
         y = Points$y,
         r = Points$r,
         fill = Points$fill,
         alpha.fill = alpha.val[,1],
         stroke.col = 'white',
         alpha.stroke = alpha.val[,2]
         ))
}
