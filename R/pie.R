#' @title Draw pie with opacity
#' @export
opac.pie <- function(x = 0.5,
                     y = 0.5,
                     r = 1,
                     slices = c(10, 12, 4, 16, 8),
                     labels = c("US", "UK", "Australia", "Germany", "France"),
                     fill = NULL,
                     alpha.fill = 0.5,
                     stroke.size = 0.2,
                     stroke.col = NULL,
                     alpha.stroke = 1

)
{
  if(length(slices) == 1){
    if(is.null(fill)){
      fill <- 'skyblue'
    }
    if(is.null(stroke.col)){
      stroke.col <- 'blue'
    }
    opac.point(
      x = x,
      y = y,
      r = r,
      fill = fill,
      alpha.fill = alpha.fill,
      stroke.size = stroke.size,
      stroke.col = stroke.col,
      alpha.stroke = alpha.stroke)
  }else{
    if(is.null(fill)){
      fill <- sample(colors(),
                     length(slices))
    }
    if(is.null(stroke.col)){
      stroke.col <- fill
    }
    ##################
    # color components
    fill <- as.character(
      mapply(
        col2alpha,
        col = fill,
        alpha = alpha.fill
        )
      )
    stroke.col <- as.character(
      mapply(
        FUN = col2alpha,
        col = stroke.col,
        alpha = alpha.stroke
        )
      )
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
    o.x <- x
    o.y <- y
    coord_o <- get.xy(
      x = x,
      y = y,
      r.x = r_o$x_,
      r.y = r_o$y_,
      start.angle = 0,
      end.angle = 360)
    ##################
    # size components
    r <- scale.device(r)
    # nor r has two components
    # x and y
    coord_in <- get.xy(
      x = x,
      y = y,
      r.x = r$x_,
      r.y = r$y_,
      start.angle = 0,
      end.angle = 360)
    ################
    frac <- prop.table(slices)
    names(frac) <- labels
    props <-seq(
      0,
      1,
      length.out = 361)
    bins <- findInterval(
      props,
      cumsum(frac)
    )
    bins[361] <- bins[360]
    bins_ox <- split(coord_o$x, as.factor(bins))
    bins_oy <- split(coord_o$y, as.factor(bins))
    bins_ix <- split(coord_in$x, as.factor(bins))
    bins_iy <- split(coord_in$y, as.factor(bins))
    #---
    # fill area
    bins_fill <- list()
    for(i in seq_along(bins_ix)){
      if(i == 1){
        bins_fill[[i]] <- list(
          x = c(o.x,
                bins_ix[[i]]
                ),
          y = c(o.y,
                bins_iy[[i]]
                ),
          fill = fill[i]
          )
      }else{
        bins_fill[[i]] <- list(
          x = c(o.x,
                rev(bins_ix[[i-1]])[1],
                bins_ix[[i]]
                ),
          y = c(o.y,
                rev(bins_iy[[i-1]])[1],
                bins_iy[[i]]
                ),
          fill = fill[i]
          )
      }
    }
    invisible(lapply(bins_fill,function(x){
      polypath(x = x$x,
               y = x$y,
               border = NA,
               col = x$fill)
    }))
    #---
    # stroke area
    bins_stroke <- list()
    for(i in seq_along(bins_ix)){
      if(i == 1){
        bins_stroke[[i]] <- list(
          x = c(bins_ix[[i]],
                rev(bins_ox[[i]])
                ),
          y = c(bins_iy[[i]],
                rev(bins_oy[[i]])
                ),
          fill = stroke.col[i]
        )
      }else{
        bins_stroke[[i]] <- list(
          x = c(
            # in
            # previous points' start
            rev(bins_ix[[i-1]])[1],
            # current point
            bins_ix[[i]],
            # out
            # current point
            rev(bins_ox[[i]]),
            # previous point
            rev(bins_ox[[i-1]])[1]
            ),
          y = c(
            # in
            rev(bins_iy[[i-1]])[1],
            bins_iy[[i]],
            # out
            rev(bins_oy[[i]]),
            rev(bins_oy[[i-1]])[1]
           ),
          fill = stroke.col[i]
          )
      }
    }
    invisible(lapply(bins_stroke,function(x){
      polypath(
        x = x$x,
        y = x$y,
        border = NA,
        col = x$fill)
    }))
  }
}
