#' @title Tree layout with opac.point
#' @author Rintu Kutum
tree.layout <- function(n=40,r=0.2,seed=10){
  set.seed(seed)
  trg <- igraph::make_tree(n, children = 4, mode = 'undirected')
  coords <- igraph::layout_with_fr(trg)
  ncoords <- igraph::norm_coords(coords)
  plot(x=0,y=0,type = 'n',
       xlim = c(-1,1),
       ylim = c(-1,1),
       axes = FALSE,
       ann = FALSE)
  # draw lines
  mat.lines <- igraph::get.edgelist(trg)
  invisible(mapply(function(x,y){
    lines(x = c(ncoords[x,1],
                ncoords[y,1]),
          y = c(ncoords[x,2],
                ncoords[y,2]))
  },
    x = mat.lines[,1],
    y = mat.lines[,2]
  ))
  invisible(mapply(opac.point,
         x = ncoords[,1],
         y = ncoords[,2],
         r = r,
         fill = '#ff8080',
         alpha.fill = 0.4,
         stroke.col = 'blue',
         alpha = 0.5,
         stroke.size = 0.1

  ))
}
