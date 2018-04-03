
scale.device <- function(r){
  par_ <- par()
  x_ <- diff(par_$usr[1:2])/(par_$pin[1] * 2.54)
  y_ <- diff(par_$usr[3:4])/(par_$pin[2] * 2.54)
  return(
    list(
      x_ = r * x_,
      y_ = r * y_
    )
  )
}


get.xy <- function(
  x=1,
  y=1,
  r.x=1, # width
  r.y=1, # height
  start.angle = 0,
  end.angle =  360,
  rotation = 0,
  type = 'p'
)
{
  if(type == 'p'){
    ang <- (start.angle:end.angle)
    rad <- ang2rad(ang)
    # ref
    # https://www.youtube.com/watch?v=Xuj8gY6He5w
    mat.trans <- matrix(c(cos(ang2rad(rotation)),-sin(ang2rad(rotation)),
                          sin(ang2rad(rotation)),cos(ang2rad(rotation))),
                        ncol=2)
    org.coord <- matrix(c(r.x * cos(rad),
                          r.y * sin(rad)),
                        ncol = 2)
    new.coord <- apply(org.coord,1,function(x){x %*% mat.trans})
    coord <- list(
      x = x + new.coord[1,],
      y = y + new.coord[2,]
    )
  }else{
    if(type == 't'){
      # triangle
      ang <- seq(start.angle,
                 end.angle,
                 length.out = 4) - 30 + rotation
    }
    if(type == 'r'){
      # rectangle
      ang <- seq(start.angle,
                 end.angle,
                 length.out = 5) - 45 + rotation
    }
    rad <- ang2rad(ang)
    coord <- list(
      x = x + r.x * cos(rad),
      y = y + r.y * sin(rad)
    )
  }
  return(coord)
}

ang2rad <- function(angle){
  radian <- (angle/180)*pi
  return(radian)
}

col2alpha <- function(col = 'red', alpha = 1){
  if(0 <= alpha & alpha <= 1){
    # its ok
  }else{
    stop('alpha value should be "0 <= alpha <= 1"')
  }
  col_rgb <- col2rgb(col=col)[,1]/255
  col <- rgb(red = col_rgb[1],
             green = col_rgb[2],
             blue = col_rgb[3],
             alpha = alpha)
  return(col)
}

paired.cols <- function(){
  # RColorBrewer::brewer.pal('Paired',n = 12)
  c("#A6CEE3", "#1F78B4", "#B2DF8A",
    "#33A02C", "#FB9A99", "#E31A1C",
    "#FDBF6F", "#FF7F00", "#CAB2D6",
    "#6A3D9A", "#FFFF99", "#B15928")
}
