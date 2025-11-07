draw_tangent_plane <- function(data, 
                               point, v1, v2,
                               grid.size = 16, grid.length = 5,
                               plane.col = "rgba(0,200,255,0.5)",
                               point.col = "black",
                               tangent.point.col = "red",
                               tangent.point.size = 10) {
  
  # Make grid for plane
  s <- seq(-grid.length/2, grid.length/2, length.out=grid.size)
  t <- seq(-grid.length/2, grid.length/2, length.out=grid.size)
  grid <- expand.grid(s=s, t=t)
  pts_on_plane <- t(apply(grid, 1, function(st) point + st[1]*v1 + st[2]*v2))
  xg <- matrix(pts_on_plane[,1], nrow=grid.size)
  yg <- matrix(pts_on_plane[,2], nrow=grid.size)
  zg <- matrix(pts_on_plane[,3], nrow=grid.size)
  
  plot_ly() %>%
    add_markers(x=data[,1], y=data[,2], z=data[,3], marker=list(color=point.col, size=3), name="Data") %>%
    add_surface(x=~xg, y=~yg, z=~zg, opacity=as.numeric(sub(".*,(.*)\\)", "\\1", plane.col)), 
                surfacecolor=matrix(1, nrow=nrow(xg), ncol=ncol(xg)),
                colorscale = list(c(0,1), c(plane.col, plane.col)),
                showscale=FALSE, name="Plane") %>%
    add_markers(x=point[1], y=point[2], z=point[3],
                marker=list(color=tangent.point.col, size=tangent.point.size),
                name="Tangent Point") %>%
    layout(scene=list(aspectmode="data"))
}
