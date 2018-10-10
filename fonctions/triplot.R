# Fonctionne similairement à biplot dans un plan 3d avec plot_ly
triplot_ly <- function(individus, variables)
{
  library(plotly)
  p <- dim(variables)[1]
  n <- dim(variables)[2]
  
  points <- rbind(individus[,1:3], variables[,1:3], c(0,0,0))
  pointsType = as.factor(c(rep("individus", n), rep("variable",p), "zero"))
  print(pointsType)
  theplot <- plot_ly(x = points[,1], y = points[,2], z = points[,3],
                     color = pointsType,
                     type="scatter3d", mode="markers",
                     colors = c('#BF382A', '#0C4B8E', "#00A770"))
  add_text(theplot, x = points[,1], y = points[,2], z = points[,3],
           text = c(rownames(individus),rownames(variables),"zero"))
}

# Fonctionne similairement à triplot dans un plan 3d avec plot_ly
triplot <- function(individus, variables)
{
  library(plot3D)
  p <- dim(variables)[1]
  print(p)
  print(length(variables[,1]))
  points3D(x = individus[,1], y = individus[,2], z = individus[,3])
  arrows3D(x = rep(0,p), y = rep(0,p), z = rep(0,p), 
           x1 = variables[,1], 
           y1 = variables[,2], 
           z1 = variables[,3]
           ,add=T)
  text3D(x = individus[,1],
         y = individus[,2], 
         z = individus[,3],
         labels=sapply(data[,1], as.character), adj=1, add=TRUE)
  text3D(x = variables[,1]*0.8,
         y = variables[,2]*0.8, 
         z = variables[,3]*0.8,
         col = "red",
         labels=colnames(data.quant), adj=1, add=TRUE)
}