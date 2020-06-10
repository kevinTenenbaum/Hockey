
circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(x = center[1] + diameter / 2 * cos(tt), 
             y = center[2] + diameter / 2 * sin(tt))
}


plotRink <- function(){
  # Rink dimensions described here: https://www.usahockeyrulebook.com/page/show/1082185-rule-104-face-off-spots-and-face-off-circles
  crease <- data.frame(x = c(-89, -89, 89, 89),
                       xend = c(-84.5, -84.5, 84.5, 84.5),
                       y = c(-4, 4, -4,4),
                       yend = c(-4, 4, -4, 4))
  
  
  
  ggplot() + theme_classic() + xlim(-100, 100) + ylim(-42.5, 42.5) + coord_equal() +
    geom_vline(xintercept = 0, colour = 'red', lwd = 1) + 
    geom_vline(xintercept = c(-25, 25), colour = 'blue', lwd = 1) +
    geom_vline(xintercept = c(-89, 89), colour = 'red', lwd = 1) +
    geom_polygon(data = circleFun(diameter = 30), aes(x = x, y = y), colour = 'blue', fill = NA) + 
    geom_polygon(data = circleFun(diameter = 1), aes(x = x, y = y), colour = 'red', fill = 'red') + 
    geom_segment(data = crease, aes(x = x, y= y, xend = xend, yend = yend)) + 
    geom_polygon(data = circleFun(center = c(-89, 0), diameter = 12, start = 1.75, end = 2.25), aes(x = x, y = y), colour = 'red', fill = NA) + 
    geom_polygon(data = circleFun(center = c(89, 0), diameter = 12, start = .75, end = 1.25), aes(x = x, y = y), colour = 'red', fill = NA) +
    geom_polygon(data = circleFun(center = c(-69, -22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, -22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, 22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, 22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    
    geom_polygon(data = circleFun(center = c(-20, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(20, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    geom_polygon(data = circleFun(center = c(-20, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(20, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') 
  
}
