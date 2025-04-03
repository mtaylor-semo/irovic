# calculate slope based on duck scores

vars1.2


width.x <- vars1.2$NMDS1[1]
width.y <- vars1.2$NMDS2[1]

hatched.x <- vars1.2$NMDS1[2]
hatched.y <- vars1.2$NMDS2[2]

slope_width <- (width.y - 0)/(width.x - 0)
slope_hatched <- (0 - hatched.y)/(0 - hatched.x)


fred <- pi - atan(slope_hatched) - atan(slope_width)

(fred * 180)/pi

atan((slope_hatched - slope_width)/(1 + (slope_hatched * slope_width)))


atan(slope_width)
atan(slope_hatched)

180- abs(atan(slope_width) - atan(-slope_hatched))





