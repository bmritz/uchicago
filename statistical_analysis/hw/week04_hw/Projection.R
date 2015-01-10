# Script Projection.R
# Example on slide 14, Lecture 4
# Dot product and projection
u<-c(1.5,sqrt(3)/2)
v<-c(2,0)
LengthV<-sqrt(dot(v,v))
LengthU<-sqrt(dot(u,u))
# Dot product usinf fastR, definition and cos
dotProductUV<-dot(u,v)
dotProductDefinition<-sum(u*v)
dotProductCos<-LengthU*LengthV*cos(pi/6)
c(dotProductUV=dotProductUV,
  dotProductDefinition=dotProductDefinition,
  dotProductCos=dotProductCos)
# Projection using fastR, manual calculation
ProjectionUonVFastR<-project(u,v)
ProjectionUonVManual<-dotProductUV/LengthV^2*v
c(ProjectionUonVFastR=ProjectionUonVFastR,
  ProjectionUonVManual=ProjectionUonVManual)
# Projection length using fastR and dot product
ProjectionLengthFastR<-project(u,v,type='length')
ProjectionLengthDot<-sqrt(dot(ProjectionUonVFastR,
                              ProjectionUonVFastR))
c(ProjectionLengthFastR=ProjectionLengthFastR,
  ProjectionLengthDot=ProjectionLengthDot)
# Decompose U