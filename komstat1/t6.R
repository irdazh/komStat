
# Love, Lang. -------------------------------------------------------------

t = seq(0, 2*pi, le=100)
x	=	16*sin(t)^3	
y	=	13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)

plot(c(-20,20), c(-20,20), type="n", xlab="", ylab="")
polygon(x, y, col="red")
lines(c(14,15,12,15,-11,-11.5,-15,-11.5,-12.5), 
      c(12,15,14,15,-11,-11.5,-12.5,-11.5,-15), 
      col="blue", lwd=8)

polygon(x[1:50], y[1:50], col="red", border="red")
lines(x[1:50], y[1:50])

polygon(x=4*sin(t), y=4*cos(t),col="red", border="red")



# Uhmmmmmmmmm -------------------------------------------------------------
library(rgl)

options(rgl.printRglwidget = TRUE)
lx = ly = seq(-3/2, 3/2, le=1000)
lz = rep(c(-1.2,1.2), 500)

df = data.frame(x1=lx)

fZ = function(lx, ly, lz){

for (j in 1:1000){
  k = lz*(9/80*ly^2+lx^2)^(1/3)
  
  # for x1
  m = k - (lx^2+9/4*ly^2-1)
  lz = ifelse(lz>0, -sqrt(m), sqrt(m))
  
  m = k - (lz^2+9/4*ly^2-1)
  lx = ifelse(lx>0, -sqrt(m), sqrt(m))
  
  m = k*4/9 - (lz^2+lx^2-1)*4/9
  ly = ifelse(ly>0, -sqrt(m), sqrt(m))
}
}

fZ = function(lx, ly){
  
    k = lz*(9/80*ly^2+lx^2)^(1/3)
    
    # for x1
    m = k - (lx^2+9/4*ly^2-1)
    lz = ifelse(lz>0, -sqrt(m), sqrt(m))
  }

summary(lz)

plot(lx[1:100],lz[1:100])
hist(lz)
?outer

zz = outer(lx, ly, fZ)

open3d()
surface3d(lx,ly,zz, col="blue", 
          zlim=c(0,0), ylim=c(0,1), xlim=c(0,1),
          front="l", back="l")

?persp3d()


plot3d(lx, ly, lz, type="s")



# #2 ----------------------------------------------------------------------

x = y = seq(-0.7,0.7,le=100)
lz1 = function(x,y){5+(-sqrt(1-x^2-(y-abs(x)^2))*
                         cos(30*(1-x^2-(y-abs(x))^2)))}

z = outer(x,y,lz1)

open3d()
surface3d(x,y,z, col="blue", 
          zlim=c(0,0), ylim=c(0,1), xlim=c(0,1),
          front="l", back="l")
plot3d(x, y, lz1(x,y))



# Hella kind --------------------------------------------------------------
library(rgl)
library(misc3d)
options(rgl.printRglwidget = TRUE)
open3d()

fxx = function(u, v){
  sin(v)*(15*sin(u)-4*sin(3*u))
}

fyy = function(u,v){
  fy = 8*cos(v)
}

fzz = function(u,v){
  fz = sin(v)*(15*cos(u)-5*cos(2*u)-
                 2*cos(3*u)-cos(2*u))
}

open3d()

parametric3d(fx = fxx,
             fy = fyy,
             fz = fzz,
             u = seq(0,2*pi, le=30),
             v = seq(0, pi, le=30),
             color="blue",
             alpha=.6)

parametric3d(fx = fxx,
             fy = fyy,
             fz = fzz,
             u = seq(0,2*pi, le=30),
             v = seq(0, pi, le=30),
             color="blue", fill=F,
             lwd=1,
             col.mesh="red", add=T)
             
             
             
?parametric3d()










#

# batas suci --------------------------------------------------------------

library(iplots)

df1 = scan(text="
159    176    168    171    174    172    180    176    164    170    169    183    155    159    177    181    179    163    165    167
81    76    60    55    68    73    84    91    75    87    57 56    59    78    88    86    83    53    63    82")

label = c(rep("TB", 20), rep("BB",20))
BB = df1[label=="BB"]
TB = df1[label=="TB"]

# plot 1
ibox(df1,label)
# plot 2
ihist(TB)
# plot 3
iplot(x=TB, y=BB,xlab = "Tinggi Badan", 
      ylab = "Berat Badan")
iabline(lm(BB~TB))


# rggobi ------------------------------------------------------------------
library(rggobi)

#open 
ggobi("data/t6.csv")

# coloring
gdata = ggobi_get()$t6.csv
glyph_size(gdata) = 5
glyph_color(gdata) <- gdata$Bentuk
glyph_type(gdata) <- c(4,3)[gdata$Bentuk]

# interactive

gdata[, 5:6]
