library(maps)
library(ggplot2)
library(dplyr)
library(mapproj)
library(gsl)
  #Meteorite_Landings.csv is under the data folder in this repository. Working directory might need to be changed to where the dataset is saved.
  meteordata <- read.csv('Meteorite_Landings.csv')
  subset<-which(meteordata$mass..g.>1000)
  
  #Extract latitude and longitude values for each star
  meteorlat <- meteordata[,8]
  meteorlong <- meteordata[,9]
  meteorlatlong <- data.frame(lati = meteorlat,longi = meteorlong)
  
  #Subset for meteors weighing more than a kilogram  
  meteorlatlong1kg <- meteorlatlong[subset,]
  #Removing all NA values from lat and long
  meteorlatlong1kg <- meteorlatlong1kg[!is.na(meteorlatlong1kg$lati) & !is.na(meteorlatlong1kg$longi) & !(meteorlatlong1kg$lati == 0 & meteorlatlong1kg$longi == 0),]
  lati1kg <- meteorlatlong1kg$lati
  longi1kg <- meteorlatlong1kg$longi
  
  n<- length(meteorlatlong1kg[,1])
  
  world_coordinates <- map_data("world")
  
  #Converting to x,y,z coordinates
  meteorlatlong1kg <- mutate(meteorlatlong1kg,
                             x = cos(meteorlatlong1kg$lati*pi/180) * cos(meteorlatlong1kg$longi*pi/180),
                             y = cos(meteorlatlong1kg$lati*pi/180) * sin(meteorlatlong1kg$longi*pi/180),
                             z = sin(meteorlatlong1kg$lati*pi/180) )  
  
  #write.csv(meteorlatlong1kg, "meteor1kgXYZ.csv", row.names = FALSE)
  #pdf("Meteor_Data_Points.pdf",width = 10,height =5)
  
  #Plot of meteor landings data on the Earth's map
  plot<- ggplot(world_coordinates) +
    geom_map(map = world_coordinates,aes(map_id = region),col = "black",fill = "gray")+
    geom_point(data = meteorlatlong1kg,aes(longi, lati),alpha = 0.5,col = "red",fill = "red",shape = 21) +
    coord_map(projection = "mollweide",xlim=c(-180,180)) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          legend.title = element_blank(),
          legend.text  = element_blank(),
          legend.key   = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank()
          )
  print(plot)
  dev.off()
  
  #Making a grid to evaluate the KDE
  out <- expand.grid(lat = seq(-90, 90, by = 1), long = seq(-180, 180, by = 1))
  
  #Coordinate conversion with lat and long
  out$x <- cos(out$lat * pi / 180) * cos(out$long * pi / 180)
  out$y <- cos(out$lat * pi / 180) * sin(out$long * pi / 180)
  out$z <- sin(out$lat * pi / 180)
  
  #Creating teh grid into a matrix
  coord_pred <- as.matrix(out[, c("x", "y", "z")])
  coord_dat  <- as.matrix(meteorlatlong1kg[, c("x", "y", "z")])
  
  #New floor and ceiing functions
  NCEIL<-function(x){if(ceiling(x)==x){return(x+1)}else{return(ceiling(x))}}
  NFLOOR<-function(x){if(floor(x)==x){return(x-1)}else{return(floor(x))}}
  
  #KDE construction in R
  s<-0.5
  r<-5+NCEIL(s)
  #Truncation point
  N<-NFLOOR(((n^((s+r)/(2*s+2)))/(2*pi*(r-2)))^(1/(r-2)))+1
  h<-n^(-1/(2*s+2))
  
  #This is what we call 'g' function in Mathematica and the thesis
  k_func <- function(x) {
    1 / (1 + x^r)
  }
  
  nu_seq<-0:N
  k_terms <-((1+2*nu_seq)/(4*pi))*k_func(h * sqrt(nu_seq * (nu_seq + 1)))
  out$f_est<-array(0,dim(out)[1])
  
  #For the KDE values for plotting
  for(j in 1:length(out$x)){
    for(nu in 0:N){
      out$f_est[j]<-out$f_est[j]+
        k_terms[nu+1]*sum(legendre_Pl(nu,coord_dat%*%coord_pred[j,]))
    }
    if(j %% 100==0){print(j)}else{}
  }
  out$f_est<-out$f_est/n 
  out$f_est_rectified <- ifelse(out$f_est > 0,out$f_est,0) #positive values
  #Plot of the KDE with the colour bar
  #pdf("KDE2_Meteor_s=2.pdf",width =10,height =5)
  ggplot() +
    geom_tile(data = out, aes(x = long, y = lat, col = f_est_rectified, fill = f_est_rectified)) +
    geom_map(data = world_coordinates, map = world_coordinates,
             aes(map_id = region), col = "black", fill = NA) +
    coord_map(projection = "mollweide", xlim = c(-180, 180)) +
    scale_color_gradientn(colours = rainbow(32)[19:1], name = NULL) +
    scale_fill_gradientn(colours = rainbow(32)[19:1], name = NULL) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.key.height = unit(2,"cm")) +
    guides(fill  = guide_colourbar(nrow = 2, byrow = TRUE),
           color = guide_colourbar(nrow = 2, byrow = TRUE))
  dev.off()

