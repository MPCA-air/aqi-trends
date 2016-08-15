
library(readr)
library(dplyr)
library(ggplot2)
library(animation)

require(installr) 
install.ImageMagick()

aqi <- read_csv("time, aqi
                 0, NaN
                 1, 22
                 2, 33
                 3, 28
                 4, 55
                 5, 66
                 7, 58
                 8, 75
                 9, 88
                10, 99
                11, 95
                12, 108
                13, 158
                14, 260
                15, NaN")

aqi_refs <- data.frame(xstart = seq(0,250,50), 
                       xend = seq(50,300,50), 
                       col = c("green", "yellow", "orange", "red", "purple", "violet"), 
                       stringsAsFactors = F)

aqi_refs$col <- factor(aqi_refs$col, ordered = T, levels = c("green", "yellow", "orange", "red", "purple", "violet"))


aqi2 <- aqi

for(i in aqi2[!is.na(aqi2$aqi), ]$time){
  
  aqi <- filter(aqi2, time <= i)
  
  aqi_last <- filter(aqi, time < i)
  
  aqi_new <- filter(aqi, time == i)
  
for(z in 1:25) {
  
p <- ggplot() +
  geom_rect(data = aqi_refs, aes(ymin = xstart, ymax = xend, 
                                 xmin = min(aqi2$time, na.rm=T), xmax = max(aqi2$time, na.rm=T), 
                                 fill = col), alpha = 0.4) 

if(z==24) { p <- p +
  geom_point(data = aqi_new, aes(x = time, y = aqi), color = "grey50", size = 5.7) +
  geom_point(data = aqi_new, aes(x = time, y = aqi), color = "white", size = 5) 
}

if(z != 25) {
  p <- p + 
     geom_point(data = aqi_last, aes(x = time, y = aqi), color = "white", size = 5) +
     geom_line(data = aqi_last, aes(x = time, y = aqi * .996), size =1.3, color="black", alpha = 0.1) +
     geom_line(data = aqi_last, aes(x = time, y = aqi), size =1.1, color="grey50") +
     geom_point(data = aqi_last, aes(x = time, y = aqi), color = "grey50", size = 5.7) +
     geom_point(data = aqi_last, aes(x = time, y = aqi), color = "white", size = 5) 
}


if(z < 22) p <- p + geom_point(data = aqi_new, aes(x = time, y = aqi), color = "grey50", size = z**0.84, pch=21) 

if(z < 24 && z > 8) { p <- p + geom_point(data = aqi_new, aes(x = time, y = aqi), color = "grey50", size = (z-8)**0.81, pch=21) 
}

if(z < 24 && z > 16) { p <- p + 
  geom_point(data = aqi_new, aes(x = time, y = aqi), color = "grey50", size = (z-16)**0.77, pch=21) 
}

if(z==25) {
  p <- p + 
    geom_point(data = aqi, aes(x = time, y = aqi), color = "white", size = 5) +
    geom_line(data = aqi, aes(x = time, y = aqi * .996), size =1.3, color="black", alpha = 0.1) +
    geom_line(data = aqi, aes(x = time, y = aqi), size =1.1, color="grey50") +
    geom_point(data = aqi, aes(x = time, y = aqi), color = "grey50", size = 5.7) +
    geom_point(data = aqi, aes(x = time, y = aqi), color = "white", size = 5) 
}

p <- p +
  guides(fill = "none") +
  scale_fill_manual(values = as.character(aqi_refs$col)) +
  labs(x = "Time", y = "", subtitle = "Air Quality Index") +
  ylim(c(0, min(seq(150, 300, 50)[seq(150, 300, 50) >= max(aqi2$aqi, na.rm=T)]))) +
  #xlim(c(min(aqi$time, na.rm=T), max(aqi$time, na.rm=T))) +
  scale_x_continuous(breaks = aqi2$time) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

print(p)

}

  if(i == (max(aqi2$time)-1)) for(y in 1:8) {print(p)}
}

