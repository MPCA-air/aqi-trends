
library(readr)
library(dplyr)
library(ggplot2)
library(httr)
#library(animation)
library(magick)

#require(installr) 
#install.ImageMagick()

#devtools::install_github("hadley/ggplot2")

setwd("C:/Users/dkvale/Desktop/State Fair 2016/Color chart")

# Load past AQI values

aqi_test <- read_csv("row, time, time_txt, aqi
                 1,0,0am, NA
                 2,23,11pm, 22
                 3,2,2am, 33
                 4,3,3am, 28
                 5,4,4am, 55
                 6,5,5am, 66
                 7,6,6am, 58
                 8,7,7am, 75
                 9,8,8am, 88
                 10,9,9am, 99
                11,10,10am, 95
                12,11,11am, 108
                13,12,12pm, 118
                14,13,1pm, NA")

aqi_old <- read_csv("aqi_old.csv")

aqi_old$aqi  <- as.integer(aqi_old$aqi)
aqi_old$time <- as.integer(aqi_old$time)

# Request data from SmogWatch
#curl('curl -X POST -H "Content-Type: application/x-www-form-urlencoded" -H "Cache-Control: no-cache" -H "Postman-Token: 4c949d2b-2fc4-33a1-5a4f-3006bba29323" -d 'TheDateID=1&theDate=07/15/2016&ParamID=2&Unit=ug/m3' 
#     "http://www.smogwatch.com/minn/aqdata.cfm"')

#url <- "http://www.smogwatch.com/minn/aqdata.cfm"
#body <- list(TheDateID=1, theDate='07/15/2016', ParamID=2, Unit='ug/m3')

# Form encoded
#r <- POST(url, body = body, encode = "form")
#aqi <- content(r, as="text")


aqi_new <- readLines("https://mpca.sonomatechdata.com/reportingarea/currentConditions")

new_time <- grepl("Last Updated", aqi_new) %>%
           aqi_new[.] %>%
           strsplit(. , " ") %>% 
           unlist(.) %>% 
           .[grepl("PM|AM", .)]

new_time_24 <- as.integer(format(strptime(new_time, "%I:%M %p"), "%H"))

if(new_time_24 > max(aqi_old[!is.na(aqi_old$aqi), ]$time)) {

  twin_cities <- grep("Twin", aqi_new)
  
  new_value <-  grep("cc_ozone_cell", aqi_new) %>%
                  .[. > twin_cities] %>%
                  aqi_new[.] %>%
                  strsplit(. , ">") %>% 
                  unlist(.) %>% 
                  .[2] %>%
                  gsub("<[///]div", "", .)
  
  # Quality check
  
  ## Set negative values to one
  if(new_value < 1) new_value <- 1
  
  ## Set >499 values to 499
  if(new_value > 499) new_value <- 499
  
  
  # Create table for new data
  aqi_new <- data_frame(time      = new_time_24, 
                        time_txt  = tolower(gsub(":00", "", new_time)),
                        aqi       = new_value)
  
  aqi_new$aqi  <- as.integer(aqi_new$aqi)
  aqi_new$time <- as.integer(aqi_new$time)
  
  aqi_new <- rbind(aqi_old[-c(1, nrow(aqi_old)), ], aqi_new)
  
  aqi_new[1, ]$aqi <- NA
  
  aqi_new <- rbind(aqi_new, 
                   data_frame(time = as.integer(format(strptime(new_time, "%I:%M %p")+60*60, "%H")),
                              time_txt = "",
                              aqi = NA))
  
  write_csv(aqi_new, "aqi_old.csv")
}
  
# Create background colors
aqi_refs <- data.frame(xstart = c(seq(0,150,50), 200, 300),
                       xend = c(seq(50,200,50), 300, 500),
                       col = c("#53BF33", "#F4C60B", "#DB6B1A", "#c81d25", "#52154E", "#4c061d"), 
                       stringsAsFactors = F)

aqi_refs$col <- factor(aqi_refs$col, ordered = T, levels = aqi_refs$col)


aqi2 <- aqi_new

setwd("charts")

img_count <- 0

par(mar=c(0,0,0,0))


for(i in 2:13){
  
  aqi <- aqi2[2:i, ]
  
  aqi_last <- aqi[!is.na(aqi$aqi), ][2:(i-1), ]
  
  aqi_new <- aqi[i, ]
  
for(z in seq(1, 37, 2)) {
  
  img_count <- img_count + 1
  
p <- ggplot() +
  geom_rect(data = aqi_refs, aes(ymin = xstart, ymax = xend, 
                                 xmin = min(aqi2$time, na.rm=T), xmax = max(aqi2$time, na.rm=T), 
                                 fill = col), alpha = 0.74) 

# Background line
p <- p + 
  geom_line(data = aqi2[!is.na(aqi2$aqi), ], aes(x = row, y = aqi), size =1.1, color="grey40", alpha = 0.08) +
  #geom_line(data = aqi2[!is.na(aqi2$aqi), ], aes(x = time, y = aqi), size =1, color="grey50", alpha = 0.07) +
  #geom_point(data = aqi2, aes(x = time, y = aqi), color = "grey50", size = 5.7,, alpha = 0.09) +
  geom_point(data = aqi2[!is.na(aqi2$aqi), ], aes(x = row, y = aqi), color = "grey40",  size = 4, alpha = 0.06)



# Connecting lines
if(z < 25 && nrow(aqi) > 1) {
  p <- p + 
       geom_line(data = aqi_last, aes(x = row, y = aqi * .996), size =1.1, color="grey20", alpha = 0.15) +
       geom_line(data = aqi_last, aes(x = row, y = aqi), size =1, color="grey40", alpha = 0.65)
}  

if(z >= 25 && nrow(aqi) > 1) {
    p <- p + 
      geom_line(data = aqi, aes(x = row, y = aqi * .996), size =1.1, color="grey20", alpha = 0.15) +
      geom_line(data = aqi, aes(x = row, y = aqi), size =1, color="grey40", alpha = 0.65) 
}  
  

# Previous points
p <- p + 
    geom_point(data = aqi_last, aes(x = row, y = aqi), color = "grey50", size = 4.5) +
    geom_point(data = aqi_last, aes(x = row, y = aqi), color = "white", size = 4)

# New point
if(z >= 25) {
  p <- p + 
    geom_point(data = aqi_new, aes(x = row, y = aqi), color = "grey50", size = 4.5, alpha = .8) +
    geom_point(data = aqi_new, aes(x = row, y = aqi), color = "white", size = 4, alpha = .8) 
}

# Ripple effect
if(z < 29) p <- p + 
                geom_point(data = aqi_new, aes(x = row, y = aqi), color = "grey50", size = 0.8*z**0.81, alpha = 0.15 + 0.025 * abs(27-z), pch=21) 

# Fade in white circle
if(z < 25 && z > 9) { 
  p <- p + 
       geom_point(data = aqi_new, aes(x = row, y = aqi), color = "white", size = 0.8*(z-10)**0.54, alpha = .81 - 0.03 * abs(24-z)) 
}


if(z < 29 && z > 3) { 
  p <- p + 
       geom_label(data    = aqi_new,
                  aes(x = row, y = aqi + 30 - 60 * (aqi > 105), label = aqi), 
                  color   = "grey50", 
                  size    = 4.2 - 0.047 * abs(27-z), 
                  alpha   = .95 - 0.025 * abs(28-z),
                  family  = c("serif", "mono")[2])
}

if(z >= 29 && z < 37) { 
  p <- p + 
    geom_label(data    = aqi_new,
               aes(x = row, y = aqi + 30 - 60 * (aqi > 105), label = aqi), 
               color   = "grey50", 
               size    = 4.2, 
               alpha   = .95,
               family  = c("serif", "mono")[2])
}


p <- p +
  guides(fill = "none") +
  scale_fill_manual(values = as.character(aqi_refs$col)) +
  labs(x = NULL, y = NULL, subtitle = "Air Quality Index") +
  scale_x_continuous(breaks = aqi2$row, labels = c("", aqi2$time_txt[-c(1, nrow(aqi2))], ""), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, min(c(seq(150, 200, 50), 300, 500)[c(seq(150, 200, 50), 300, 500) >= max(aqi2$aqi, na.rm=T)])), 
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor= element_blank(), 
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size=7.3),
        plot.subtitle = element_text(size=9, color="grey40"))
  
#print(p)

p

ggsave(paste0(img_count, ".png"), width=4.7, height=2)

}

  if(i == (max(aqi2$time)-1)) for(y in 1:5) {
    img_count <- img_count + 1
    ggsave(paste0(img_count, ".png"), width=4.7, height=2)

    }

}
list.files() %>% 
  .[grepl("png", .)] %>% 
  .[order(as.numeric(sub("([0-9]*).*", "\\1", .)))] %>% 
  image_read() %>%
  image_join() %>%
  image_animate(fps=(20)) %>%
  image_write("ozone_chart.gif")


# Push to github


}


# Close

