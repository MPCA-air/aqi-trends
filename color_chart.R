#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(ggplot2)
library(magick)

#devtools::install_github("hadley/ggplot2")

# Check time
#format(Sys.time(), "%H")


options(warn = -1)

#require(installr) 
#install.ImageMagick()

size_adjust <- 0.67

# Load credentials
credentials <- read_csv("C:/Users/dkvale/Desktop/credentials.csv")


setwd("C:/Users/dkvale/Desktop/State_Fair_2016/Color_chart")


# Load past AQI values
aqi_test <- read_csv("row, time, time_txt, aqi
                 0,0,0am, NA
                 1,23,11pm, 22
                 2,2,2am, 33
                 3,3,3am, 28
                 4,4,4am, 55
                 5,5,5am, 66
                 6,6,6am, 58
                 7,7,7am, 75
                 8,8,8am, 88
                 9,9,9am, 99
                10,10,10am, 95
                11,11,11am, 108
                12,12,12pm, 118
                13,13,1pm, NA")

aqi_old <- read_csv("aqi_old.csv")

aqi_old$aqi  <- as.integer(aqi_old$aqi)
aqi_old$time <- as.integer(aqi_old$time)

# Request data from SmogWatch
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

if(new_time_24 != aqi_old[13, ]$time) {

  twin_cities <- grep("Twin", aqi_new)
  
  new_ozone <-  grep("cc_ozone_cell", aqi_new) %>%
                  .[. > twin_cities] %>%
                  aqi_new[.] %>%
                  strsplit(. , ">") %>% 
                  unlist(.) %>% 
                  .[2] %>%
                  gsub("<[///]div", "", .) %>%
                  as.numeric()
  
  
  new_pm25 <-  grep("cc_pm_cell", aqi_new) %>%
                 .[. > twin_cities] %>%
                 aqi_new[.] %>%
                 strsplit(. , ">") %>% 
                 unlist(.) %>% 
                 .[2] %>%
                 gsub("<[///]div", "", .) %>%
                 as.numeric()
  
  new_value <- max(new_ozone, new_pm25, na.rm=T)
  
  # Quality check
  
  if(!is.na(new_value)) {
  
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
  aqi_new$row <- 12
  
  aqi_new <- rbind(aqi_old[-c(1, nrow(aqi_old)), ], aqi_new)
  
  aqi_new[1, ]$aqi <- NA
  
  aqi_new <- rbind(aqi_new, 
                   data_frame(row      = 13,
                              time     = as.integer(format(strptime(new_time, "%I:%M %p")+60*60, "%H")),
                              time_txt = "",
                              aqi      = NA))
  
  aqi_new$row <- 0:13
  
  write_csv(aqi_new, "aqi_old.csv")

  
# Create background colors
aqi_refs <- data.frame(xstart = c(seq(0,150,50), 200, 300),
                       xend = c(seq(50,200,50), 300, 500),
                       col = c("#53BF33", "#F4C60B", "#DB6B1A", "#c81d25", "#52154E", "#4c061d"), 
                       stringsAsFactors = F)

aqi_refs$col <- factor(aqi_refs$col, ordered = T, levels = aqi_refs$col)


aqi2 <- aqi_new

time_labels <- c("", aqi2$time_txt[-c(1, nrow(aqi2))], "")

setwd("charts")

img_count <- 0

par(mar=c(0,0,0,0))


for(i in 2:13){
  
  aqi <- aqi2[1:i, ]
  
  aqi_last <- aqi2[2:max(c(2, i-1)), ]
  
  aqi_new <- aqi2[i, ]
  
for(z in seq(1, 37, 2)) {
  
  img_count <- img_count + 1
  
p <- ggplot() +
  geom_rect(data = aqi_refs, aes(ymin = xstart, ymax = xend, 
                                 xmin = 0, xmax = 13, 
                                 fill = col), alpha = 0.74) 

# Background line
p <- p + 
  geom_line(data = aqi2[!is.na(aqi2$aqi), ], aes(x = row, y = aqi), size =1.1*size_adjust, color="grey40", alpha = 0.08) +
  #geom_line(data = aqi2[!is.na(aqi2$aqi), ], aes(x = time, y = aqi), size =1, color="grey50", alpha = 0.07) +
  #geom_point(data = aqi2, aes(x = time, y = aqi), color = "grey50", size = 5.7,, alpha = 0.09) +
  geom_point(data = aqi2[!is.na(aqi2$aqi), ], aes(x = row, y = aqi), color = "grey40",  size = 4*size_adjust, alpha = 0.06)



# Connecting lines
if(z < 25 && nrow(aqi) > 1) {
  p <- p + 
       geom_line(data = aqi_last, aes(x = row, y = aqi * .996), size =1.1*size_adjust, color="grey20", alpha = 0.15) +
       geom_line(data = aqi_last, aes(x = row, y = aqi), size =1*size_adjust, color="grey40", alpha = 0.65)
}  

if(z >= 25 && nrow(aqi) > 1) {
    p <- p + 
      geom_line(data = aqi, aes(x = row, y = aqi * .996), size =1.1*size_adjust, color="grey20", alpha = 0.15) +
      geom_line(data = aqi, aes(x = row, y = aqi), size =1*size_adjust, color="grey40", alpha = 0.65) 
}  
  

# Previous points
p <- p + 
    geom_point(data = aqi_last, aes(x = row, y = aqi), color = "grey50", size = 4.5*size_adjust) +
    geom_point(data = aqi_last, aes(x = row, y = aqi), color = "white", size = 4*size_adjust)

# New point
if(z >= 25) {
  p <- p + 
    geom_point(data = aqi_new, aes(x = row, y = aqi), color = "grey50", size = 4.5*size_adjust, alpha = .8) +
    geom_point(data = aqi_new, aes(x = row, y = aqi), color = "white", size = 4*size_adjust, alpha = .8) 
}

# Ripple effect
if(z < 29) p <- p + 
                geom_point(data = aqi_new, aes(x = row, y = aqi), color = "grey50", size = 0.8*z**0.81*size_adjust, alpha = 0.15 + 0.025 * abs(27-z), pch=21) 

# Fade in white circle
if(z < 25 && z > 9) { 
  p <- p + 
       geom_point(data = aqi_new, aes(x = row, y = aqi), color = "white", size = 0.8*(z-10)**0.54 *size_adjust, alpha = .83 - 0.03 * abs(24-z)) 
}


if(z < 29 && z > 3) { 
  p <- p + 
       geom_label(data    = aqi_new,
                  aes(x = row, y = aqi + 30 - 60 * (aqi > 105), label = aqi), 
                  color   = "grey40", 
                  size    = size_adjust * 4.2 - 0.047 * abs(27-z), 
                  alpha   = .95 - 0.025 * abs(28-z),
                  family  = c("serif", "mono")[2])
}

if(z >= 29 && z < 37) { 
  p <- p + 
    geom_label(data    = aqi_new,
               aes(x = row, y = aqi + 30 - 60 * (aqi > 105), label = aqi), 
               color   = "grey40", 
               size    = 4.2 * size_adjust, 
               alpha   = .95,
               family  = c("serif", "mono")[2])
}


p <- p +
  guides(fill = "none") +
  scale_fill_manual(values = as.character(aqi_refs$col)) +
  labs(x = NULL, y = NULL) + 
  #labs(subtitle = "Air Quality Index") +
  scale_x_continuous(breaks = aqi2$row, labels = time_labels, expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, min(c(seq(150, 200, 50), 300, 500)[c(seq(150, 200, 50), 300, 500) >= max(aqi2$aqi, na.rm=T)])), 
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor= element_blank(), 
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=7.5*size_adjust*1.3),
        axis.text.x = element_text(size=6*size_adjust*1.25),
        plot.subtitle = element_text(size=8.2*size_adjust, color="grey30"),
        axis.ticks = element_line(size = 0.25),
        axis.ticks.length = unit(.04, "cm"))
  
p

#ggsave(paste0(img_count, ".png"), width=4, height=1.7)
ggsave(paste0(img_count, ".png"), width=4, height=0.85)

}

  if(i == 13) for(y in 1:5) {
    img_count <- img_count + 1
    #ggsave(paste0(img_count, ".png"), width=4, height=1.7)
    ggsave(paste0(img_count, ".png"), width=4, height=0.85) 
    }

}

list.files() %>% 
  .[grepl("png", .)] %>% 
  .[order(as.numeric(sub("([0-9]*).*", "\\1", .)))] %>% 
  image_read() %>%
  image_join() %>%
  image_animate(fps=(20)) %>%
  image_write("ozone_chart.gif")


# Push to github if afer 7am and before 10pm

if(new_time_24 < 22 && new_time_24 > 6) {
  
git <- 'C: & CD "C:/Users/dkvale/Desktop/State_Fair_2016/signup-aqi" & "C:/Users/dkvale/AppData/Local/Programs/Git/bin/git.exe" '


shell(paste0('C: & copy "C:/Users/dkvale/Desktop/State_Fair_2016/Color_chart/charts/ozone_chart.gif" ',
             '"C:/Users/dkvale/Desktop/State_Fair_2016/signup-aqi/ozone_chart.gif"'))

shell(paste0(git, "add ozone_chart.gif"))

commit <- paste0(git, 'commit -a -m ', '"update aqi chart"')

shell(commit)

shell(paste0(git, "config --global user.name dkvale"))
shell(paste0(git, "config --global user.email ", credentials$email))
shell(paste0(git, "config credential.helper store"))

push <- paste0(git, "push -f origin master")
#push <- paste0(git, "push -f origin gh-pages")

shell(push)
}

}}


# Close

