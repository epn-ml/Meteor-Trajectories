# Author:         Lucas Mandl
# Company:        FH Joanneum - DAT 21
# Date:           May 2023
# Description:    Master thesis - data exploration
# ============================================
rm(list = ls())       # clear workspace
cat("\014")           # clear command line
# set working directory to source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#dev.off(dev.list()["RStudioGD"])
# ============================================#

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(bmp)
library(pixmap)

load("meteor_data.Rda")

# filter for datasets with more than 1 and less than 31 frames
#meteor_data_df <- meteor_data_df %>% filter(sN > 1, sN <= 30)

# meteor_data_df_cam1 <- meteor_data_df %>% filter(cam == "CAM1")

#lc_idx <- 130

# plot light curve (LC), df - meteor dataframe, idx - index in data frame
plot_lc <- function(df, idx)
{
  # convert magnitude string into a vector & make a df for plotting
  mag <- as.numeric(strsplit(df$mag[idx], ";")[[1]])
  df_lc <- data.frame(idx = c(1:length(mag)), mag = mag)

  # extract date, time and number of frames (sN) for title
  date <- paste0(df$day[idx], ".", df$month[idx], ".", df$year[idx])
  hour <- str_pad(df$hour[idx], 2, pad = "0")
  minute <- str_pad(df$minute[idx], 2, pad = "0")
  second <- df$second[idx]
  time <- paste0(hour, ":", minute, ":", second)
  frames <- df$sN[idx]

  # make title
  title <- paste0("Meteor light curve \n", date, " ", time)

  # plot LC
  p <- ggplot(df_lc, aes(x = idx, y = mag)) +
    geom_line() +
    labs(title = paste0("Light curve magnitude, ", df$cam[idx]),
         subtitle = paste0("Date/time: ", date, " ", time, "\nFrames: ", frames, ", index = ", idx))

  #print(p)

  # convert coordinates (right ascension & declination) from string to vectors 
  # and make df for plotting
  ra <- as.numeric(strsplit(df$ra[idx], ";")[[1]])
  dec <- as.numeric(strsplit(df$dec[idx], ";")[[1]])
  df_coord <- data.frame(ra = ra, dec = dec)

  # strangeness = deviation from straight path
  strangeness_ra <- round(df$strange_ra[idx], 2)
  strangeness_dec <- round(df$strange_dec[idx], 2)

  # delta right ascension, delta declination (can be ignored,, still testing)
  dra <- round(df$delta_ra[idx], 2)
  ddec <- round(df$delta_dec[idx], 2)

  # plot the path of the meteor
  p2 <- ggplot(df_coord, aes(x = ra, y = dec)) +
    geom_line() +
    labs(title = "Meteor course",
         subtitle = paste0("Right ascension & declination \n",
                           "Strangeness: ra: ", strangeness_ra, " (", dra,
                           ") dec: ", strangeness_dec, " (", ddec, ")",
                           "\nStraight path deviation: ", round(df$sp_deviation[idx], 2)))

  #path_bmp <- paste0(meteor_data_df$path[idx], "/", meteor_data_df$fname_bmp[idx])
  #img <- read.bmp(path_bmp)
  #pr=pixmapRGB(img)
  #plot(pr, main = title)

  # arrange the plots into one plot
  grid.arrange(p, p2, nrow = 2)
}

# plot the light curves one by one, 
# seq: switch for plotting LC data sequentially
#     TRUE - plot sequentially, FALSE - plot only one
# idx: if seq is set to FALSE, only the LC with the index idx will be shown
# ==============================================================================
# USAGE: press ENTER to display next LC, ESC to stop plotting
# ==============================================================================
plot_sequential <- function(seq = TRUE, idx = 1)
{
  if(seq)
  {
    for(i in c(1:100))
    {
      cat("\014")
      print("Press enter for next plot")
      readline()
      plot_lc(meteor_data_df, i)
      print(paste0("Plot: ", i))
    }
  } else {
    plot_lc(meteor_data_df, idx)
  }
}

#meteor_data_df$strange_ra <- 0
#meteor_data_df$strange_dec <- 0

#meteor_data_df$delta_ra <- meteor_data_df$ra_end - meteor_data_df$ra_start
#meteor_data_df$delta_dec <- meteor_data_df$dec_end - meteor_data_df$dec_start




meteor_data_df <- check_for_strange_path(meteor_data_df)

meteor_data_df$sp_deviation <- 0

# calculate the length between the start and endpoint of the LC 
get_straight_path_deviation <- function(df)
{
  n <- nrow(df)
  for(i in c(1:n))
  {
    #i <- 1
    ra <- as.numeric(strsplit(df$ra[i], ";")[[1]])
    dec <- as.numeric(strsplit(df$dec[i], ";")[[1]])
    
    ra_start <- as.numeric(df$ra_start[i])
    ra_end <- as.numeric(df$ra_end[i])
    dec_start <- as.numeric(df$dec_start[i])
    dec_end <- as.numeric(df$dec_end[i])
    
    l_sp <- sqrt((ra_end - ra_start)^2 + (dec_end - dec_start)^2)
    
    l_actual <- 0
    for(j in c(1:(length(ra)-1)))
    {
      x1 = ra[j]
      x2 = ra[j+1]
      
      y1 = dec[j]
      y2 = dec[j+1]
      
      l = sqrt((x2 - x1)^2 + (y2 - y1)^2)
      l_actual <- l_actual + l
    }
    if(length(ra) > 1) {
      df$sp_deviation[i] <- l_sp/l_actual
    } else {
      df$sp_deviation[i] <- -1
    }
  }
  return(df)
}

meteor_data_df <- get_straight_path_deviation(meteor_data_df)

meteor_data_df$sp_deviation[1]

  
#plot_sequential(TRUE) # enter to display next LC, esc to stop
plot_sequential(FALSE, 46)
plot_sequential(FALSE, 34)
#plot_sequential(FALSE, 24)
#plot_sequential(FALSE, 36)

#i <- 1
#path_bmp <- paste0(meteor_data_df$path[i], "/", meteor_data_df$fname_bmp[i])
#img <- read.bmp(path_bmp)
#pr=pixmapRGB(img)
#r=read.bmp('mygreyimage.bmp')
#pr=pixmapGrey(r)
#plot(pr)

# plot the distribution of number of frames per measurement
frames_plot <- ggplot(meteor_data_df, aes(x = sN)) +
  geom_bar() +
  facet_grid((vars(cam))) +
  xlab("Number of frames") +
  ylab("Measurements") +
  ggtitle("Measurements per frame count for every camera")

#print(frames_plot)

# === ignore, was testing a parameter for the crookedness of a curve - does not work yet ===
check_for_strange_path <- function(df)
{
  n <- nrow(df)
  for(i in c(1:n))
  {
    ra <- as.numeric(strsplit(df$ra[i], ";")[[1]])
    dec <- as.numeric(strsplit(df$dec[i], ";")[[1]])
    if(length(ra) > 1) {
      df$strange_ra[i] <- delta_sum(ra)
      df$strange_dec[i] <- delta_sum(dec)
    }
  }
  return(df)
}

# === ignore, was testing a parameter for the crookedness of a curve - does not work yet ===
delta_sum <- function(vec)
{
  n <- length(vec) - 1
  dx <- 0
  for(i in c(1:n))
  {
    x1 <- vec[i]
    x2 <- vec[i + 1]
    dx <- dx + abs(x1 - x2)
  }
  return(dx/n)
}

# ====== Legacy code - will not run =====
if(FALSE)
{
  mag <- as.numeric(strsplit(meteor_data_df$mag[lc_idx], ";")[[1]])
  sN <- meteor_data_df$sN[lc_idx]

  mag_df <- data.frame(idx = c(1:length(mag)), mag = mag)


  ra <- as.numeric(strsplit(meteor_data_df$ra[lc_idx], ";")[[1]])
  dec <- as.numeric(strsplit(meteor_data_df$dec[lc_idx], ";")[[1]])

  coord_df <- data.frame(ra = ra, dec = dec)

  ra2 <- as.numeric(strsplit(meteor_data_df$ra[lc_idx+1], ";")[[1]])
  dec2 <- as.numeric(strsplit(meteor_data_df$dec[lc_idx+1], ";")[[1]])

  coord_df2 <- data.frame(ra = ra2, dec = dec2)

  p <- ggplot(mag_df, aes(x = idx, y = mag)) +
    geom_line() #+
    #ggtitle(paste0("Light curve from ", date, " at ", time, " (frames: ",sN,")"))

  #print(p)

  p <- ggplot(coord_df, aes(x = ra, y = dec)) +
    geom_line() #+
    #ggtitle(paste0("Light curve from ", date, " at ", time, " (frames: ",sN,")"))

  #print(p)

  coord_plot <- ggplot() +
    geom_line(aes(x = coord_df$ra, y = coord_df$dec)) #+
    #geom_line(aes(x = coord_df2$ra, y = coord_df2$dec))
  coord_plot <- coord_plot + geom_line(aes(x = coord_df2$ra, y = coord_df2$dec))


  print(coord_plot)
}



if(0)
{
  plot_all_meteors <- function(df)#, md_plot)
  {
    print(nrow(df))
    md_plot <- ggplot()
    n <- 10#nrow(df)
    for(i in 1:n)
    {
      #print(i)
      ra1 <- as.numeric(strsplit(df$ra[i], ";")[[1]])
      dec1 <- as.numeric(strsplit(df$dec[i], ";")[[1]])

      coord_df <- data.frame(ra = ra1, dec = dec1)
      #print(coord_df)
      #md_plot <- md_plot + geom_line(aes(x = coord_df$ra, y = coord_df$dec))
      md_plot = md_plot + geom_line(aes(x = ra1, y = dec1))
      #md_plot = md_plot + geom_line(aes(x = coord_df$ra, y = coord_df$dec))
      print(md_plot)


      if(exists('coord_df')){rm(coord_df)}
    }
    return(md_plot)
  }

  #coord_plot <- ggplot()
  #md_plot <- ggplot()

  #i = 1

  #ra <- as.numeric(strsplit(meteor_data_df$ra[i], ";")[[1]])
  #dec <- as.numeric(strsplit(meteor_data_df$dec[i], ";")[[1]])

  #coord_df <- data.frame(ra = ra, dec = dec)

  #md_plot + geom_line(aes(x = coord_df$ra, y = coord_df$dec))

  coord_plot2 <- plot_all_meteors(meteor_data_df_cam1)#, md_plot)
  print(coord_plot2)
}
#print(md_plot)

if(FALSE)
{
  df <- meteor_data_df_cam1
  coord_plot <- ggplot()

  i = 1
  ra1 <- as.numeric(strsplit(df$ra[i], ";")[[1]])
  dec1 <- as.numeric(strsplit(df$dec[i], ";")[[1]])
  coord_df <- data.frame(ra = ra1, dec = dec1)

  coord_plot = coord_plot + geom_line(aes(x = coord_df$ra, y = coord_df$dec))
  print(coord_plot)

  i = 2
  ra1 <- as.numeric(strsplit(df$ra[i], ";")[[1]])
  dec1 <- as.numeric(strsplit(df$dec[i], ";")[[1]])
  coord_df1 <- data.frame(ra = ra1, dec = dec1)

  coord_plot = coord_plot + geom_line(aes(x = coord_df1$ra, y = coord_df1$dec))
  print(coord_plot)
}

if(0)
{
  meteor_data_df_filtered <- meteor_data_df %>% filter((ra_start < 10) | (ra_start > 350))
  meteor_data_df_filtered <- meteor_data_df_filtered %>% filter((ra_end < 10) | (ra_end > 350))
  meteor_data_df_filtered <- meteor_data_df %>% filter((dec_start < 10) | (dec_start > 80))
  meteor_data_df_filtered <- meteor_data_df %>% filter((dec_end < -20) | (dec_end > 85))

  print("Ra_start:")
  print(min(meteor_data_df_filtered$ra_start))
  print(max(meteor_data_df_filtered$ra_start))

  print("Ra_end:")
  print(min(meteor_data_df_filtered$ra_end))
  print(max(meteor_data_df_filtered$ra_end))

  print("dec_start:")
  print(min(meteor_data_df_filtered$dec_start))
  print(max(meteor_data_df_filtered$dec_start))

  print("dec_end:")
  print(min(meteor_data_df_filtered$dec_end))
  print(max(meteor_data_df_filtered$dec_end))


  barplot(table(sort(round(as.integer(meteor_data_df$ra_start), digits = -1))))
  barplot(table(sort(round(as.integer(meteor_data_df$ra_end), digits = -1))))
  barplot(table(sort(round(as.integer(meteor_data_df$dec_start), digits = -1))))
  barplot(table(sort(round(as.integer(meteor_data_df$dec_end), digits = -1))))

  print("az_start:")
  print(min(meteor_data_df_cam1$az_start))
  print(max(meteor_data_df_cam1$az_start))

  print("az_end:")
  print(min(meteor_data_df_cam1$az_end))
  print(max(meteor_data_df_cam1$az_end))

  print("ev_start:")
  print(min(meteor_data_df_cam1$ev_start))
  print(max(meteor_data_df_cam1$ev_start))

  print("ev_end:")
  print(min(meteor_data_df_cam1$ev_end))
  print(max(meteor_data_df_cam1$ev_end))
}

