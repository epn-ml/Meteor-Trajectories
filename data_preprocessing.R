# Author:         Lucas Mandl
# Company:        FH Joanneum - DAT 21
# Date:           May 2023
# Description:    Master thesis - data preprocessing
# ============================================
rm(list = ls())       # clear workspace
cat("\014")           # clear command line
# set working directory to source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ============================================#

library(tidyverse)
library(xml2)
#library(XML)
library(ggplot2)
library(gridExtra)

# define file path of extracted data (unzipped)
fpath = "meteor-data/extracted_data"

# extract all paths in the given folder
dir_list <- list.dirs(fpath, recursive = TRUE)

get_files_in_dir <- function(dir)
{
  # get all files in dir, filter for xml files
  files <- list.files(dir)
  xml_data <- files[grepl("XML", files)]
  bmp_data <- files[grepl("bmp", files)]

  # reduce strings to date and time e.g. M20130403_003556
  xml_data_reduced <- substring(xml_data, 1, 16)
  bmp_data_reduced <- substring(bmp_data, 1, 16)

  # number of files in current directory
  n_files <- length(xml_data)

  # create temporary data frame to store extracted data
  # pre-allocate dataframe as number of files in dir is already known
  meteor_data_temp <- create_df(n_files)

  # iterate over each file individually, grab date & time from file name
  # read out parameters from XML and save to temporary data frame
  for(i in c(1:n_files))
  {
    # only write to dataframe if the filename starts with M20.. as there are
    # some other XML files which are not measurements
    if(grepl("M20", xml_data[i]))
    {
      year <-   substring(xml_data[i], 2, 5)
      month <-  substring(xml_data[i], 6, 7)
      day <-    substring(xml_data[i], 8, 9)
      hour <-   substring(xml_data[i], 11, 12)
      minute <- substring(xml_data[i], 13, 14)
      second <- substring(xml_data[i], 15, 16)

      # extract camera name
      cam <- substring(dir, 28, 31)

      # open XML file
      path <- paste0(dir, "/", xml_data[i])
      md_data <- read_xml(path)

      # extract number of observed frames (sN)
      ua2_object <- xml_find_all(md_data, ".//ua2_object")
      sN <- xml_attr(ua2_object, "sN")

      # extract light curve data and convert it to a single string (magnitude)
      ua2_fdata2 <- xml_find_all(md_data, ".//ua2_fdata2")
      mag <- xml_attr(ua2_fdata2, "mag")
      mag_str <- paste(mag, collapse = ";")

      # extract coordinate data (right ascension and declination)
      ra <- xml_attr(ua2_fdata2, "ra")
      dec <- xml_attr(ua2_fdata2, "dec")

      # extract coordinate data (azimute & elevation)
      az <- xml_attr(ua2_fdata2, "az")
      ev <- xml_attr(ua2_fdata2, "ev")

      # extract bmp filename corresponding to current xml file
      if(bmp_data_reduced[i] %in% xml_data_reduced)
      {
        bmp_idx <- which(bmp_data_reduced == bmp_data_reduced[i])
        bmp_fname <- bmp_data[bmp_idx]
      } else {
        bmp_fname <- "No bmp"
      }

      # only take data which contains more than one measurement
      if(length(sN) == 1)
      {
        # extract coordinates of the camera
        cam_az <- as.numeric(xml_attr(md_data, "az"))
        cam_ev <- as.numeric(xml_attr(md_data, "ev"))
        
        # start and enpoint of the light curve
        ra_start <- as.numeric(ra[1])
        ra_end <- as.numeric(ra[length(ra)])

        dec_start <- as.numeric(dec[1])
        dec_end <- as.numeric(dec[length(dec)])

        az_start <- as.numeric(az[1])
        az_end <- as.numeric(az[length(az)])

        ev_start <- as.numeric(ev[1])
        ev_end <- as.numeric(ev[length(ev)])
        
        # parameter which states if the LC is in  the frame, see documentation 
        # --> there the parameter is called (lo)
        io <- xml_attr(ua2_object, "io")

      } else {
        ra_start <- 0
        ra_end <- 0
        dec_start <- 0
        dec_end <- 0

        az_start <- 0
        az_end <- 0

        ev_start <- 0
        ev_end <- 0

        cam_az <- 0
        cam_ev <- 0

        io <- 0
      }

      # collapse the vectors for the coordinates into a single comma seperated
      # string in order to store it in the data frame
      ra_str <- paste(ra, collapse = ";")
      dec_str <- paste(dec, collapse = ";")

      az_str <- paste(az, collapse = ";")
      ev_str <- paste(ev, collapse = ";")

      # observations with more than one object present: fN = NA
      # observations with no object present (false positive): fN = 0
      # third case for other unusual events
      if(length(sN) == 1) {
        sN <- as.numeric(sN)
      } else if (length(sN) > 1) {
        sN <- -1
      } else if (length(sN) == 0) {
        sN <- 0
      } else {
        sN <- NA
      }

      # write data to temp df
      meteor_data_temp$path[i]  	   <- dir
      meteor_data_temp$fname[i] 	   <- xml_data[i]
      meteor_data_temp$fname_bmp[i]  <- bmp_fname
      meteor_data_temp$cam[i]        <- cam
      meteor_data_temp$year[i]  	   <- as.numeric(year)
      meteor_data_temp$month[i] 	   <- as.numeric(month)
      meteor_data_temp$day[i]   	   <- as.numeric(day)
      meteor_data_temp$hour[i]  	   <- as.numeric(hour)
      meteor_data_temp$minute[i]	   <- as.numeric(minute)
      meteor_data_temp$second[i]	   <- as.numeric(second)
      meteor_data_temp$sN[i]  		   <- sN # number of frames (measuring points)
      meteor_data_temp$mag[i]        <- mag_str
      meteor_data_temp$ra[i]         <- ra_str
      meteor_data_temp$dec[i]        <- dec_str
      meteor_data_temp$ra_start[i]   <- ra_start
      meteor_data_temp$ra_end[i]     <- ra_end
      meteor_data_temp$dec_start[i]  <- dec_start
      meteor_data_temp$dec_end[i]    <- dec_end
      meteor_data_temp$io[i]         <- io
      #meteor_data_temp$az[i]         <- az_str
      #meteor_data_temp$ev[i]        <- ev_str
      #meteor_data_temp$az_start[i]  <- az_start
      #meteor_data_temp$az_end[i]    <- az_end
      #meteor_data_temp$ev_start[i]  <- ev_start
      #meteor_data_temp$ev_end[i]    <- ev_end
      #meteor_data_temp$cam_az[i]    <- cam_az
      #meteor_data_temp$cam_ev[i]    <- cam_ev
    }
  }
  return(meteor_data_temp)
}

# create the data frame, if data is added, expand the df here
create_df <- function(n_rows = 0)
{
  return(data.frame(path      = character(n_rows),
                    fname     = character(n_rows),
                    fname_bmp = character(n_rows),
                    cam       = character(n_rows),
                    year      = numeric(n_rows),
                    month     = numeric(n_rows),
                    day       = numeric(n_rows),
                    hour      = numeric(n_rows),
                    minute    = numeric(n_rows),
                    second    = numeric(n_rows),
                    sN        = double(n_rows),
                    mag       = character(n_rows),
                    ra        = character(n_rows),
                    dec       = character(n_rows),
                    ra_start  = numeric(n_rows),
                    ra_end    = numeric(n_rows),
                    dec_start = numeric(n_rows),
                    dec_end   = numeric(n_rows),
                    io        = numeric(n_rows)#,
                    #az        = character(n_rows),
                    #ev        = character(n_rows),
                    #az_start  = numeric(n_rows),
                    #az_end    = numeric(n_rows),
                    #ev_start  = numeric(n_rows),
                    #ev_end    = numeric(n_rows),
                    #cam_az    = numeric(n_rows),
                    #cam_ev    = numeric(n_rows)
                    ))
}
# number of directories in the folder
n <- length(dir_list)

# create an empty df for storing the data
meteor_data_df <- create_df()

# iterate over all directories & extract dirs containing xml files
for(i in c(1:n))
{
  # if the dir contains a xml file, extract data from all relevant XML files
  if(TRUE %in% grepl("XML", list.files(dir_list[i])))
  {
    cat("\014")
    print(paste0("Searching dir ", i, " of ", n))
    print(paste0("<",strrep("=",as.integer(i/5)),strrep(" ", as.integer((n-i)/5)),">"))
    print(paste0(nrow(meteor_data_df), " files found"))

    meteor_data_df <- rbind(meteor_data_df, get_files_in_dir(dir_list[i]))
  }
}

# delete empty rows
meteor_data_df <- meteor_data_df %>% filter(path != "")

#barplot(table(meteor_data_df$sN))

meteor_data_df_filtered <- meteor_data_df %>% filter(sN >= 1, sN <= 30)

# Save the dataframe
save(meteor_data_df, file = "meteor_data.Rda")

#plot_lc(6)

# ==== Legacy code - will not run ====
if(0)
{

  plot_lc <- function(lc_idx)
  {
    wd <- getwd()

    path <- paste0(meteor_data_df_filtered$path[lc_idx],
                   "/",
                   meteor_data_df_filtered$fname[lc_idx])

    md_data <- read_xml(path)

    ua2_object <- xml_find_all(md_data, ".//ua2_fdata2")
    mag <- as.numeric(xml_attr(ua2_object, "mag"))
    ra <- as.double(xml_attr(ua2_object, "ra"))
    dec <- as.double(xml_attr(ua2_object, "dec"))

    ua2_object <- xml_find_all(md_data, ".//ua2_object")
    sN <- xml_attr(ua2_object, "sN")

    mag_df <- data.frame(idx = c(1:length(mag)), mag = 1/mag)

    date <- paste0(meteor_data_df$day[lc_idx],".",
                   meteor_data_df$month[lc_idx], ".",
                   meteor_data_df$year[lc_idx])

    time <- paste0(meteor_data_df$hour[lc_idx],":",
                   meteor_data_df$minute[lc_idx])

    cam <- meteor_data_df$cam[lc_idx]

    p <- ggplot(mag_df, aes(x = idx, y = mag)) +
      geom_line() +
      ggtitle(paste0("Light curve from ", date, " at ", time, " (frames: ",sN,")"))

    coord_df <- data.frame(ra = ra, dec = dec)

    p_c <- ggplot(coord_df, aes(x = ra, y = dec)) +
      geom_line()

    #p <- qplot(1)
    #p_c <- qplot(1)
    #grid.arrange(p, p_c, ncol=2)
    #par(mfrow=c(2,1))
    print(p)
    #print(p_c)
  }




  year_ = 2014
  month_ = 8
  day_ = 11
  hour_ = 21

  filtered <- meteor_data_df %>% filter(year == year_,
                                        month == month_,
                                        day == day_,
                                        hour == hour_)

  p1 <- ggplot(meteor_data_df, aes(x = sN)) +
    geom_bar() +
    facet_grid((vars(cam)))


  print(p1)

  #print_fpath <- function(file_number)
  #{
  #  print(paste0(getwd(), "/", meteor_data_df_filtered$path[file_number],
  #               "/", meteor_data_df_filtered$fname[file_number]))
  #}

  #print_fpath(121)


  #frames_table <- as.data.frame(table(meteor_data_df$sN))
  #frames_table$Var1 <- as.numeric(levels(frames_table$Var1))[frames_table$Var1]

  #path <- paste0(meteor_data_df$path[1], "/", meteor_data_df$fname[1])
  #print(path)
  #xmlParse(read_xml("M20130403_003556_ARMAGH_BGA.XML"))
  #xmlParse(read_xml(file(as.character(path))))
  #test <- xmlParse(path)
  #rootnode <- xmlRoot(test)
  #nodes <- xmlSize(rootnode)

  #second_node <- rootnode[1]

  #test_df <- xmlToDataFrame(test)

  #obj_path <- second_node[["ua2_objects"]][["ua2_object"]][["ua2_objpath"]]
  #xmlToDataFrame(path)


  #md_data <- read_xml(path)


  #ua2_fdata2 <- xml_find_all(md_data, ".//ua2_fdata2")
  #as.numeric(xml_attr(ua2_fdata2, "fno"))

  #ua2_object <- xml_find_all(md_data, ".//ua2_object")
  #xml_attr(ua2_object, "sN")

  #length(xml_attr(ua2_object, "sN"))

  #m <- 10
  #meteor_data_temp <- data.frame(path = character(m),
  #                               fname = character(m),
  #                               year = numeric(m))

  #meteor_data_temp[nrow(meteor_data_temp) + 1, ] <- c("test", "test", as.numeric(2015))

  lc_idx = 2
  wd <- getwd()

  path <- paste0(meteor_data_df_filtered$path[lc_idx],
                 "/",
                 meteor_data_df_filtered$fname[lc_idx])

  md_data <- read_xml(path)

  ua2_object <- xml_find_all(md_data, ".//ua2_fdata2")
  mag <- as.numeric(xml_attr(ua2_object, "mag"))
  ra <- as.double(xml_attr(ua2_object, "ra"))
  dec <- as.double(xml_attr(ua2_object, "dec"))

  coord_df <- data.frame(ra = ra, dec = dec)

  p_c <- ggplot(coord_df, aes(x = ra, y = dec)) +
    geom_line()

  print(p_c)

  ua2_object <- xml_find_all(md_data, ".//ua2_object")
  sN <- xml_attr(ua2_object, "sN")

  mag_df <- data.frame(idx = c(1:length(mag)), mag = 1/mag)

  date <- paste0(meteor_data_df$day[lc_idx],".",
                 meteor_data_df$month[lc_idx], ".",
                 meteor_data_df$year[lc_idx])

  time <- paste0(meteor_data_df$hour[lc_idx],":",
                 meteor_data_df$minute[lc_idx])

  cam <- meteor_data_df$cam[lc_idx]

  p <- ggplot(mag_df, aes(x = idx, y = mag)) +
    geom_line() +
    ggtitle(paste0("Light curve from ", date, " at ", time, " (frames: ",sN,")"))

  print(p)

  dec_str <- paste(dec, collapse = ";")
  print(dec_str)
  as.numeric(strsplit(dec_str, ";")[[1]])
}
