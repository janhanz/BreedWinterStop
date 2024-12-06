#############################################################
# filtering and thinning of GPS records
# reads move2 format in an RDS file
#############################################################

### data processing and filtering

# create Long and Lat (geometry column is expected)
dt_bird_coords <- data %>% 
  st_coordinates() %>%
  data.frame() %>% 
  rename("Long" = X, "Lat" = Y)

# combine the data set with coordinates
dt_bird <- bind_cols(data, dt_bird_coords) %>% 
  data.frame() %>% 
  st_as_sf()

# variable selection - select relevant variables
#  drop rows and columns containing only NAs
#  break timestamp into separate columns
dt_bird <- dt_bird %>% 
  mutate(timestamp = mt_time(data)) %>% 
  select(any_of(c("event_id", "Long", "Lat", "timestamp", "ground_speed", "gps_hdop", "gps_satellite_count"))) %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  mutate(Year = year(timestamp), 
         Month = month(timestamp), 
         Day = day(timestamp),
         Hour = hour(timestamp),
         Minute = minute(timestamp),
         Second = second(timestamp)) %>% 
  select_if(~ !all(is.na(.)))

# remove rows with NA coordinates
if (any(is.na(dt_bird$Long) | is.na(dt_bird$Lat))) {
  logger.info("NAs in Lat or Long! Removing rows with incomplete coordinates.\n")
  dt_bird <- dt_bird %>% 
    filter(!is.na(Long) & !is.na(Lat))
}

# remove rows with any NAs
if (any(is.na(dt_bird))) {
  logger.info("NAs in other variables! Removing rows with incomplete records.\n")
  dt_bird <- dt_bird %>%
    filter(!if_any(everything(), is.na))
}

# convert ground speed to numeric
if("ground_speed" %in% colnames(dt_bird)) {
  dt_bird <- dt_bird %>% 
    mutate(ground_speed = as.numeric(ground_speed))
}

# check the percentage of records with HDOP >5
if ("gps_hdop" %in% colnames(dt_bird)) {
  logger.info(paste0("Data contains ", round(nrow(dt_bird[as.numeric(dt_bird$gps_hdop) > 5,])/nrow(dt_bird)*100,1)," % of records with HDOP >5."))
} else {
  logger.info(paste0("HDOP not available in the data."))
}

# check the percentage of records with GPS fixes of <4 satellites
if ("gps_satellite_count" %in% colnames(dt_bird)) {
  logger.info(paste0("Data contains ", round(nrow(dt_bird[as.numeric(dt_bird$gps_satellite_count) < 4,])/nrow(dt_bird)*100,1)," % of records with GPS fixes of <4 satellites."))
} else {
  logger.info(paste0("Number of satellites not available in the data."))
}

# filtering of rows with sufficient satellites in fix and gps_hdop values
dt_sub <- dt_bird %>% 
  filter(if_any(matches("gps_satellite_count"),  \(gps_satellite_count) as.numeric(gps_satellite_count) >= 4)) %>% 
  filter(if_any(matches("gps_hdop"),  \(gps_hdop) as.numeric(gps_hdop) <= 5))

# proportion of the original data after filtering by gps_hdop and gps_satellite_count
if ("gps_hdop" %in% colnames(dt_bird) | "gps_satellite_count" %in% colnames(dt_bird)) {
  logger.info(paste0("Data contains ", round(nrow(dt_sub)/nrow(dt_bird)*100,1), " % of the original data."))
}


###################################
### data thinning

# apply data thinning to at least 1-min interval if duplicated coordinates are present, but not when bursts should be preserved
if (dt_sub %>% select(geometry, timestamp) %>% n_distinct() != nrow(dt_sub) & !bursts_rec) {

  logger.info("Duplicate coordinates (likely due to bursts)! Thinning of 1 min is applied.\n")

  # start at the first record
  time_start <- dt_sub$timestamp[1]
  # create empty selection vector 
  vec_sel <- vector("list", nrow(dt_sub))
  # first record will be always selected
  vec_sel[[1]] <- TRUE
  
  for (i in 2:nrow(dt_sub)) {
    if (difftime(dt_sub$timestamp[i], time_start, units = "min") < 1) {
      vec_sel[[i]] <- FALSE
    } else {
      vec_sel[[i]] <- TRUE
      time_start <- dt_sub$timestamp[i]
    }
  }
  
  dt_sub$sel <- unlist(vec_sel)
  
  # filter relevant records (sel == TRUE)
  dt_sub <- dt_sub %>% 
    filter(sel == TRUE) %>% 
    select(-c(sel))
  
  # what share of original data has left after data thinning?
  logger.info(paste0("Thinned data contains", round(nrow(dt_sub)/nrow(dt_sub_orig)*100,1), "% of the filtered data."))
}


### calculation of distances (in meters), time intervals (in minutes) and speed (m/s) for consecutive records

dt <- dt_sub %>%
  mutate(
    Dist_consec = c(as.numeric(geodist(select(., Long, Lat), measure = "geodesic", sequential = TRUE)), NA),
    Time_consec = c(as.numeric(difftime(timestamp[-1], timestamp[-n()], units = "mins")), NA),
    Speed_consec = if (!gr_speed) Dist_consec / (Time_consec * 60) else NULL
  )

# select the desired speed
speed_sel <- ifelse(gr_speed, "ground_speed", "Speed_consec")

# drop rows with speed > max_flight_sp m/s, which are likely nonsensical
if (any(dt[[speed_sel]] > max_flight_sp, na.rm = TRUE)) {
  logger.info(paste0("Too high speed of flight (> ", max_flight_sp, " m/s), dropping rows: ", 
                     paste(which(dt[[speed_sel]] > max_flight_sp), collapse = ", ")))
  
  # filter rows and recalculate the parameters
  dt <- dt %>% 
    filter(dt[[speed_sel]] <= max_flight_sp) %>% 
    mutate(
      Dist_consec = c(as.numeric(geodist(select(., Long, Lat), measure = "geodesic", sequential = TRUE)), NA),
      Time_consec = c(as.numeric(difftime(timestamp[-1], timestamp[-n()], units = "mins")), NA),
      Speed_consec = if (!gr_speed) Dist_consec / (Time_consec * 60) else NULL
    )
}

# drop the last row as distance, time, and Speed_consec (if available) is NA (no consecutive row)
dt  <- dt %>% 
  filter(row_number() <= n()-1)

# check the data time range
logger.info(paste0("Data range is from ", as.character(range(date(dt$timestamp)))[1], " to ", as.character(range(date(dt$timestamp)))[2],"\n"))
