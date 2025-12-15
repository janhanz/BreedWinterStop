# -------------------------------------------------------- #
# script for MoveApps App "Migration and stationary sites"
# -------------------------------------------------------- #

# load libraries
library('doFuture')
library('dplyr')
library('fpc')
library('future')
library('geodist')
library('lubridate')
library('move2')
library('purrr')
library('readr')
library('sf')
library('stringr')
library('tidyr')

rFunction = function(data, prep_class = "class", cap_status = NULL, nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
                     max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
                     near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30, ...) {

  # obtain bird_ID
  bird_ID <- as.character(unique(mt_track_id(data)))
  
  # prepare and export the form with parameter settings
  if (prep_class == "prep") {
    if (length(bird_ID) == 1) {
      stop("Data on a single individual were supplied, input file with parameter settings will not be created.", call. = FALSE)
    } else {    
      # convert NULL to NA
      cap_status <- if (is.null(cap_status)) NA else cap_status
      nest_coords <- if (is.null(nest_coords)) NA else paste0("Long = ", nest_coords[1], ", Lat = ", nest_coords[2])
      
      # input form with user set parameters (or default values if not changed) 
      input_form <- data.frame(ID = bird_ID, cap_status = cap_status, nest_coords = nest_coords, single_blk_merge = single_blk_merge, gr_speed = gr_speed, bursts_rec = bursts_rec,
                               max_flight_sp = max_flight_sp, dst_stat_gap = dst_stat_gap, time_stat_gap = time_stat_gap, centr_dist = centr_dist, near_blk_dist = near_blk_dist, near_stop_rec = near_stop_rec, 
                               near_stat_dist = near_stat_dist, clust_min_rec = clust_min_rec, br_win_min = br_win_min)
      
      # export the form
      write.csv(input_form, file = appArtifactPath(paste0("input_form.csv")), row.names = FALSE)
      logger.info("Input file with default parameters for each individual is ready. Please review the file.")
      logger.info("Terminating the App.")
      return(invisible(NULL))
      }
  # load the form
  } else {
    if (length(bird_ID) == 1) {
      # convert NULL to NA
      cap_status <- if (is.null(cap_status)) NA else cap_status
      nest_coords <- if (is.null(nest_coords)) NA else paste0("Long = ", nest_coords[1], ", Lat = ", nest_coords[2])
      
      # ignore the input form when supplied for the analysis of a single individual
      form_file_name <- getAuxiliaryFilePath("input_form")
      if (!is.null(form_file_name)) {
        logger.info("Provided csv file with parameter settings is ignored as redundant in a single individual analysis.")
      }
    } else {
      # load the input file (the form) with parameter values
      form_file_name <- getAuxiliaryFilePath("input_form")
      if (!is.null(form_file_name)) {
        input_form <- read_delim(form_file_name, delim = NULL, show_col_types = FALSE)
        # check if colnames match with names of parameters in App settings
        if (identical(c("ID", "cap_status", "nest_coords", "single_blk_merge", "gr_speed", "bursts_rec", "max_flight_sp", "dst_stat_gap", "time_stat_gap", "centr_dist", "near_blk_dist", "near_stop_rec", "near_stat_dist",
                        "clust_min_rec", "br_win_min"), names(input_form))) {
          logger.info("A csv file with parameter settings has been loaded.")
        } else {
          stop("The csv file (Input form) does not include all required columns!", call. = FALSE)
        }
        # split the form into the original variables provided in App settings
        list2env(input_form, envir = environment())
      } else {
        # stop when form is missing
        stop("csv file with parameter settings is needed for classification!", call. = FALSE)
      }
    }
  }
  
  # set workers for parallel computation
  if (length(unique(bird_ID)) > 1) {
    n_workers <- future::availableCores(omit = 1)
    # setting parallel processing strategy
    future::plan("cluster", workers = n_workers)
  } else {
    # setting sequential computing if a single individual is analysed
    future::plan("sequential")
  }
  
  # send all output to a log file
  study_id <- if(length(unique(mt_track_data(data)$study_id)) == 1) paste0("study_", unique(mt_track_data(data)$study_id)) else "multiple_studies"
  log_path <- appArtifactPath(paste0("Log_classif_", study_id, ".txt"))
  
  # open the log and run the computation
  {
    outcon <- file(log_path, open = "w")
    sink(outcon, type = "output")
    
    on.exit({
            sink(type = "output")
            closeAllConnections()
            }, add = TRUE)
    
    # parallel computation
    out <- foreach(b = seq_along(unique(bird_ID)), .options.future = list(seed = TRUE)) %dofuture% {
      iter_start <- Sys.time()
      
      cat("\n#####################################################################\n")
      logger.info(paste0("Classification of tracking data in ", bird_ID[b]))
      cat("#####################################################################\n\n")
      
      tryCatch({
        if (gr_speed[b] & !("ground_speed" %in% colnames(data))) {
          stop("Ground speed is not available in the data and hence cannot be used!", call. = FALSE)
        }
      
        if (is.na(cap_status[b]) & is.na(nest_coords[b])) {
          stop("Bird capture status or coordinates of the nest are required!", call. = FALSE)
        }
        
        if (!is.na(cap_status[b]) & !is.na(nest_coords[b])) {
          stop("Only one of nest coordinates or bird capture status are allowed!", call. = FALSE)
        }
        
        if (!is.na(cap_status[b])) {
          if (!(cap_status[b] %in% c("winter", "breed"))) {
            stop(paste0("Invalid input of the bird capture status! Only ‘winter’ or ‘breed’ are allowed."), call. = FALSE)
          }
        }
        
        if (bursts_rec[b] & !gr_speed[b]) {
          stop("Bursts can be preserved only when ground speed is used!", call. = FALSE)
        }
        
        if (clust_min_rec[b] < 2) {
          stop("Minimum number of records in a stopover block checked for ‘outliers’ is 2!", call. = FALSE)
        }
      }, error = function(e) stop(e))
      
      if(single_blk_merge[b]) {
        logger.info("Single-row blocks of migration or stationary records will be merged with the neighbouring blocks.")
      } else {
        logger.info("Single-row blocks of migration or stationary records will be preserved in the data.")
      }
      
      if(bursts_rec[b]) {
        logger.info("Bursts (i.e. very frequent records) are kept in the data.")
      } else {
        logger.info("Bursts (i.e. very frequent records) are not preserved.")
      }
      
      if(!is.na(nest_coords[b])) {
        nest_long <- as.numeric(str_extract(nest_coords[b], "(?<=Long = )[0-9.]*"))
        nest_lat = as.numeric(str_extract(nest_coords[b], "(?<=Lat = )[0-9.]*"))
      }
      
      # flag for additional grouping, default must be FALSE
      add_grouping <- FALSE
      
      ### load data
  
      # filtering and thinning of GPS records
      dt <- prepare_data(data %>% filter(.data[[mt_track_id_column(data)]] == bird_ID[b]), bursts_rec[b], gr_speed[b], max_flight_sp[b])
      
      {
        # statistics of the loaded data set regarding time intervals
        logger.info(paste0("Mean interval between records is ", round(mean(dt$Time_consec, na.rm = TRUE), 1), " mins"))
        logger.info(paste0("Median interval between records is ", round(median(dt$Time_consec, na.rm = TRUE), 1), " mins"))
        # maximum time gap between consecutive records in days
        logger.info(paste0("Maximum time gap between records is ", round(max(dt$Time_consec, na.rm = TRUE)/1440, 1), " days"))
        
        logger.info(paste0(" i.e., between ", as.character(dt$timestamp[which.max(dt$Time_consec)]), " and ", ifelse(which.max(dt$Time_consec) == nrow(dt), paste0(as.character(dt_sub$timestamp[nrow(dt_sub)]), " (which was, however, dropped as the last record with assigned NA consecutive timestamp)" ), as.character(dt$timestamp[which.max(dt$Time_consec) + 1]))))
        # print the start date and time, end date and time, and the length of the other time gaps > 10 days
        if (sum(dt$Time_consec > 14400) > 1) {
          logger.info("Other gaps of at least 10 days are present:")
          dt %>%
            st_drop_geometry() %>% 
            mutate(idx = row_number()) %>%
            filter(Time_consec > 14400, idx != which.max(Time_consec)) %>%
            transmute(
              start = as.character(timestamp),
              end   = as.character(dt$timestamp[idx + 1])
            ) %>% 
            drop_na() %>% 
            mutate(days = as.numeric(difftime(as.Date(end), as.Date(start)))) %>% 
            {
              logger.info(paste0(" between ", .$start, " and ", .$end, " (", .$days, " days)"), sep = "")
            }
        }
        
        # share of records with <= 30 min apart
        logger.info(paste0(round(sum(dt$Time_consec <= 30)/nrow(dt)*100, 1), " % of intervals between records is <= 30 min"))
        # share of records with >= 60 min apart
        logger.info(paste0(round(sum(dt$Time_consec >= 60)/nrow(dt)*100, 1), " % of intervals between records is >= 60 min"))
        # number of records at least 1 day apart
        if (sum(dt$Time_consec >= 60 * 24) == 1) {
          logger.info(paste0(sum(dt$Time_consec >= 60 * 24), " record is more than 1 day apart"))
        } else {
          logger.info(paste0(sum(dt$Time_consec >= 60 * 24), " records are more than 1 day apart"))
        }
      }
      
      
      # assign ground speed or speed between consecutive records to the variable Speed
      if (gr_speed[b]) {
          dt$Speed <- as.numeric(dt$ground_speed)
      } else {
        dt <- dt %>% 
          rename(Speed = "Speed_consec")
      }
      
      if (gr_speed[b]) {
        logger.info("Data on ground speed are used.\n")
      } else {
        logger.info("Data on speed between consecutive records are used.\n")
      }
      
      ###
      # k-means clustering of the data
      
      # distinguish two clusters (slow vs fast records)
      set.seed(10)
      km_sp <- kmeans(scale(dt$Speed), centers = 2)
      
      dt$Block_type <- km_sp$cluster
      
      # calculate median of speed and Q1 and Q3 in the clusters
      med_sp_cl <- dt %>% 
        st_drop_geometry() %>% 
        group_by(Block_type) %>% 
        summarise(med_sp = median(Speed),
                  Q1_sp = quantile(Speed, 0.25),
                  Q3_sp = quantile(Speed, 0.75),
                  .groups = "keep")
      
      med_sp_cl1 <- med_sp_cl %>% filter(Block_type == 1) %>% pull(med_sp) %>% as.numeric()
      Q1_Q3_sp_cl1 <- med_sp_cl %>% ungroup() %>% filter(Block_type == 1) %>% select(c(Q1_sp, Q3_sp)) %>% as.numeric()
      med_sp_cl2 <- med_sp_cl %>% filter(Block_type == 2) %>% pull(med_sp) %>% as.numeric()
      Q1_Q3_sp_cl2 <- med_sp_cl %>% ungroup() %>% filter(Block_type == 2) %>% select(c(Q1_sp, Q3_sp)) %>% as.numeric()
      
      # assign "migration" and "stationary" to the clusters
      if (med_sp_cl1 < med_sp_cl2) {
        dt <- dt %>% 
          mutate(Block_type = recode(Block_type, "1" = "stationary", "2" = "migration"))
          logger.info(paste0("First block has median (Q1, Q3) of speed ", round(med_sp_cl1, 2)," (", round(Q1_Q3_sp_cl1[1], 2), ", ",round(Q1_Q3_sp_cl1[2], 2), ") m/s."))
          logger.info(paste0("Second block has median (Q1, Q3) of speed ", round(med_sp_cl2, 2)," (", round(Q1_Q3_sp_cl2[1], 2), ", ",round(Q1_Q3_sp_cl2[2], 2), ") m/s."))
    	    logger.info("Stationary and migration clusters were defined.\n")
      } else {
        dt <- dt %>% 
          mutate(Block_type = recode(Block_type, "1" = "migration", "2" = "stationary"))
          logger.info(paste0("First block has median (Q1, Q3) of speed ", round(med_sp_cl1, 2)," (", round(Q1_Q3_sp_cl1[1], 2), ", ",round(Q1_Q3_sp_cl1[2], 2), ") m/s."))
          logger.info(paste0("Second block has median (Q1, Q3) of speed ", round(med_sp_cl2, 2)," (", round(Q1_Q3_sp_cl2[1], 2), ", ",round(Q1_Q3_sp_cl2[2], 2), ") m/s."))
          logger.info("Migration and stationary clusters were defined.\n")
      }
    
      # print the number of migration single-row records
      nrow_migr <- dt %>% 
        filter((Block_type == "migration" & lag(Block_type == "stationary") & lead(Block_type) == "stationary") |
                 (Block_type == "migration" & is.na(lag(Block_type)) & lead(Block_type) == "stationary") |
                 (Block_type == "migration" & lag(Block_type) == "stationary" & is.na(lead(Block_type)))) %>% 
        nrow()
      
      logger.info(paste0(nrow_migr, " migration records are single-row."))
      
      # print the number of stationary single-row records
      nrow_stat <- dt %>% 
        filter((Block_type == "stationary" & lag(Block_type == "migration") & lead(Block_type) == "migration") |
                 (Block_type == "stationary" & is.na(lag(Block_type)) & lead(Block_type) == "migration") |
                 (Block_type == "stationary" & lag(Block_type) == "migration" & is.na(lead(Block_type)))) %>% 
        nrow()
      
      logger.info(paste0(nrow_stat, " stationary records are single-row.\n"))
    
    
      # replace single-row stationary records by migration and then single-row migration records by stationary
      if (single_blk_merge[b]) {
        dt <- dt %>% 
          mutate(Block_type = if_else((Block_type == "stationary" & lag(Block_type == "migration") & lead(Block_type) == "migration") |
                                        (Block_type == "stationary" & is.na(lag(Block_type)) & lead(Block_type) == "migration") |
                                        (Block_type == "stationary" & lag(Block_type) == "migration" & is.na(lead(Block_type))), "migration", Block_type)) %>% 
          mutate(Block_type = if_else((Block_type == "migration" & lag(Block_type == "stationary") & lead(Block_type) == "stationary") |
                                        (Block_type == "migration" & is.na(lag(Block_type)) & lead(Block_type) == "stationary") |
                                        (Block_type == "migration" & lag(Block_type) == "stationary" & is.na(lead(Block_type))), "stationary", Block_type))
      }
      
      ### run a loop:
      # 1st step - classification and searching for near blocks for additional grouping of stationary records
      # 2nd step - re-runs classification if additional grouping was needed
      # 3rd & 4th (optionally used) - additional grouping applied once more, as some blocks may be still close apart after the first/second reclassification
      
      for (g in 1:4) {
        ### reclassify records into stationary and migration if additional grouping is needed
        if (add_grouping) {
          dt <- dt %>% 
            mutate("row_nb" = as.numeric(rownames(.))) %>% 
            mutate(Block_type = case_when(row_nb %in% stat_rows ~ "stationary",
                                          !row_nb %in% stat_rows ~ "migration"))
          logger.info("Reclassifying records into stationary and migration.\n")
        }
        
        # pre-final data set
        # keep ground speed in the data set (if available) even when it was not used for the classification
        #   it can be used for additional filtering of records in breeding and wintering grounds in the final output
        dt_export <- dt %>% 
          select(any_of(c("event_id", "timestamp", "Lat", "Long", "Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Speed", "ground_speed", "Dist_consec", "Time_consec", "Block_type"))) %>% 
          {if (gr_speed[b]) select(.,-ground_speed) else .} %>% 
          mutate(Block_nr = cumsum(Block_type != lag(Block_type, default = first(Block_type)))) %>% 
          mutate("row_nb" = as.numeric(rownames(.))) %>% 
          group_by(Block_nr) %>% 
          mutate(Block_size = block_size_fun(pick(everything()))) %>% 
          data.frame()
        
        ### divide stationary blocks when time & space gaps are present
        # records after which a new block should start
        block_div <- dt_export %>% 
          filter(Block_type == "stationary" & Dist_consec > dst_stat_gap[b]*1000 & Time_consec > time_stat_gap[b]*60) %>% 
          pull(row_nb)
        
        if (length(block_div) > 0 & g == 1) { # print the info on gapped blocks only initially
          logger.info(paste0("Stationary blocks have gaps > ", dst_stat_gap[b], " km (", paste(round(dt_export[block_div,]$Dist_consec/1000,0), collapse = ", "), " km) and > ", time_stat_gap[b], " hours (", paste(round(dt_export[block_div,]$Time_consec/60,0), collapse = ", "), " hours) at row(s): ", paste(block_div, collapse = ", "),". Adding groups of records."))
        }
        
        # add +1 to Block_nr where needed and eventually recalculate Block_size
        if (length(block_div) > 0) {
          for (i in seq_along(block_div)) {
            dt_export <- dt_export %>%
              mutate(Block_nr = if_else(dt_export$row_nb > block_div[i], Block_nr +1, Block_nr))
            if (i == length(block_div)) {
              dt_export <- dt_export %>% 
                group_by(Block_nr) %>% 
                mutate(Block_size = block_size_fun(pick(everything()))) %>% 
                ungroup()
            }
          }
        }
        
        # re-cluster stationary blocks to reduce the presence of large incoherent clusters
        
        if (g == 1) {
          # distinguish the block types, migration will be 0, which is reserved for "outliers" in the dbscan
          dt_export <- dt_export %>% 
            mutate(cls = case_when(Block_type == "migration" ~ 0,
                                   Block_type == "stationary" ~ 99)
            )
          
          # numbers of stationary blocks
          block_stat_nr <- dt_export %>% 
            filter(Block_type == "stationary") %>% 
            select(Block_nr) %>% 
            distinct()
          
          for (i in seq_len(nrow(block_stat_nr))) {
            dt_export_sub <- dt_export %>% 
              filter(Block_nr == block_stat_nr$Block_nr[i])
            
            dist_mat <- geodist(x = dt_export_sub %>% select("Long", "Lat"), measure = "cheap", paired = TRUE, quiet = TRUE)/1000
            
            cl <- dbscan(dist_mat, eps = near_stat_dist[b], MinPts = clust_min_rec[b], method = "dist")
            
            # set cluster number to 1 (instead of 0) for single-record blocks
            # this prevents changing single-record blocks to migration
            if (nrow(dt_export_sub) == 1) {
              cl$cluster <- 1
            }
            
            # unique cluster numbers between blocks
            dt_export <- dt_export %>%
              mutate(cls = replace(cls, Block_nr == block_stat_nr$Block_nr[i], cl$cluster*i))
          }
          
          logger.info("Clustering of stationary records done.")
          
          # assign migration status to the "outliers", re-assign numbers to the blocks and recalculate block size
          dt_export <- dt_export %>% 
            mutate(Block_type = case_when(cls == 0 ~ "migration", TRUE ~ Block_type)) %>% 
            mutate(Block_nr = cumsum(cls != lag(cls, default = first(cls)))) %>% 
            select(-cls) %>% 
            group_by(Block_nr) %>% 
            mutate(Block_size = block_size_fun(pick(everything()))) %>% 
            ungroup()
        }
        
        
        # pre-final summary output table
        tab_out <- dt_export %>%
          mutate("row_nb" = as.numeric(rownames(.))) %>% 
          st_as_sf(., coords = c("Long", "Lat"), crs = "EPSG:4326", remove = FALSE) %>% 
          group_by(Block_nr, Block_type) %>%
          summarise(Start_date = min(timestamp),
                    End_date = max(timestamp),
                    First_row = min(row_nb),
                    Last_row = max(row_nb),
                    Days = floor(as.numeric(difftime(End_date, Start_date, units = "days"))),
                    Hours = floor(as.numeric(difftime(End_date, Start_date, units = "hours"))),
                    Minutes = floor(as.numeric(difftime(End_date, Start_date, units = "mins"))),
                    Block_size = max(Block_size),
                    latlong = st_union(geometry),
                    .groups = "keep") %>% 
          st_centroid() %>%
          ungroup()
        
        # distance to the following stationary centroid in km (between stationary centroids only)
        tab_out_stat_sub <- tab_out %>% 
          filter(Block_type == "stationary")
        
        tab_out_stat_sub_coords <- tab_out_stat_sub %>% 
          st_coordinates() %>%
          data.frame() %>% 
          rename(Long = "X", Lat = "Y")
        
        distances_stat_sub_all <- geodist(tab_out_stat_sub_coords[c(-nrow(tab_out_stat_sub_coords)), ], tab_out_stat_sub_coords[-1, ], quiet = TRUE)
        
        distances_stat_sub <- as.numeric(diag(distances_stat_sub_all))/1000
        
        dist_stat <- data.frame(Block_nr = tab_out_stat_sub$Block_nr, Dist_next_stat = round(c(distances_stat_sub, NA), 0))
        
        # summary output, with coordinates and distances between the centroids of stationary records 
        tab_sum_stat_migr <- left_join(tab_out, dist_stat, by = "Block_nr") %>% 
          data.frame() %>% 
          select(-"latlong") %>% 
          mutate(Long_centr = st_coordinates(tab_out)[,1], Lat_centr = st_coordinates(tab_out)[,2])
        
        # preserve the original tab_sum_stat_migr output
        if (g == 1) {
          tab_sum_stat_migr_orig <- tab_sum_stat_migr
        }
        
        ###
        # rules for distinguishing "breeding" and "winering":
        # the first block is classified using cap_status value or nest_coords defined initially by the user
        # other blocks in the same cluster are then classified into the same category as in the first block
        # rest of the blocks are assigned to the other category (i.e. that one not assigned to cap_status)
        # blocks < br_win_min days are post hoc assigned to stopover category
        # unsuccessful (short) breeding attempts, if any, are now masked as stopovers
        
        # filter stationary blocks
        tab_out_stat <- tab_out %>%
          filter(Block_type == "stationary")
        
        tab_out_stat_coords <- tab_out_stat %>% 
          st_coordinates() %>%
          data.frame() %>% 
          rename(Long = "X", Lat = "Y")
        
        # recalculate distances between stationary blocks when very short stopovers were dropped
        distances_stat_all <- geodist(tab_out_stat_coords[c(-nrow(tab_out_stat_coords)), ], tab_out_stat_coords[-1, ], quiet = TRUE)
        distances_stat <- as.numeric(diag(distances_stat_all))/1000
        
        dist_stat2 <- data.frame(Block_nr = tab_out_stat$Block_nr, Dist_next_stat = round(c(distances_stat, NA), 0))
        
        # summary output, with coordinates and distances between the centroids of stationary records (without very short stopovers)
        tab_sum_stat <- left_join(tab_out_stat, dist_stat2, by = "Block_nr") %>% 
          data.frame() %>% 
          select(-"latlong") %>% 
          mutate(Long_centr = st_coordinates(tab_out_stat)[,1], Lat_centr = st_coordinates(tab_out_stat)[,2])
        
        # assign status to the blocks
        if (is.na(nest_coords[b])) {
          if (cap_status[b] == "winter") {
            status_order <- c("winter", "breed")
          } else {
            status_order <- c("breed", "winter")
          }
        }
        
        ### k-means clustering, only two clusters (winter or breed)
        
        # select coordinates only
        dt_kmeans <- tab_sum_stat %>% 
          select(Long_centr, Lat_centr)
        
        # K-means clustering analysis, skipped if only 2 stationary blocks exist
        if (nrow(dt_kmeans) >2) {
          set.seed(10)
          kmeans_res <- kmeans(dt_kmeans, centers = 2)
          dt_kmeans$Block_class <- as.character(kmeans_res$cluster)
          
          tab_sum_stat <- left_join(tab_sum_stat, dt_kmeans, by = c("Long_centr", "Lat_centr")) %>% 
            mutate_if(is.character, coalesce, "stopover")
          
          # assign status to the blocks
          if (!is.na(nest_coords[b])) {
            nest_centr <- as.numeric(geodist(data.frame(kmeans_res$centers), data.frame(Long = nest_long, Lat = nest_lat), measure = "geodesic", quiet = TRUE)/1000)
            if (nest_centr[1] < nest_centr[2]) {
              status_order <- c("breed", "winter")
            } else {
              status_order <- c("winter", "breed")
            }
          }
          
          # assign breed or winter status to the first block
          tab_sum_stat <- tab_sum_stat %>%
            mutate(Block_class = if_else(Block_class == first(Block_class), status_order[1], status_order[2]))
          
        } else {
          if (nrow(dt_kmeans) == 2) {
            # centroids very close (i.e. < centr_dist in km) to each other suggest either wintering or breeding was not detected in the data set 
            if (max(as.numeric(geodist(dt_kmeans %>% select("Long_centr", "Lat_centr"), measure = "geodesic"))/1000) < centr_dist[b]) {
              logger.info(paste0("Breeding and wintering grounds very close to each other (i.e. < ", centr_dist[b], " km)."))
              dt_kmeans$Block_class <- rep(status_order[1], 2)
            } else {
              dt_kmeans$Block_class <- status_order
            }
            tab_sum_stat <- left_join(tab_sum_stat, dt_kmeans, by = c("Long_centr", "Lat_centr")) %>% 
              mutate_if(is.character, coalesce, "stopover")
          } else {
            stopifnot("Only one block detected, check manually whether breeding coordinates are near." = is.na(nest_coords[b]))
            dt_kmeans$Block_class <- status_order[1]
            tab_sum_stat <- left_join(tab_sum_stat, dt_kmeans, by = c("Long_centr", "Lat_centr")) %>% 
              mutate_if(is.character, coalesce, "stopover")
          }
          
        }
        
        # blocks shorter than br_win_min days are assigned to stopover
        tab_sum_stat <- tab_sum_stat %>%
          mutate(Block_class = if_else(Days < br_win_min[b], "stopover", Block_class))
        
        ### final assignment of "migration" and "stationary" in the original data set
        
        # merge summary data including breeding, wintering and stopover with data including migration
        #   replace NAs by "migration" in Block_class column
        tab_sum_stat_status <- left_join(tab_sum_stat_migr, tab_sum_stat %>% select(c("Block_nr", "Block_class")), by  = "Block_nr") %>% 
          mutate_if(is.character, coalesce, "migration")
        
        # expand the summary to the length of the original data set and obtain the status of each record
        expand_tab_sum_stat <- tab_sum_stat_status %>%
          rowwise() %>% 
          mutate(rowID = list(seq(First_row, Last_row))) %>% 
          unnest(rowID) %>% 
          data.frame()
        
        # replace "stationary" and "migration" by their final assignment
        dt_export$Block_type <- expand_tab_sum_stat$Block_type
        # add a column describing the seasonal status
        dt_export$Block_class <- expand_tab_sum_stat$Block_class
        # replace groups by those available in summary
        dt_export$Block_nr <- expand_tab_sum_stat$Block_nr
        
        # drop unnecessary columns
        dt_export <- dt_export %>% 
          select(-c(row_nb, Time_consec))
        
        # preserve the original dt_export output
        if (g == 1) {
          dt_export_orig <- dt_export
        }
        
        ### additional grouping of stationary blocks
        
        # preserve the original tab_sum_stat output
        if (g == 1) {
          tab_sum_stat_orig <- tab_sum_stat
        }
        
        # new column classifying the blocks into groups "gr" (when less than near_blk_dist apart) or separate blocks "sep"
        tab_sum_stat$grp <- ifelse(tab_sum_stat$Dist_next_stat >= near_blk_dist[b] | is.na(tab_sum_stat$Dist_next_stat), "sep", "gr")
        
        # run additional grouping only when needed
        if (any(tab_sum_stat$grp=="gr")) {
          # flag for additional grouping
          add_grouping <- TRUE
          logger.info("Creating additional groups.")
          
          # create a vector with numbers used for grouping of consecutive blocks < 20 km apart
          k <- 1
          group <- vector("list", nrow(tab_sum_stat)-1)
          for (i in seq_len(nrow(tab_sum_stat))) {
            if (i == 1) { # first row
              group[[i]] <- k
            } else {
              if (i == nrow(tab_sum_stat)) { # last row
                if (tab_sum_stat$Dist_next_stat[[i-1]] < near_blk_dist[b]) {
                  group[[i]] <- k
                } else {
                  group[[i]] <- k + 1
                }
              } else { # all but first and last rows
                if (tab_sum_stat$grp[[i]] == "gr" & (tab_sum_stat$grp[[i]] == tab_sum_stat$grp[[i-1]])) {
                  group[[i]] <- k
                } else {
                  if (tab_sum_stat$grp[[i]] == "sep" & (tab_sum_stat$grp[[i]] == tab_sum_stat$grp[[i-1]])) {
                    k <- k+1
                    group[[i]] <- k
                  } else {
                    if (tab_sum_stat$grp[[i-1]] == "sep") {
                      k <- k+1
                      group[[i]] <- k
                    } else {
                      group[[i]] <- k
                      k <- k+1
                    }
                  }
                }
              }
            }
          }
          
          tab_sum_stat$new_group <- unlist(group)
          
          
          tab_sum_stat_new <- tab_sum_stat %>%
            group_by(new_group) %>% 
            summarise(Block_type = unique(Block_type),
                      Start_date = min(Start_date),
                      End_date = max(End_date),
                      First_row = min(First_row),
                      Last_row = max(Last_row),
                      Days = floor(as.numeric(difftime(End_date, Start_date, units = "days"))),
                      Hours = floor(as.numeric(difftime(End_date, Start_date, units = "hours"))),
                      Minutes = floor(as.numeric(difftime(End_date, Start_date, units = "mins"))),
                      .groups = "keep") %>%
            ungroup()
          
          # extract row numbers for stationary blocks
          stat_rows <- map2(tab_sum_stat_new$First_row, tab_sum_stat_new$Last_row, ~seq(.x, .y)) %>% unlist()
        } else {
          add_grouping <- FALSE
          
          # print a message when the mean longitude of the breeding centroids is smaller than of wintering centroids
          # cases when either breeding or wintering ground coordinate are not present are omitted
          if (all(c("winter","breed") %in% tab_sum_stat$Block_class)) {
            if (mean(tab_sum_stat[tab_sum_stat$Block_class == "breed",]$Long_centr) < mean(tab_sum_stat[tab_sum_stat$Block_class == "winter",]$Long_centr)) {
              logger.info("Warning, the mean longitude of breeding ground coordinates is smaller than of wintering grounds!")
              logger.info("It would mean that the bird migrates to the west for breeding.\n")
            }
          }
          
          if (all(tab_sum_stat$Block_class == "breed")) { 
            logger.info("Only breeding ground was found! The bird likely did not migrate anywhere.\n")
          }
          
          break # terminate the for loop when additional grouping is not needed
        }
      } # end of for loop
      
      
      # reclassify the Block_class in the original tab_sum_stat using the updated classification
      tab_sum_stat_sep <- tab_sum_stat_orig %>%
        rowwise() %>%
        mutate(Block_class = case_when(
          any(tab_sum_stat$First_row <= First_row & tab_sum_stat$Last_row >= Last_row) ~ tab_sum_stat$Block_class[which(tab_sum_stat$First_row <= First_row & tab_sum_stat$Last_row >= Last_row)[1]],
          TRUE ~ Block_class
        )) %>%
        ungroup() %>% 
        data.frame()
      
      # merge summary data including breeding, wintering and stopover with data including migration
      #   replace NAs by "migration" in Block_class column
      tab_sum_stat_status_sep <- left_join(tab_sum_stat_migr_orig, tab_sum_stat_sep %>% select(c("Block_nr", "Block_class")), by  = "Block_nr") %>% 
        mutate_if(is.character, coalesce, "migration")
      
      # expand the summary to the length of the original data set and obtain the status of each record
      expand_tab_sum_stat_sep <- tab_sum_stat_status_sep %>%
        rowwise() %>% 
        mutate(rowID = list(seq(First_row, Last_row))) %>% 
        unnest(rowID) %>% 
        data.frame()
      
      # replace "stationary" and "migration" by their final assignment
      dt_export_orig$Block_type <- expand_tab_sum_stat_sep$Block_type
      # add a column describing the seasonal status
      dt_export_orig$Block_class <- expand_tab_sum_stat_sep$Block_class
      # replace groups by those in summary
      dt_export_orig$Block_nr <- expand_tab_sum_stat_sep$Block_nr
      # rename Speed to either ground_speed (ground speed) or Speed_consec (speed between consecutive records)
      dt_export_orig <- dt_export_orig %>% 
        rename_with(~ if (gr_speed[b]) {"ground_speed"} else {"Speed_consec"}, .cols = "Speed")
      
      dt_export_sep <- dt_export_orig
      
      
      ###
      # re-cluster only the stopover blocks to detect possibly separate blocks and the "outliers"
      
      {
        # distinguish migration from the other classes, and breed from winter; stopovers can have the same number as breed or winter
        dt_export_sep <- dt_export_sep %>% 
          mutate(cls = case_when(Block_type == "migration" ~ 0,
                                 Block_class == "stopover" | Block_class == "breed" ~ 200,
                                 Block_class == "winter" ~ 100)
          )
        
        tab_sum_rev <- tab_sum_stat_sep %>% 
          filter(Block_class == "stopover")
        
        if (nrow(tab_sum_rev) != 0) {
          for (i in seq_len(nrow(tab_sum_rev))) {
            dt_rev_sub <- dt_export_sep %>% 
              filter(Block_nr == tab_sum_rev$Block_nr[i])
            
            dist_mat <- geodist(x = dt_rev_sub %>% select("Long", "Lat"), measure = "cheap", paired = TRUE, quiet = TRUE)/1000
            
            cl <- dbscan(dist_mat, eps = near_stop_rec[b], MinPts = clust_min_rec[b], method = "dist")
            
            # set cluster number to 1 (instead of 0) for single-record blocks
            # this prevents changing single-record blocks to migration
            if (nrow(dt_rev_sub) == 1) {
              cl$cluster <- 1
            }
            
            # unique cluster numbers between blocks
            dt_export_sep <- dt_export_sep %>%
              mutate(cls = replace(cls, Block_nr == tab_sum_rev$Block_nr[i], cl$cluster*i))
          }
        }
      }
      
      # assign migration status to the "outliers", re-assign the numbers to the blocks and recalculate all the metrics
      dt_export_sep_rev <- dt_export_sep %>% 
        mutate(cls = case_when(Block_class != "migration" ~ cls*(Block_nr+1), TRUE ~ cls)) %>% 
        mutate(Block_type = case_when(cls == 0 ~ "migration", TRUE ~ Block_type)) %>% 
        mutate(Block_class = case_when(cls == 0 ~ "migration", TRUE ~ Block_class)) %>% 
        mutate(Block_nr = cumsum(cls != lag(cls, default = first(cls)))) %>% 
        select(-cls) %>% 
        mutate("row_nb" = as.numeric(rownames(.))) %>% 
        group_by(Block_nr) %>% 
        mutate(Block_size = block_size_fun(pick(everything()))) %>% 
        data.frame()
      
      # print the number of migration single-row records after re-analyzing stopovers
      nrow_migr_rev <- dt_export_sep_rev %>% 
        filter((Block_type == "migration" & lag(Block_type == "stationary") & lead(Block_type) == "stationary") |
                 (Block_type == "migration" & is.na(lag(Block_type)) & lead(Block_type) == "stationary") |
                 (Block_type == "migration" & lag(Block_type) == "stationary" & is.na(lead(Block_type)))) %>% 
        nrow()
      
      logger.info(paste0(nrow_migr_rev, " migration records are single-row."))
    
      # print the number of stationary single-row records after re-analyzing stopovers
      nrow_stat_rev <- dt_export_sep_rev %>% 
        filter((Block_type == "stationary" & lag(Block_type == "migration") & lead(Block_type) == "migration") |
                 (Block_type == "stationary" & is.na(lag(Block_type)) & lead(Block_type) == "migration") |
                 (Block_type == "stationary" & lag(Block_type) == "migration" & is.na(lead(Block_type)))) %>% 
        nrow()
      
      logger.info(paste0(nrow_stat_rev, " stationary records are single-row.\n"))
      
      # recalculate the metrics in summary output
      tab_sum_stat_sep_rev <- dt_export_sep_rev %>%
        mutate("row_nb" = as.numeric(rownames(.))) %>% 
        filter(Block_type == "stationary") %>% 
        st_as_sf(., coords = c("Long", "Lat"), crs = "EPSG:4326", remove = FALSE) %>% 
        group_by(Block_nr, Block_type, Block_class) %>%
        summarise(Start_date = min(timestamp),
                  End_date = max(timestamp),
                  First_row = min(row_nb),
                  Last_row = max(row_nb),
                  Days = floor(as.numeric(difftime(End_date, Start_date, units = "days"))),
                  Hours = floor(as.numeric(difftime(End_date, Start_date, units = "hours"))),
                  Minutes = floor(as.numeric(difftime(End_date, Start_date, units = "mins"))),
                  Block_size = max(Block_size),
                  latlong = st_union(geometry),
                  .groups = "keep") %>% 
        st_centroid() %>% 
        ungroup() %>% 
        mutate(Long = st_coordinates(.)[,1], Lat = st_coordinates(.)[,2]) %>% 
        mutate(Dist_next_stat = c(round(diag(geodist(select(., Long, Lat) %>% slice(-nrow(.)), select(., Long, Lat) %>% slice(-1), measure = "geodesic", quiet = TRUE))/1000,0), NA) %>% 
                 round(., 1)) %>% 
        rename(Long_centr = "Long", Lat_centr = "Lat") %>% 
        relocate(c(Block_nr, Block_type, Block_class, Block_size, Dist_next_stat)) %>% 
        data.frame() %>% 
        select(-latlong)
      
      dt_export_sep_rev <- dt_export_sep_rev %>% 
        select(-c(row_nb, geometry))
      
      if (dt_export_sep_rev %>% 
          filter(Block_class == "breed" & Month %in% c(7,8)) %>% 
          nrow > 1) {
        logger.info("Breeding blocks appeared in months of July or August.")
        logger.info("Check if this classification is relevant, i.e. the bird stayed in the breeding area for summer.")
        logger.info("Otherwise, a long stopover during migration (pre-wintering) might be mistaken for breeding.")
      }
      
      # print a warning if another breeding grounds, located more than 500 km apart, were identified in a given year
      for (y in unique(year(tab_sum_stat_sep_rev$Start_date))) {
        if (tab_sum_stat_sep_rev %>% 
            filter(year(Start_date) == y & Block_class == "breed") %>% 
            nrow == 0) {
          next
        } else {
        
        breed_sub <- tab_sum_stat_sep_rev %>% 
          filter(year(Start_date) == y & Block_class == "breed") %>% 
          st_as_sf(., coords = c("Long_centr", "Lat_centr"), crs = "EPSG:4326", remove = FALSE) %>% 
          group_by(Block_nr) %>%
          summarise(latlong = st_union(geometry)) %>% 
          st_centroid() %>%
          #ungroup() %>% 
          mutate(Long = st_coordinates(.)[,1],
                 Lat = st_coordinates(.)[,2]) %>% 
          st_drop_geometry()
        
        # distances between breeding blocks (in km)
        dst_blk <- geodist(x = breed_sub %>% select(Long, Lat), measure = "geodesic", sequential = TRUE, quiet = TRUE)/1000
        
        if (any(dst_blk > 500)) {
          # coordinates of the centroid of wintering cluster in K-means
          win_coords <- t(kmeans_res$centers[status_order == "breed", ]) %>% 
            data.frame() %>% 
            rename(Long = "Long_centr", Lat = "Lat_centr")
          
          # coordinates of the centroids of breeding block > 500 km apart, and of the following breeding block
          breed500_coords <- breed_sub[c(which(dst_blk > 500), which(dst_blk > 500)+1),] %>% 
            select(Long, Lat)
          
          # distance between centroids of wintering cluster in K-means and centroids of breeding block > 500, and the following breeding block
          dst_to_win <- geodist(win_coords, breed500_coords, measure = "geodesic", quiet = TRUE)/1000
          
          if (dst_to_win[1] > dst_to_win[2]) {
            long_blk_date <- tab_sum_stat_sep_rev %>% 
              filter(Block_nr == breed_sub[which(dst_blk > 500)+1,]$Block_nr & year(Start_date) == y) %>% 
              slice(1) %>% 
              select(Start_date) %>% 
              pull()
            logger.info(paste0("Breeding block starting on ", as.character(long_blk_date), " may be rather a stopover during migration, or pre-wintering grounds. Check the output!"))
          } else {
            long_blk_date <- tab_sum_stat_sep_rev %>% 
              mutate(bks = case_when(Block_class == "breed" ~ 0,
                                     Block_class != "breed" ~ 99)) %>% 
              filter(Block_nr <= breed_sub[which(dst_blk > 500),]$Block_nr & bks == 0 & year(Start_date) == y) %>% 
              slice(1) %>% 
              select(Start_date) %>% 
              pull()
            logger.info(paste0("Breeding block starting on ", as.character(long_blk_date), " may be rather a stopover. Check the output!"))
          }
        }
        }
      }
      
      # add classification into the original (but filtered) data set
      data_out <- data %>%
        filter(event_id %in% as.numeric(dt_export_sep_rev$event_id)) %>%
        left_join(dt_export_sep_rev %>% select(event_id, Block_class), by = "event_id") %>%
        rename(class_rec = Block_class)
      
      # add bird ID into the summary output and change column order
      tab_sum_stat_sep_rev <- tab_sum_stat_sep_rev %>% 
        mutate(Bird_ID = bird_ID[b], .before = Block_nr) %>% 
        select("Bird_ID", "First_row", "Last_row", "Block_nr", "Block_type", "Block_class", "Block_size", "Dist_next_stat", "Start_date", "End_date", "Days", "Hours", "Minutes", "Long_centr", "Lat_centr")
      
      
      # add bird ID, rename some columns, and change order of the columns
      dt_export_sep_rev <- dt_export_sep_rev %>% 
        select(-c(event_id)) %>% 
        mutate(Bird_ID = bird_ID[b], .before = timestamp) %>% 
        rename(any_of(c(Speed_gr = "ground_speed", Timestamp = "timestamp"))) %>% 
        select(any_of(c("Bird_ID", "Block_nr", "Block_type", "Block_class", "Block_size", "Dist_consec", "Speed_consec", "Speed_gr", "Year", "Month", "Day", "Hour", "Minute", "Second", "Timestamp", "Lat", "Long")))
    
      # write the final output and summary output
      write.csv(tab_sum_stat_sep_rev, file = appArtifactPath(paste0("Classification_records_summary_", bird_ID[b], ".csv")), row.names = FALSE)
      write.csv(dt_export_sep_rev, file = appArtifactPath(paste0("Classification_records_", bird_ID[b], ".csv")), row.names = FALSE)
      
      iter_end <- Sys.time()
      
      # report runtime
      cat("\n")
      logger.info(paste0("Runtime: ", round(difftime(iter_end, iter_start, units = "s"), 3), " secs\n"))
      
      return(data_out)
    }
  }
  # stack the output data sets
  out_all <- mt_stack(out)
  return(out_all)
}



### helper functions ----

# data preparation
prepare_data <- function(data, bursts_rec, gr_speed, max_flight_sp) {
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
    select(any_of(c("event_id", "Long", "Lat", "timestamp", "ground_speed", "gps_hdop", "gps_satellite_count", bird_ID = attr(data, "track_id_column")))) %>% 
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
    
    for (i in seq.int(2, nrow(dt_sub))) {
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
    
    # share of original data left after data thinning
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
  
  return(dt)
}


# calculation of the maximum distances in groups of migration and stationary records
block_size_fun <- function(x) {
  bbox <- st_bbox(x$geometry)
  rec_dist_geom <- x %>% 
    filter(Long < bbox$xmin+0.001 | Long > bbox$xmax-0.001 | Lat < bbox$ymin+0.001 | Lat > bbox$ymax-0.001)
  # maximum migration distance (in km)
  max_migr_dst <- max(geodist(x = rec_dist_geom %>% select("Long", "Lat"), measure = "cheap", paired = TRUE, quiet = TRUE))/1000
  return(max_migr_dst)
}
