# data including a single individual
test_data <- test_data("Input_Curlew.rds")

test_that("classification column added to the output", {
  actual <- rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = NULL,
                      single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE, max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3,
                      centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30)
  expect_true("class_rec" %in% names(actual))
})


test_that("output is a valid move2 object", {
  actual <- rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
                      max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
                      near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30)
  expect_true(move2::mt_is_move2(actual))
})


test_that("no duplicate coordinates in the output", {
  actual <- rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
                      max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
                      near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30)
  expect_equal(actual %>% select(geometry, timestamp) %>% n_distinct(), nrow(actual))
})


test_that("missing input or invalid input combinations", {
  
  # capture status or nest coordinates needed
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = NULL, nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30),
    "Bird capture status or coordinates of the nest are required!"
  )
  
  # both capture status and nest coordinates supplied
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = c(Long = 2.3593, Lat = 48.8415), single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30),
    "Only one of nest coordinates or bird capture status are allowed!"
  )
  
  # bird capture status misspelled or wrong
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "wintering", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30),
    paste0("Invalid input of the bird capture status! Only ‘winter’ or ‘breed’ are allowed.")
  )
  
  # bursts not relevant
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = TRUE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30),
    "Bursts can be preserved only when ground speed is used!"
  )
  
  # MinPts parameter in DBSCAN too low
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000), prep_class = "class", cap_status = "winter", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 1, br_win_min = 30),
    "Minimum number of records in a stopover block checked for ‘outliers’ is 2!"
  )
  
  # ground speed selected but not available
  expect_error(
    rFunction(data = test_data %>% dplyr::slice(1:3000) %>% select(-any_of("ground_speed")), prep_class = "class", cap_status = "winter", nest_coords = NULL, single_blk_merge = TRUE, gr_speed = TRUE, bursts_rec = FALSE,
              max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3, centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, 
              near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30),
    "Ground speed is not available in the data and hence cannot be used!"
  )
})

# data including multiple individuals
test_data_mult <- test_data("Input_Curlews.rds")


test_that("Input form prepared", {
  actual <- rFunction(data = test_data_mult %>% dplyr::slice(c(1:3000, 69124:72124)), prep_class = "prep", cap_status = "winter", nest_coords = NULL,
                      single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE, max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3,
                      centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30)
  expect_true(file.exists(appArtifactPath(paste0("Input_form.csv"))))
})

test_that("Input form successfully loaded", {
  actual <- rFunction(data = test_data_mult %>% dplyr::slice(c(1:3000, 69124:72124)), prep_class = "class", cap_status = "winter", nest_coords = NULL,
                      single_blk_merge = TRUE, gr_speed = FALSE, bursts_rec = FALSE, max_flight_sp = 40, dst_stat_gap = 30, time_stat_gap = 3,
                      centr_dist = 50, near_blk_dist = 20, near_stop_rec = 1, near_stat_dist = 5, clust_min_rec = 2, br_win_min = 30)
  expect_true(identical(c("ID", "cap_status", "nest_coords", "single_blk_merge", "gr_speed", "bursts_rec", "max_flight_sp", "dst_stat_gap", "time_stat_gap",
                          "centr_dist", "near_blk_dist", "near_stop_rec", "near_stat_dist", "clust_min_rec", "br_win_min"), names(input_form)))
})
