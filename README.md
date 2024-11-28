# Migration and stationary sites

MoveApps

Github repository: <https://github.com/janhanz/BreedWinterStop>

## Description

An algorithm distinguishing migration from stationary (non-migration) records in GPS tracks of migratory birds. Stationary records are further classified into wintering grounds, breeding grounds, and stopover sites.

## Documentation

The App was developed for the purpose of automated detection of breeding sites, wintering sites, and stopovers in multi-year GPS tracking data of migratory birds investigated in [HABITRACK](https://habitrack.eu/?lang=en) project.

<center><img src="images/logo5.png" width="120"/></center>

The algorithm uses either the speed between consecutive records or ground speed derived from GPS devices to run K-means clustering to distinguish between migration and stationary records in the tracks. Stationary records are then classified into breeding grounds and wintering grounds based on the information on bird status when tagged. The user supplies whether the bird was tagged on (or closer) to breeding grounds or wintering grounds. Alternatively, WGS84 coordinates of bird's nest can be supplied. This information allows K-means analysis to distinguish blocks of breeding and wintering records. Those blocks being shorter than the user defined minimum period of stay in the breeding grounds are subsequently assigned to stopover sites. Additional steps using DBSCAN clustering split incoherent blocks of stationary records, and reassign outlying stopover records to migration, to increase the precision of data classification.

*Enter here a detailed description of your App. What is it intended to be used for. Which steps of analyses are performed and how. Please be explicit about any detail that is important for use and understanding of the App and its outcomes. You might also refer to the sections below.*

### Application scope

#### Generality of App usability

This App was developed to identify breeding and wintering grounds and stopover sites in GPS tracking data of Eurasian Curlew (*Numenius arquata*), and was also tested on the tracking data of the other bird species in HABITRACK project. It should be able to distinguish the mentioned stages of annual cycle in many migratory bird species, if the migration movement is distinguished from the stationary stages by clearly different speed.

*State here if the App was developed for a specific species, taxon or taxonomic group, or to answer a specific question. How might it influence the scope and utility of the App. This information will help the user to understand why the App might be producing no or odd results.*

*Examples:*

This App was developed using data of birds.

This App was developed using data of red deer.

This App was developed for any taxonomic group.

This App was developed to identify kill sites, but can probably be used to identify any kind of location clusters like nests, dens or drinking holes.

#### Required data properties

The App is designed to use GPS tracking data with structure as defined in Movebank.org. It means a column `timestamp`, and GPS coordinates as `geometry` in a single column are required. It also extracts bird ID (`individual_local_identifier`) from the attributes of the data. Optional data can include Horizontal dilution of precision (HDOP) as `gps_hdop`, number of satellites in fix as `gps_satellite_count`, and ground speed derived from GPS device as `ground_speed`.

The App may provide acceptable classification results even for data of lower frequency of fixes around 30 to 60 mins.

*State here the required and/or optimal data properties for this App to perform properly.*

*Examples:*

This App is only applicable to data that reflect range resident behavior.

The data should have a fix rate of at least 1 location per 30 minutes.

The App should work for any kind of (location) data.

### Input type

`move2::move2_loc` object

*Indicate which type of input data the App requires.*

*Example*: `move2::move2_loc`

### Output type

`move2::move2_loc` object

*Indicate which type of output data the App produces to be passed on to subsequent Apps.*

*Example:* `move2::move2_loc`

### Artefacts

`"Classification_records_summary_", bird_ID, ".csv"`: csv-file with a summary of stationary blocks of records. It shows bird ID, block class (breeding, wintering, stopover sites), block size (in km), distance to the consecutive stationary block (in km), UTC time and calendar date and duration of stay in each block (separately in days, hours, and minutes), WGS84 coordinates (longitude, latitude) of the centroids of the blocks, and range of rows and number of each block corresponding to the output file.

`"Classification_records_", bird_ID, ".csv"`: csv-file with the complete output. It shows bird ID, and for each record UTC timestamp, separately the year, month, day, hour, minute and seconds, consecutive and/or ground speed (in m/s), distance to the next record (in m), block type (stationary or migration), block class (migration, wintering, breeding, stopover site), block size (in km), block number, and WGS84 coordinates (longitude, latitude).

*If the App creates artefacts (e.g. csv, pdf, jpeg, shapefiles, etc), please list them here and describe each.*

*Example:* `rest_overview.csv`: csv-file with Table of all rest site properties

### Settings

**Capture status of a bird** (`cap_status`): Capture status of a bird during tagging. ‘winter’ when tagged on the wintering grounds, ‘breed’ when tagged on the breeding grounds. Birds tagged at stopover sites should be assigned to ‘winter’ if the location is closer to wintering grounds, and ‘breed’ if closer to breeding grounds. Alternatively, nest coordinates can be supplied.

**Nest coordinates** (`nest_coords`): Nest coordinates (longitude, latitude) supplied in decimal format and WGS84 coordinate system. Alternatively, capture status of a bird during tagging can be provided.

**Merge single-row blocks** (`single_blk_merge`): Merge single-row blocks of migration or stationary records with the neighbouring blocks. (default TRUE)

**Ground speed** (`gr_speed`): Use the ground speed measured by GPS device instead of the speed between consecutive records. (default FALSE)

**Bursts in the records** (`bursts_rec`): Preserve bursts in the data, i.e. very frequent records. (default FALSE)

**Maximum reasonable speed of flight** (`max_flight_sp`): The highest reasonable speed of bird flight. The value may be a bit larger than the highest know speed of flight of a given bird species to preserve exceptional but real fast flights. Units: `m/s` (default 40)

**Distance gap between consecutive records** (`dst_stat_gap`): The limit distance between two consecutive records above which the records will be split into two block. Units: `km` (default 30)

**Time gap between consecutive records** (`time_stat_gap`): The limit time between two consecutive records above which the records will be split into two block. Units: `hours` (default 3)

**Distance for distinguishing breeding from wintering grounds** (`centr_dist`): The limit distance below which breeding and wintering grounds are not separated from each other, i.e. the bird did not migrate. Units: `km` (default 50)

**Distance between near (neighbouring) blocks** (`near_blk_dist`): Distance below which two near (neighbouring) blocks of stationary records are merged. Units: `km` (default 20 km)

**Distance for detection of ‘outlier’ records in stopover blocks** (`near_stop_rec`): Distance above which the neighbouring records in a stopover block are considered ‘outliers’ and reclassified as migratory records. Units: `km` (default 1 km)

**Minimum number of records in a stopover block** (`clust_min_rec`): Minimum number of records in a stopover block to be checked for ‘outliers’. (default 2)

**Distance for detection of new stationary blocks** (`near_stat_dist`): Distance above which the neighbouring records in a stationary block are assigned to a new block. Units: `km` (default 5)

**Minimum length of a breeding or wintering block** (`br_win_min`): Minimum length of a block of records to be recognized as breeding or wintering grounds. Units: `days` (dafault 30)

*Please list and define all settings/parameters that the App requires to be set by the App user, if necessary including their unit. Please first state the Setting name the user encounters in the Settings menu defined in the appspecs.json, and between brackets the argument used in the R function to be able to identify it quickly in the code if needed.*

*Example:* `Radius of resting site` (radius): Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

### Changes in output data

The input data is filtered according to the user settings.

*Specify here how and if the App modifies the input data. Describe clearly what e.g. each additional column means.*

*Examples:*

The App adds to the input data the columns `Max_dist` and `Avg_dist`. They contain the maximum distance to the provided focal location and the average distance to it over all locations.

The App filterers the input data as selected by the user.

The output data is the outcome of the model applied to the input data.

The input data remains unchanged.

### Most common errors

*Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling

*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set.

### Acknowledgements

<img src="images/EN_FundedbytheEU_RGB_POS.png" width="600">
