#' Pull sensor volume and occupancy
#'
#' Create a tidy dataframe, containing volume and occupancy, for a single date and sensor.  Use `pull_sensor_ids` to obtain metro sensor IDs.
#'
#' @param pull_date an object of class integer or string which indicates the date of data to pull.  Can be in mdy, ymd, or dmy format, but format must be specified in the date_fmt argument.  A vector of dates is accepted when used in tandem with the `purrr::pmap()` function.
#' @param date_fmt an object of class string which indicates the format of the `pull_date` argument.
#' @param sensor an object of class integer or string which indicates the sensor ID.  See documentation for `pull_sensor_ids` to obtain metro sensor IDs.unction
#'
#' @return dataframe containing variables volume, occupancy, sensor, date, time.  Note that occupancy *can* be missing while volume data exists and vice versa.  It is unknown how a loop could be monitoring volume and not occupancy. Also note that if you assign the output of pull_loops, the result is returned in-memory, and there must be sufficient space in-memory to do so.
#'
#' @examples
#'   # Simple example
#'   loop_data <- pull_sensor(5474, 20181014, "ymd")

#:   # Mapping example
#'   date_range <- c(20190101:20190201)
#'   loop_data <- pmap(list(8564, date_range, "ymd"), pull_sensor)
#'   loops_full <- rbindlist(loop_data)
#'
#'   # Parallel mapping example; takes longer if only pulling one or two days because libraries have to be copied to each core
#'   library(parallel)
#'   cl <- makeCluster(detectCores() - 1) # Leaving one core unused
#'   params <- list(8564, date_range, "ymd")
#'
#'   clusterSetRNGStream(cl, 1)
#'   loop_data <- params %>%
#'    lift(clusterMap, cl = cl)(fun = pull_sensor)
#'   stopCluster(cl)
#'
#'   loops_full <- rbindlist(loop_data)
#'
#' @importFrom magrittr %>%
#' @importFrom tibble enframe as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom rowr cbind.fill
#'
#' @export


pull_sensor <- function(sensor, pull_date) {

  extension_pull <- function (ext, ...) {

    pull_year <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")
    pull_month <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")
    pull_day <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")

    df_default <- as_tibble(NA, validate = F)

    try(df_default <- enframe(fromJSON(paste0("http://data.dot.state.mn.us:8080/trafdat/metro/", pull_year, "/", pull_year, pull_month, pull_day, "/", sensor, ".", ext, "30.json"))) %>%
          select(-name))

    return(df_default)

  }

  #exts <- c("v", "c")
  #loops_ls <- map(exts, extension_pull)

  volume <- extension_pull("v")
  occupancy <- extension_pull("c")

  loop_uneven <- rowr::cbind.fill(volume, occupancy, fill = NA)
  names(loop_uneven) <- c("volume", "occupancy")

  loop_date_sensor <- loop_uneven %>%
    mutate(date = pull_date,
           sensor = sensor)

  #Add time
  if (nrow(loop_date_sensor) == 1) {
    # Return essentially empty dataframe if both volume and occupancy are missing for entire day
    loop_date_sensor %>%
      mutate(hour = NA,
             min = NA)
  } else {
    # Add hour and minutes if either volume or occupancy (or both) are available
    bind_cols(loop_date_sensor,
              as_tibble(rep(0:23, each = 120)) %>% rename(hour = value),
              as_tibble(rep(seq(from = 0, to = 59.5, by = 0.5))) %>% rename(min = value))
  }
}

#' Function to pull all sensor IDs in the Twin Cities metro
#'
#' Create a tidy dataframe containing sensor IDs for MnDOT metro district, mainly to be used with pull_sensor
#'
#' @return dataframe containing variable "detector"
#'
#' @examples
#' sensors <- sensor_pull()
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr
#' @importFrom magrittr %>%
#' @importFrom dplyr transmute
#' @importFrom tibble enframe
#'
#' @export


pull_sensor_ids <- function() {

  url <- "http://data.dot.state.mn.us/iris_xml/metro_config.xml.gz"
  tmp <- tempfile()
  download.file(url,tmp)
  metro_config <- read_xml(gzfile(tmp))

  enframe(trimws(xml_attr(xml_find_all(metro_config, "//detector"), "name"))) %>%
    transmute(detector = value)
}

#' Pull metro sensor configuration
#'
#' Read MnDOT JSON feed and wrangle into a tidy dataframe containing 20 variables related to sensor configuration.  Useful for mapping (contains lat/lons) and calculating performance measures (contains detector_field).
#'
#' @param return_opt an object of class string which indicates how to return the data.  "within_dir" will return the data within the directory as a csv entitled "Configuration of Metro Detectors <<date in format yyyy-mm-dd>>". "in-memory" will return the data in R, but requires assignment.
#' @return dataframe containing 20 variables, including detector_field and lat/lons, for each sensor in MnDOT's metro district
#'
#' @examples
#' config <- pull_configuration("in-memory") # Assign to an object
#' pull_configuration("within_dir) # No assignment necessary
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_path
#' @importFrom dplyr mutate select rename bind_rows bind_cols left_join
#' @importFrom tidyr separate unite
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom data.table fwrite
#'
#' @export


pull_configuration <- function(return_opt = c("within_dir", "in_memory")) {

  url <- "http://data.dot.state.mn.us/iris_xml/metro_config.xml.gz"
  tmp <- tempfile()
  download.file(url,tmp)
  metro_config <- read_xml(gzfile(tmp))

  # ------------------
  # PATHS
  # ------------------

  # Detector paths - connect rnodes and corridors to this
  detector_paths <- enframe(xml_path(xml_find_all(metro_config, "//detector"))) %>%
    mutate(detector_path = value) %>%
    separate(detector_path, into = c('front', 'tms_config', 'device', 'rnode', 'detector'), sep = '/') %>%
    unite(rnode_path, front, tms_config, device, rnode, sep = '/') %>%
    mutate(rnode_path = trimws(rnode_path)) %>%
    mutate(corridor_path = rnode_path) %>%
    separate(corridor_path, into = c('front', 'tms_config', 'device', 'rnode'), sep = '/') %>%
    unite(corridor_path, front, tms_config, device, sep = '/') %>%
    mutate(corridor_path = trimws(corridor_path)) %>%
    select(-name) %>%
    rename(detector_path = value)

  # Rnode paths
  rnode_paths <- enframe(xml_path(xml_find_all(metro_config, "//r_node"))) %>%
    transmute(rnode_path = value)

  # Corridor paths
  corridor_paths <- enframe(xml_path(xml_find_all(metro_config, "//corridor"))) %>%
    transmute(corridor_path = value)

  # ------------------
  # ATTRIBUTES (rnodes & detectors)
  # ------------------

  attr_clean <- function(category, attribute) {
    trimws(xml_attr(xml_find_all(metro_config, paste0("//", category)), attribute))
  }

  d_attr_ls <- list("name",
                    "label",
                    "category",
                    "lane",
                    "field",
                    "abandoned")

  rn_attr_ls <- list("name",
                     "n_type",
                     "transition",
                     "label",
                     "lon",
                     "lat",
                     "lanes",
                     "shift",
                     "s_limit",
                     "station_id",
                     "attach_side")

  c_attr_ls <- list("route", "dir")

  attr_to_df <- function(category, attr_ls) {
    attributes_ls <- map2(category, attr_ls, attr_clean)
    names(attributes_ls) <- paste(category, attr_ls, sep = "_")
    bind_rows(attributes_ls)
  }

  attr_all_ls <- list(d_attr_ls, rn_attr_ls, c_attr_ls)
  categories <- list("detector", "r_node", "corridor")

  attributes_full <- map2(categories, attr_all_ls, attr_to_df)

  # Bind paths to attributes
  d_paths_attr <- bind_cols(detector_paths, attributes_full[[1]])
  rnode_paths_attr <- bind_cols(rnode_paths, attributes_full[[2]])
  corr_paths_attrs <- bind_cols(corridor_paths, attributes_full[[3]])

  detector_rnodes_full <- left_join(d_paths_attr, rnode_paths_attr, by = c('rnode_path'))
  configuration <- left_join(detector_rnodes_full, corr_paths_attrs, by = c('corridor_path'))

  config_tidy <- configuration %>%
    select(-rnode, -rnode_path, -detector, -detector_path, -corridor_path) %>%
    mutate(date = Sys.Date())

  if (return_opt == "in_memory") {

    return(config_tidy)

  } else if (return_opt == "within_dir") {

    fwrite(config_tidy, paste0("Configuration of Metro Detectors ", Sys.Date(), ".csv"))

  }

  config_tidy

}
