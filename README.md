[![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com) [![forthebadge](https://forthebadge.com/images/badges/60-percent-of-the-time-works-every-time.svg)](https://forthebadge.com)

[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues) [![HitCount](http://hits.dwyl.io/sullivannicole/tcsensors.svg)](http://hits.dwyl.io/sullivannicole/tcsensors) [![Website MnDOT JSON](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](http://data.dot.state.mn.us:8080/trafdat/metro/2018/20181021/5474.c30.json)

<!-- badges: start --> 
<!---- use_badge(Maturing)
<!-- badges: end -->

# tc.sensors
A package for pulling data for Minnesota Department of Transportation (MnDOT) loop detectors installed on the Minnesota Freeway system in 30-second interval measurements of occupancy and volume, data which are pushed daily to a public JSON feed.

## Required libraries
This package works with various semi-structured and unstructured datatypes, including XML and JSON, and wrangling them into a tidy format involves working with dates and times in addition to a number of other operations.  Therefore, there are a number of dependencies.

To install the dependencies, run `install.packages("<PACKAGE-NAME>")` in R.

*tidyverse
*data.table
*lubridate
*chron
*rowr
*jsonlite
*xml2

## Use the package!

To use this package, clone this repo (that just means "download all the files locally").  Navigate to the "tc.sensors.Rproj" file, and double-click on it to open it.  To install the package, run `install.packages("tc.sensors")`.  To access documentation and for help on how to use the package, run `?<FUNCTION-NAME>` (e.g. `?pull_sensor`, `?pull_configuration`).

## Contributors
* Nicole Sullivan (nicole.sullivan@metc.state.mn.us)
