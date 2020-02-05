# tc.sensors
[![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com) [![forthebadge](https://forthebadge.com/images/badges/60-percent-of-the-time-works-every-time.svg)](https://forthebadge.com)

[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues) [![HitCount](http://hits.dwyl.io/sullivannicole/tcsensors.svg)](http://hits.dwyl.io/sullivannicole/tcsensors) [![Website MnDOT JSON](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](http://data.dot.state.mn.us:8080/trafdat/metro/2018/20181021/5474.c30.json)

<!-- badges: start --> 
<!---- use_badge(Maturing)
<!-- badges: end -->

## Overview
A package for pulling data for Minnesota Department of Transportation (MnDOT) loop detectors installed on the Minnesota Freeway system in 30-second interval measurements of occupancy and volume, data which are pushed daily to a public JSON feed.

## Installation

To use this package, clone this repo (open the terminal and navigate to the directory of your choice, then run `git clone https://github.com/sullivannicole/tc.sensors`) or just download this repo manually by clicking the green "Clone or Download" button above.  To install the package, you'll first need the devtools library installed, if you don't have it already.  Run `install.packages("devtools")` in an R script or notebook to do so.  Then attach the devtools package by running `library(devtools)`.  Last, run `build("~/package/path/here")`, the path being the directory in which you saved/cloned your local copy of tc.sensors.  Alternatively, if you're using RStudio, you can open the tc.sensors.Rproj file and then select "Clean and Rebuild" from the "Build" tab.

## Documentation

To access documentation and for help on how to use the package, run `?<FUNCTION-NAME>` (e.g. `?pull_sensor`, `?pull_configuration`, `?pull_sensor_ids`).  Access vignettes in the vignettes file to see examples of end-to-end workflows for pulling and storing data locally en masse.  Check back for a vignette (to be added in the near future!) for calculating speeds, reference speeds, delay, and VMT from the resulting files.

## Contributor/Maintainer
* Nicole Sullivan (nicole.sullivan@metc.state.mn.us)
