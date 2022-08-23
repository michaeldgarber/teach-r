# teach-r

This repository contains course materials for a course on managing and visualizing data in R, including spatial data.

**Course title**: Introduction to managing and visualizing data in R: a short course on spatial and non-spatial data.

Course url: <https://michaeldgarber.github.io/teach-r/>

My background using R: <https://michaeldgarber.github.io/teach-r/who-am-i.html>

<br />

**Course summary**

This short course will introduce participants to popular tools in R for manipulating ("wrangling") and visualizing data through public-health-focused examples. Specifically, students will become familiar with ways to wrangle and visualize both aspatial (i.e., common spreadsheet-like) data and spatial data. The course will focus on tools that are a part of and that work well with the tidyverse, a set of packages with a common philosophy that, in my opinion, makes R intuitive and fun. Packages covered will include dplyr and sf for manipulating data, and ggplot2, mapview, and tmap for visualizing data. Many examples will transition back and forth between data with and without a spatial component, showing that working with spatial data need not require separate specialized geographic information systems (GIS) software. The course will conclude with a somewhat advanced but important topic, creating functions and iterating over them using the purrr toolkit.

**Modules**

*Background*

-   Background and set-up

    <https://michaeldgarber.github.io/teach-r/pre-reqs>

-   Managing packages: attached vs loaded via a namespace and other nuances

    <https://michaeldgarber.github.io/teach-r/manage-packages>

*Main modules*

1.  Introduction to R and data wrangling: learning the basics of dplyr with publicly available COVID-19 data:

    [https://michaeldgarber.github.io/teach-r/dplyr-1-nyt-covid](https://michaeldgarber.github.io/teach-r/dplyr-1-nyt-covid.html)

2.  R for data wrangling 2: more dplyr with tidycensus, mapview, and ggplot2

    <https://michaeldgarber.github.io/teach-r/dplyr-2-mapview-tidycensus>

3.  R for spatial data wrangling: using sf to wrangle OpenStreetMap-downloaded pharmacies in Atlanta and assess population living nearby

    [https://michaeldgarber.github.io/teach-r/sf-atl-pharm-part-1](https://michaeldgarber.github.io/teach-r/sf-atl-pharm-part-1.html)

    <https://michaeldgarber.github.io/teach-r/sf-atl-pharm-part-2>

4.  Making static and interactive maps in R

    <https://michaeldgarber.github.io/teach-r/map-making>

5.  Monte Carlo simulations and boots with the `purrr::map_dfr()`

    <https://michaeldgarber.github.io/teach-r/monte-carlo-sim-bootstrapping-purrr>
