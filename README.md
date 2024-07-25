# fMRI_Multiverse Shiny App

This repository contains the code for the METEOR shiny app accompanying this [paper](https://doi.org/10.1101/2024.01.14.575565)
The online version of the app is available [here](https://daniel-develop.shinyapps.io/METEOR/)

## Overview

This Shiny app is designed to visualize the analytical choices in fMRI studies using graph theoretical approach.

## Installation

To run this app locally, you will need to have R installed on your machine. You can download R [here](https://www.r-project.org/).

You will also need to install the `shiny` package and other required packages in R, which you can do by running the following command in your R console:

```r
install.packages("shiny")
install.packages(c('shiny', 'networkD3', 'dplyr', 'igraph', 'visNetwork', 'stringr', 'png', 'shinyjs', 'DT', 'rintrojs', 'ggplot2', 'qdapTools', 'RColorBrewer', 'forcats', 'readxl', 'shinyWidgets', 'tibble', 'htmlwidgets', 'ggtext'))
```

## Running the App
You can run the app via GitHUb

```r
shiny::runGitHub("METEOR", "kristantodan12")
```

## Running the App with Docker

For sustainability, we also shared the app in the form of a Docker image that can be accessed at:

```bash
docker pull danielkris12/brain_behav:METEOR
docker run -p 3838:3838 danielkris12/brain_behav:METEOR
```


