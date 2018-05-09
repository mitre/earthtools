---
output: 
  html_document:
    keep_md: true
---

A package to make Spherical Earth computations easier.

## Use

This package relies heavily on the [`geosphere`](https://cran.r-project.org/web/packages/geosphere/index.html) package.
The added value here is that it makes it much easier to use the spherical trigonometry functions with data in `data.frame`s. 

## Key Features


```r
library(dplyr)
library(data.table)
library(earthtools)
```

First, some setup data:

```r
# for use in dplyr operations
airports <- data.frame(airport=c("EWR", "PHL", "JFK", "LGA"),
                       latitude=c(40.6924798333333, 39.8720833333333, 
                                  40.63992575, 40.77725),
                       longitude=c(-74.1686867777778, -75.2406611111111,
                                   -73.7786949722222, -73.8726111111111))

# for use in data.table operations
airports_dt <- setDT(copy(airports))

jfk <- airports %>% filter(airport=="JFK")
```

Now we can compute distances:

```r
airports %>%
  mutate(dist=et_dist_haversine(latitude, longitude, jfk$latitude, jfk$longitude))
```

```
##   airport latitude longitude      dist
## 1     EWR 40.69248 -74.16869 18.028939
## 2     PHL 39.87208 -75.24066 81.271730
## 3     JFK 40.63993 -73.77869  0.000000
## 4     LGA 40.77725 -73.87261  9.281924
```

Determine the initial/final bearing

```r
airports %>%
  filter(airport!="JFK") %>%
  mutate(bearing_initial=et_bearing_initial(latitude, longitude, 
                                            jfk$latitude, jfk$longitude),
         bearing_terminal=et_bearing_terminal(latitude, longitude, 
                                              jfk$latitude, jfk$longitude))
```

```
##   airport latitude longitude bearing_initial bearing_terminal
## 1     EWR 40.69248 -74.16869        99.90871        100.16285
## 2     PHL 39.87208 -75.24066        55.09815         56.04293
## 3     LGA 40.77725 -73.87261       152.47557        152.53682
```

And do point projections. 
Since the projected coordinates have both a latitude and longitude output, this was implemented as an S3 method to support both `dplyr` and `data.table` use cases.

The `dplyr` way:

```r
airports %>%
  et_projection(latitude, longitude, bearing=90, distance=10)
```

```
##   airport latitude longitude end_latitude end_longitude
## 1     EWR 40.69248 -74.16869     40.69227     -73.94890
## 2     PHL 39.87208 -75.24066     39.87188     -75.02352
## 3     JFK 40.63993 -73.77869     40.63972     -73.55908
## 4     LGA 40.77725 -73.87261     40.77704     -73.65254
```

The `data.table` way:

```r
airports_dt[,
            c("end_latitude", "end_longitude"):=et_projection(latitude, longitude, 
                                                              bearing=90, distance=10)
            ]
airports_dt
```

```
##    airport latitude longitude end_latitude end_longitude
## 1:     EWR 40.69248 -74.16869     40.69227     -73.94890
## 2:     PHL 39.87208 -75.24066     39.87188     -75.02352
## 3:     JFK 40.63993 -73.77869     40.63972     -73.55908
## 4:     LGA 40.77725 -73.87261     40.77704     -73.65254
```

See the function documentation for all supported operations.

## Installation


```r
# install.packages("devtools")
devtools::install_github("mitre/earthtools")
```
