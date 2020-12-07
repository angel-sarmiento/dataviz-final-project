---
title: "Visualizing Text and Distributions"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise you will explore methods to visualize text data and practice how to recreate charts that show the distributions of a continuous variable. 


## Part 1: Density Plots

Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) from 2016 to 2017, attempt to recreate the charts shown below


```r
library(tidyverse)
library(lubridate)
weather_tpa <- read_csv("https://github.com/reisanar/datasets/raw/master/tpa_weather_16_17.csv")
# random sample 
sample_n(weather_tpa, 4)
```

```
## # A tibble: 4 x 6
##    year month   day precipitation max_temp min_temp
##   <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>
## 1  2016     1     1             0       81       70
## 2  2016    10    13             0       89       71
## 3  2016     7    10             0       91       81
## 4  2016     7     7             0       90       81
```

See https://www.reisanar.com/slides/relationships-models#10 for a reminder on how to use this dataset with the `lubridate` package for dates and times.


(a) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: the option `binwidth = 3` was used with the `geom_histogram()` function.


```r
# Changing months to their respective names
tpa_clean <- weather_tpa %>% 
  mutate(month = month(month), 
         max_temp = as.double(max_temp), 
         min_temp = as.double(min_temp), 
         precipitation = as.double(precipitation)) %>% 
  arrange(month)
```


```r
#getting the months as names
tpa_clean <- tpa_clean %>% 
  transform(monthName = month.name[month]) 

# this is the only thing that sorts the numbers for some reason.
# there is probably a more tidy way to do this. 
tpa_clean$monthName <- factor(tpa_clean$monthName, levels = month.name) 

# plotting
tpa_clean %>% 
  ggplot(aes(x = max_temp, fill = monthName)) +
  geom_histogram( binwidth = 3) + 
  facet_wrap(~monthName) + 
  theme_bw() +
  theme(legend.position = "None") +
  # this gave me conniptions 
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  labs(x = "Maximum Temperatures", y = "Number of Days")
```

![](A_Sarmiento_project_03_files/figure-html/1a-1.png)<!-- -->

> For some reason, it does not look exactly like the original, however I think its right. Maybe something wrong with the `binwidths` parameter?

(b) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density.png" width="80%" style="display: block; margin: auto;" />

Hint: check the `kernel` parameter of the `geom_density()` function, and use `bw = 0.5`.



```r
tpa_clean %>% 
  ggplot(aes( x= max_temp), fill = "gray") + 
  geom_density(fill = 'gray', kernel = "epanechnikov", bw = 0.5) + 
  theme_minimal() + 
  labs(x = "Maximum Temperature")
```

![](A_Sarmiento_project_03_files/figure-html/1b-1.png)<!-- -->



(c) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 


```r
tpa_clean %>% 
  ggplot(aes(x = max_temp, fill = monthName)) +
  geom_density(kernel = "triangular") + 
  facet_wrap(~monthName) + 
  theme_bw() +
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  theme(legend.position = "None") + 
  labs(x = "Maximum Temperature", y = element_blank(), title = "Density plots for each month in 2016")
```

![](A_Sarmiento_project_03_files/figure-html/1c-1.png)<!-- -->



(d) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />


```r
library(ggridges)
tpa_clean %>% 
  ggplot(aes(x = max_temp, y = monthName, fill = monthName)) + 
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) + 
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  theme_minimal() +
  theme(legend.position = "None") + 
  labs(x = "Maximum Temperature", y = element_blank())
```

```
## Picking joint bandwidth of 1.49
## Picking joint bandwidth of 1.49
```

![](A_Sarmiento_project_03_files/figure-html/1d-1.png)<!-- -->




Hint: default options for `geom_density()` were used. 

(e) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: use the`ggridges` package, and the `geom_density_ridges()` function paying close attention to the `quantile_lines` and `quantiles` parameters.

> I cannot see what the differences in these two plots above are. 


(f) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges_plasma.png" width="80%" style="display: block; margin: auto;" />

Hint: this uses the `plasma` option (color scale) for the _viridis_ palette.



```r
tpa_clean %>% 
  ggplot(aes(x = max_temp, y = monthName, fill = stat(x))) + 
  stat_density_ridges(geom = "density_ridges_gradient",
                      quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_c(name = "", option = "C") +
  theme_minimal() +
  labs(x = "Maximum Temperature (in Fahrenheit Degrees)", y = element_blank())
```

```
## Picking joint bandwidth of 1.49
```

![](A_Sarmiento_project_03_files/figure-html/1f-1.png)<!-- -->


## Part 2: Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: https://www.reisanar.com/slides/text-viz#1

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://github.com/reisanar/datasets/blob/master/BB_top100_2015.csv)

- [RateMyProfessors comments](https://github.com/reisanar/datasets/blob/master/rmp_wit_comments.csv)

- [FL Poly News 2020](https://github.com/reisanar/datasets/blob/master/poly_news_FL20.csv)

- [FL Poly News 2019](https://github.com/reisanar/datasets/blob/master/poly_news_FL19.csv)

(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)
