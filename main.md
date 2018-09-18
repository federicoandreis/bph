BPH - Exploratory analysis
================
Federico Andreis
18 September 2018

Data in
=======

Load the data and create a reduced version of the dataset with outcome grouped by *hydrocloride*

``` r
df <- read_csv('Data/df.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   health_board = col_character(),
    ##   drug = col_character(),
    ##   POSTCODE = col_character(),
    ##   Latitude = col_double(),
    ##   Longitude = col_double(),
    ##   DZ = col_character(),
    ##   SIMD16_Rank = col_double()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 2798 parsing failures.
    ## row # A tibble: 5 x 5 col     row col     expected               actual file          expected   <int> <chr>   <chr>                  <chr>  <chr>         actual 1  1305 Easting no trailing characters e3     'Data/df.csv' file 2  1306 Easting no trailing characters e3     'Data/df.csv' row 3  3550 Easting no trailing characters e3     'Data/df.csv' col 4  3551 Easting no trailing characters e3     'Data/df.csv' expected 5  6036 Easting no trailing characters e3     'Data/df.csv'
    ## ... ................. ... ........................................................... ........ ........................................................... ...... ........................................................... .... ........................................................... ... ........................................................... ... ........................................................... ........ ...........................................................
    ## See problems(...) for more details.

``` r
dr <- df %>% group_by(gp_code,year,hydrocloride) %>% slice(1) %>% 
  ungroup %>% dplyr::select(-one_of('drug')) %>% mutate(lhydro_n=log(hydro_n))
```

A few plots: show temporal trends in prescription by hydrocloride.

``` r
dr %>% ggplot(aes(x=year,y=lhydro_n,group=hydrocloride))+
  geom_smooth()+theme_bw()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 1969 rows containing non-finite values (stat_smooth).

![](main_files/figure-markdown_github/unnamed-chunk-2-1.png) Show spatial trends faceted by year.

``` r
dr %>% ggplot(aes(Longitude, Latitude, size = hydro_n, colour = hydrocloride)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~year)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](main_files/figure-markdown_github/unnamed-chunk-3-1.png) \#
