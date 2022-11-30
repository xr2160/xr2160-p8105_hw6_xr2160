P8105_hw6_REN XIN
================
2022-11-30

\###Problem 1:

\###Problem 2:

\##Step 1: import the data.

``` r
homicides_df = read_csv("data/homicide-data.csv",na = c("", "NA", "Unknown"))
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
homicides_df
```

    ## # A tibble: 52,179 × 12
    ##    uid   repor…¹ victi…² victi…³ victi…⁴ victi…⁵ victi…⁶ city  state   lat   lon
    ##    <chr>   <dbl> <chr>   <chr>   <chr>     <dbl> <chr>   <chr> <chr> <dbl> <dbl>
    ##  1 Alb-…  2.01e7 GARCIA  JUAN    Hispan…      78 Male    Albu… NM     35.1 -107.
    ##  2 Alb-…  2.01e7 MONTOYA CAMERON Hispan…      17 Male    Albu… NM     35.1 -107.
    ##  3 Alb-…  2.01e7 SATTER… VIVIANA White        15 Female  Albu… NM     35.1 -107.
    ##  4 Alb-…  2.01e7 MENDIO… CARLOS  Hispan…      32 Male    Albu… NM     35.1 -107.
    ##  5 Alb-…  2.01e7 MULA    VIVIAN  White        72 Female  Albu… NM     35.1 -107.
    ##  6 Alb-…  2.01e7 BOOK    GERALD… White        91 Female  Albu… NM     35.2 -107.
    ##  7 Alb-…  2.01e7 MALDON… DAVID   Hispan…      52 Male    Albu… NM     35.1 -107.
    ##  8 Alb-…  2.01e7 MALDON… CONNIE  Hispan…      52 Female  Albu… NM     35.1 -107.
    ##  9 Alb-…  2.01e7 MARTIN… GUSTAVO White        56 Male    Albu… NM     35.1 -107.
    ## 10 Alb-…  2.01e7 HERRERA ISRAEL  Hispan…      43 Male    Albu… NM     35.1 -107.
    ## # … with 52,169 more rows, 1 more variable: disposition <chr>, and abbreviated
    ## #   variable names ¹​reported_date, ²​victim_last, ³​victim_first, ⁴​victim_race,
    ## #   ⁵​victim_age, ⁶​victim_sex

\##Step 2 :Create a city_state variable (e.g. “Baltimore, MD”), and a
binary variable indicating whether the homicide is solved. Omit cities
Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim
race. Also omit Tulsa, AL

``` r
homicides_df = homicides_df %>% 
  mutate(city_state = str_c(city, ", ", state)) %>% 
  filter(
    !city_state == c("Tulsa, AL", "Dallas, TX", "Phoenix, AZ", "Kansas City, MO"),
    victim_race == c("White", "Black")
  ) %>% 
  mutate(
    victim_age = as.numeric(victim_age),
    homicide_solved = case_when(
      disposition == "Closed without arrest" ~ 0,
      disposition == "Open/No arrest"        ~ 0,
      disposition == "Closed by arrest"      ~ 1
    )
  ) %>%
  select(city_state, homicide_solved, victim_age, victim_sex, victim_race)

homicides_df
```

    ## # A tibble: 19,893 × 5
    ##    city_state      homicide_solved victim_age victim_sex victim_race
    ##    <chr>                     <dbl>      <dbl> <chr>      <chr>      
    ##  1 Albuquerque, NM               0         15 Female     White      
    ##  2 Albuquerque, NM               0         72 Female     White      
    ##  3 Albuquerque, NM               0         56 Male       White      
    ##  4 Albuquerque, NM               1         22 Female     White      
    ##  5 Albuquerque, NM               1         25 Male       Black      
    ##  6 Albuquerque, NM               1         20 Male       White      
    ##  7 Albuquerque, NM               0         88 Female     White      
    ##  8 Albuquerque, NM               0         36 Female     White      
    ##  9 Albuquerque, NM               1         24 Male       White      
    ## 10 Albuquerque, NM               1         50 Male       White      
    ## # … with 19,883 more rows

Description：I filtered 5 variables as col according to the
requirements, they are city_state, homicide_solved, victim_age,
victim_sex, victim_race. This table has 19,893 entries and 5 total
columns.

For the city of Baltimore, MD, use the glm function to fit a logistic
regression with resolved vs unresolved as the outcome and victim age,
sex and race as predictors. Save the output of glm as an R object; apply
the broom::tidy to this object; and obtain the estimate and confidence
interval of the adjusted odds ratio for solving homicides comparing male
victims to female victims keeping all other variables fixed.

``` r
baltimore_df =
  homicides_df %>%
    filter(city_state == "Baltimore, MD")
  glm(
    homicide_solved ~ victim_age + victim_race + victim_sex,data = baltimore_df,family = binomial()) %>%
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic    p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)        0.497    0.240        2.07 0.0386    
    ## 2 victim_age        -0.0108   0.00488     -2.21 0.0270    
    ## 3 victim_raceWhite   0.923    0.257        3.60 0.000323  
    ## 4 victim_sexMale    -0.940    0.196       -4.80 0.00000158
