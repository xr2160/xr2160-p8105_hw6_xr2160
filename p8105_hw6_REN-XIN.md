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

Create a city_state variable (e.g. “Baltimore, MD”), and a binary
variable indicating whether the homicide is solved. Omit cities Dallas,
TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim race.
Also omit Tulsa, AL – this is a data entry mistake. For this problem,
limit your analysis those for whom victim_race is white or black. Be
sure that victim_age is numeric.

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

\##Step 3 : For the city of Baltimore, MD, use the glm function to fit a
logistic regression with resolved vs unresolved as the outcome and
victim age, sex and race as predictors. Save the output of glm as an R
object; apply the broom::tidy to this object; and obtain the estimate
and confidence interval of the adjusted odds ratio for solving homicides
comparing male victims to female victims keeping all other variables
fixed.

``` r
baltimore_df =
  homicides_df %>%
    filter(city_state == "Baltimore, MD")
  glm(
    homicide_solved ~ victim_age + victim_race + victim_sex,data = baltimore_df,family = binomial()) %>%
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate), 
    lower_CI = exp(estimate - 1.96 * std.error),
    upper_CI = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR,lower_CI, upper_CI,p.value) %>% 
    
knitr::kable(digits = 3)
```

| term             |    OR | lower_CI | upper_CI | p.value |
|:-----------------|------:|---------:|---------:|--------:|
| (Intercept)      | 1.644 |    1.027 |    2.634 |   0.039 |
| victim_age       | 0.989 |    0.980 |    0.999 |   0.027 |
| victim_raceWhite | 2.518 |    1.522 |    4.165 |   0.000 |
| victim_sexMale   | 0.391 |    0.266 |    0.573 |   0.000 |

``` r
baltimore_df
```

    ## # A tibble: 1,371 × 5
    ##    city_state    homicide_solved victim_age victim_sex victim_race
    ##    <chr>                   <dbl>      <dbl> <chr>      <chr>      
    ##  1 Baltimore, MD               0         17 Male       Black      
    ##  2 Baltimore, MD               0         21 Male       Black      
    ##  3 Baltimore, MD               1         61 Male       White      
    ##  4 Baltimore, MD               1         46 Male       Black      
    ##  5 Baltimore, MD               1         21 Male       Black      
    ##  6 Baltimore, MD               1         21 Male       Black      
    ##  7 Baltimore, MD               1         34 Male       Black      
    ##  8 Baltimore, MD               0         26 Male       Black      
    ##  9 Baltimore, MD               1         21 Male       Black      
    ## 10 Baltimore, MD               1         30 Male       Black      
    ## # … with 1,361 more rows

Description：Keeping all other variables fixed, and the estimate of the
adjusted odds ratio for solving homicides comparing male victims to
female victims is with 95% confidence interval (, )

Now run glm for each of the cities in your dataset, and extract the
adjusted odds ratio (and CI) for solving homicides comparing male
victims to female victims. Do this within a “tidy” pipeline, making use
of purrr::map, list columns, and unnest as necessary to create a
dataframe with estimated ORs and CIs for each city.

\###Step 4 :Run regression models across cities

``` r
models_df = 
  homicides_df %>% 
  nest(data = -city_state) %>% 
  mutate(
    models =  map(.x = data, ~glm(homicide_solved ~ victim_age + victim_race + victim_sex, data = .x, family = binomial())),
    results = map(models, broom::tidy)
    ) %>% 
  select(city_state, results) %>% 
  unnest(results) %>% 
  mutate(
    OR = exp(estimate), 
    lower_CI = exp(estimate - 1.96 * std.error),
    upper_CI = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(city_state, term, OR, lower_CI,upper_CI)


models_df
```

    ## # A tibble: 188 × 5
    ##    city_state      term                OR lower_CI upper_CI
    ##    <chr>           <chr>            <dbl>    <dbl>    <dbl>
    ##  1 Albuquerque, NM (Intercept)      1.25     0.226    6.91 
    ##  2 Albuquerque, NM victim_age       0.975    0.948    1.00 
    ##  3 Albuquerque, NM victim_raceWhite 2.56     0.797    8.20 
    ##  4 Albuquerque, NM victim_sexMale   2.09     0.698    6.25 
    ##  5 Atlanta, GA     (Intercept)      2.35     1.21     4.53 
    ##  6 Atlanta, GA     victim_age       0.984    0.971    0.997
    ##  7 Atlanta, GA     victim_raceWhite 0.782    0.355    1.72 
    ##  8 Atlanta, GA     victim_sexMale   1.34     0.802    2.25 
    ##  9 Baltimore, MD   (Intercept)      1.64     1.03     2.63 
    ## 10 Baltimore, MD   victim_age       0.989    0.980    0.999
    ## # … with 178 more rows

Create a plot that shows the estimated ORs and CIs for each city.
Organize cities according to estimated OR, and comment on the plot.

\###Step 5: make a polt

``` r
models_df %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI)) + 
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1),
        plot.title = element_text(face = "bold.italic")) +
  labs(title = 'Figure 1: The estimates of OR for solving homicides comparing male victims to female victims',
       y = "Estimates OR",
       x = "City")
```

<img src="p8105_hw6_REN-XIN_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
models_df
```

    ## # A tibble: 188 × 5
    ##    city_state      term                OR lower_CI upper_CI
    ##    <chr>           <chr>            <dbl>    <dbl>    <dbl>
    ##  1 Albuquerque, NM (Intercept)      1.25     0.226    6.91 
    ##  2 Albuquerque, NM victim_age       0.975    0.948    1.00 
    ##  3 Albuquerque, NM victim_raceWhite 2.56     0.797    8.20 
    ##  4 Albuquerque, NM victim_sexMale   2.09     0.698    6.25 
    ##  5 Atlanta, GA     (Intercept)      2.35     1.21     4.53 
    ##  6 Atlanta, GA     victim_age       0.984    0.971    0.997
    ##  7 Atlanta, GA     victim_raceWhite 0.782    0.355    1.72 
    ##  8 Atlanta, GA     victim_sexMale   1.34     0.802    2.25 
    ##  9 Baltimore, MD   (Intercept)      1.64     1.03     2.63 
    ## 10 Baltimore, MD   victim_age       0.989    0.980    0.999
    ## # … with 178 more rows

Description：This graph shows that the estimated ORs are different among
the major 50 cities in the U.S. New York has the lowest estimated OR,
which indicates that homicide victims in New York are much less likely
to be solved by men than by women. In addition to the highest estimated
OR in Albuquerque, it is also clear from this graph that there are many
cities with wide confidence intervals, such as Fresno,CA, Stockton,CA,
Albuquerque,NM and Stockton,CA.
