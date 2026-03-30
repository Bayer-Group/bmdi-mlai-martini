# Build the feature matrix from various sources according to a specification object

**\[maturing\]**

The `build()` function allows to build a machine learning data set from
a specification object as provided by
[`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md)
(with or without data already attached).

## Usage

``` r
build(spec, join = dplyr::full_join, rm = FALSE)
```

## Arguments

- spec:

  a specification object as provided by
  [`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md)
  (either `spec` or `path` has to be provided)

- join:

  either function to join data sets (e.g.
  [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  or a character (vector) giving the names of the data sets containing
  the .ids to keep (e.g. `join = c('adxb', 'adlb')`). Defaults to
  [`dplyr::full_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  which is equivalent to 'adsl' (if included) according to CDISC
  standards.

- rm:

  boolean. defaults to FALSE. if TRUE, a repeated measurement feature
  matrix with an additional `.rmtime` column is prepared.
  **\[experimental\]**

## Value

`build()` returns a wide data set with one row per subject and
standardized column names for the subject id (`.id`) and the treatment
variable (`.trt`), if it is provided in the `spec` object. Objects with
additional information on the data are provided in the attributes of the
returned object.

`attr("source")`: a tibble giving file path and md5 checksums of the
source data sets.

`attr("dict")`: a tibble with the following columns

- `param`:

  original parameter name in the source data

- `column`:

  column name of the variable in the returned data. `column` is derived
  from `param` by transforming it into a valid file name and possibly
  adding a time extension, if multiple time points are considered for a
  particular parameter.

- `label`:

  parameter label

- `source`:

  source id provided by the specification object. If created with
  [`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md),
  this is the name of the domain.

- `type`:

  ADaM data type of the source data (adsl, bds or occds)

- `unit`:

  parameter unit (if applicable)

- `time`:

  measurement time point (if applicable)

- `spec_id`:

  name of the corresponding spec entry (if applicable)

## Details

Missing values in variables from occurrence data sets are interpreted as
'absence of event', whereas NAs in adsl and bds data are considered to
be true missing values. For missing values in occds data after joining
with other data sets, missing values are replace by 0 for numerics, an
additional level 'none' is introduced for for factors.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

## See also

[`build_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md),
[`build_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md),
[`build_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)

## Examples

``` r
feat <- build(martini_spec)
attr(feat, "dict")
#> # A tibble: 26 × 7
#>    param   label                               source type  column spec_id unit 
#>    <chr>   <chr>                               <chr>  <chr> <chr>  <chr>   <chr>
#>  1 .id     Unique Subject Identifier           adsl   adsl  .id    adsl    NA   
#>  2 .trt    Actual Treatment for Period 01      adsl   adsl  .trt   adsl    NA   
#>  3 AGE     Age                                 adsl   adsl  AGE    adsl    NA   
#>  4 SEX     Sex                                 adsl   adsl  SEX    adsl    NA   
#>  5 RACE    Race                                adsl   adsl  RACE   adsl    NA   
#>  6 CALCIUM Calcium (mg/dL) in Serum            adlb   bds   CALCI… adlb    mg/dL
#>  7 CREAT   Creatinine (mg/dL) in Serum         adlb   bds   CREAT  adlb    mg/dL
#>  8 GGT     Gamma Glutamyl Transferase (U/L) i… adlb   bds   GGT    adlb    U/L  
#>  9 HB      Hemoglobin (g/dL) in Blood          adlb   bds   HB     adlb    g/dL 
#> 10 HCT     Hematocrit (%) in Blood - Calculat… adlb   bds   HCT    adlb    %    
#> # ℹ 16 more rows
```
