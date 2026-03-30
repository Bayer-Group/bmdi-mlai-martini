# data info on spec id

info on size of extracted data in terms of numbers of subjects and
columns (roughly)

## Usage

``` r
data_info(spec_entry)
```

## Arguments

- spec_entry:

  Top level entry of an object of class `martini_spec`.

## Value

list with entries `nsubj` and `ncol` giving the number of distinct
values in the .id column and the (approximate) number of columns derived
from the data set, respectively.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
