# Create dictionary for spec entry

Create dictionary for spec entry

## Usage

``` r
create_dict(spec_entry)
```

## Arguments

- spec_entry:

  Top level entry of an object of class `martini_spec`.

## Value

the dictionary as a tibble. For all data types, the dict contains
columns `label`, `source`, `type` and `selected`. In addition for data
type adsl and bds there is `param`, with `unit` and `selected` as
additional column for bds and adsl, resp.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
