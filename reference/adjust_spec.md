# Adjust spec object

Helper function to make adjustments to the spec object built by
`\link{adam_spec}()` to be used with the pipe. For example, update of
entries for `param`, `label`, `dupl_ctrl` (bds) `fct_levels`, `spec_id`,
`trt`, `id`, (adsl) `valuen`, `value`, `count` (occds)

## Usage

``` r
adjust_spec(spec, entry, ...)
```

## Arguments

- spec:

  spec object to modify

- entry:

  name of list element to modify in the spec

- ...:

  modifications to the `spec[[id]]` of the form `<name> = <value>`.

## Value

A modified version of `spec` to be used as input to `\link{build}()`

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
