
# esemComp

<!-- badges: start -->
<!-- badges: end -->

esemComp helps composing ESEM-within-CFA syntaxes to be run in lavaan.
It has helper functions to create target rotation matrices and to run
(ESEM-like) exploratory factor analyses.

## Installation

You can install the development version of esemComp from
[GitHub](https://github.com/MateusPsi) with:

``` r
# install.packages("devtools")
devtools::install_github("MateusPsi/esemComp", build_vignettes = TRUE)
```

Be advised: the package is still in early development stages.

## Example

The basic usage of the package follows the steps: make target rotation
matrix (optional) \> do exploratory factor analysis \> compose syntax \>
run model in lavaan.

``` r
library(esemComp)
# use Holzinger and Swineford (1939) dataset in lavaan package
hw_data <- lavaan::HolzingerSwineford1939
hw_data <- hw_data[,c(7:15)]

#make exploratory analysis with geomin rotation
geomin_efa <- esem_efa(hw_data,3)
#> Loading required namespace: GPArotation
referents_vector <- c(textual = "x5", visual = "x3", speed = "x7")
model_syntax <- syntax_composer(geomin_efa, referents_vector)
writeLines(model_syntax)
#> textual =~ start(0.193)*x1+
#> start(0.042)*x2+
#> -0.066*x3+
#> start(0.845)*x4+
#> start(0.884)*x5+
#> start(0.804)*x6+
#> 0.035*x7+
#> start(-0.044)*x8+
#> start(0.024)*x9 
#> 
#> visual =~ start(0.595)*x1+
#> start(0.509)*x2+
#> start(0.688)*x3+
#> start(0.02)*x4+
#> -0.061*x5+
#> start(0.083)*x6+
#> -0.137*x7+
#> start(0.137)*x8+
#> start(0.391)*x9 
#> 
#> speed =~ start(0.029)*x1+
#> start(-0.124)*x2+
#> 0.016*x3+
#> start(0.009)*x4+
#> 0.009*x5+
#> start(-0.011)*x6+
#> start(0.731)*x7+
#> start(0.688)*x8+
#> start(0.455)*x9

# esem-within-cfa
esem_w_cfa <- lavaan::cfa(model_syntax, data = hw_data)
```
