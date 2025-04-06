Powergrid is a package developed by [Gilles Dutilh](https://www.gillesdutilh.com), partly funded by a [Statistical programming grant](https://www.sctoplatforms.ch/en/scto-platforms/statistics-methodology-5.html) from the SCTO. 

The package is intended to allow users to easily evaluate a
function across a grid of input parameters. The package' utilities are
aimed at performing analyses of *power and sample size*, allowing for
easy search of minimum n (or min/max of any other parameter) to
achieve a desired level of power (or any other objective). Also,
plotting functions are included that present the dependency of n and
power in relation to further parameters.

Note that the package is currently in (a late stage of)
development. Development may be followed at [Gilles' github](https://www.github.com/gillesdutilh/powergrid). You are encouraged to use the package released here on SCTO's github, currently version v0.1.0.  For replicability, make
sure you explicitly refer to the current release when loading the
package in your code:

```{r, eval = FALSE}
devtools::install_github("SwissClinicalTrialOrganisation/powergrid",
                         ref = "v0.1.0", # refer to the current beta for reproducibility
                         build_vignette = TRUE)
library(powergrid)
## you may want to start with reading the vignette illustrating a typial use case of the pacakge:
vignette("powergrid")

```

Please don't hesitate making an issue above or contributing through a
pull request. You may also contact Gilles by [email](mailto:info@gillesdutilh.com).

