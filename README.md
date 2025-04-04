Powergrid is a package intended to allow users to easily evaluate a
function across a grid of input parameters. The package' utilities are
aimed at performing analyses of *power and sample size*, allowing for
easy search of minimum n (or min/max of any other parameter) to
achieve a desired level of power (or any other objective). Also,
plotting functions are included that present the dependency of n and
power in relation to further parameters.

Note that the package is currently in (a late stage of)
development. You are encouraged to use it, but for replicability, make
sure you explicitly refer to the current release when loading the
package in your code:

```{r, eval = FALSE}
devtools::install_github("SwissClinicalTrialOrganisation/powergrid",
                         ref = "v0.1.0", # the current beta
                         build_vignette = TRUE)

```

Please don't hesitate making an issue above or contributing through a
pull request. You can write me an [email](info@gillesdutilh.com).

