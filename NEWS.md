# powergrid 0.2.1

# powergrid 0.2.0

This is a large, but still minor update. Some arguments have been renamed and
one function has a new name. Vignette has been improved greatly, and all
documentation has been updated and extended. Cross-referencing in Documentation
is improved. Warnings and documentation is more consistent now.

Not that this is a Beta release. Use
  `remotes::install_github("SwissClinicalTrialOrganisation/powergrid", ref =
  "v0.2.0")` to install it. The package is very soon to be sumbitted to CRAN. No
  changes at the user side are planned.
 
## Breaking Changes
* `SummarizeIterations()` replaces the older `SummarizeSims`. Iterations may,
  but must not be simulations. Language is consistency adjusted throughout the
  package. In documentation, as well as in attributes of `power_array` objects,
  "sim" is, where relevant, replaced by "iter".
* Central argument names have changed in Example, PowerPlot, GridPlot,
  AddExample and FindTarget:
  + `find_lowest` replaces `find_min`
  + `target_at_least` replaces `minimal_target`
  + `target_value` replaces `target`
* Compatibility with sse package was omitted. see is offline and is not
  updated. Keeping all working with eventual sse output was too error-prone for
  little use.

## Improvements user level
* All help files were improved
* Vignette was extended and improved
* Tests included! (thanks Richard!)
* Input handling of all functions and respective errors and warnings was
  improved.
* PowerPlot and Example naturally deal with one-dimensional `power_array`
  object. PowerPlot has a special figure to represent the one-dimensional
  situation.

## Improvements under the hood
* The plotting of examples inside `PowerPlot()` is now handled by `AddExample`.
* Summarizing across dimenstions inside `power_grid` is now performed by
  `SummarizeIterations()` (Thanks Richard!)

## bug fixes
* A multitude of bugs were fixed




