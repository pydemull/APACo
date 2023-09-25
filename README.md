
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APACo

<!-- badges: start -->

[![R-CMD-check](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview of the repository

The aim of this repository is to allow the reproduction of the
analytical pipeline implemented to get the results of the APA&Co project
([ClinicalTrials.gov ID:
NCT04732923](https://clinicaltrials.gov/study/NCT04732923?term=APA%26Co&rank=4)).
Briefly, the APA&Co project was conducted to observe the change in
physical condition, physical activity, and motivation for physical
activity in coronary artery disease patients at 1 year after following a
cardiac rehabilitation program. This research project was managed by the
hospital center of Cholet (France).

The repository has two branches, `master` and `pipeline`:

- The `master` branch contains the code and the data of the `{APACo}`
  package, that is needed to run the analytical pipeline of the project.
  This package is not intended to provide functions that could be reused
  in other projects. Some of the functions actually could be used for
  this purpose, but most of them were designed only to work with the
  data of the project.
- The `pipeline` branch contains the code to run the analytical
  pipeline. Please visit this branch
  [here](https://github.com/pydemull/APACo/tree/pipeline) to get the
  information presenting the steps to be followed to run the pipeline.

## Licenses

### Code

The code of the `{APACo}` package is provided under GNU General Public
License Version 3.0 (please see LICENSE.md file).

### Datasets

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />The
datasets included in the `{APACo}` package are provided under
<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">License
Creative Commons Attribution NonCommercial-NoDerivatives 4.0
International (CC BY-NC-ND 4.0)</a>.
