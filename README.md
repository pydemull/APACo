
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APACo

<!-- badges: start -->

[![R-CMD-check](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pydemull/APACo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Get an overview of the package

This package was build to perform, share, and reproduce the analytical
pipeline implemented to get the results of the ‘APA&Co’ project
([ClinicalTrials.gov ID:
NCT04732923](https://clinicaltrials.gov/study/NCT04732923?term=APA%26Co&rank=4)).
Briefly, the ‘APA&Co’ project was conducted to observe the change in
physical condition, physical activity, and motivation for physical
activity in coronary artery disease patients at 1 year after following a
cardiac rehabilitation program. This research project was managed by the
hospital center of Cholet (France). The present package is not intended
to provide functions that could be reused in other projects. Some of the
functions actually could be used for this purpose, but most of them were
designed only to work with the data of the project.

## Reproduce the analytical pipeline

The analytical pipeline implemented to the get the results of the
project is based on packages that have been designed to make all
analyses reproducible with R programming language: the {renv} package,
and the {targets} package. {renv} will allow you to restore all the
exact package dependencies that have been used to perform analyses and
to get the final materials of the project. {target} will provide you the
structure to automatically drive the analyses. Please follow the steps
below if you want to reproduce the analyses of the project.

- Step 1: Install R and RStudio on your machine.
- Step 2: Download the zip file of the {APACo} package and unzip it on
  your machine.
- Step 3 : Open the project in RStudio by double-clicking on the
  `.Rproj` file of the unzipped folder.
- Step 4: Restore the package dependencies of the project with {renv}
  using the following command line in the Console:

``` r
renv::restore()
```

- Step 5: Run the analytical pipeline with {targets} using the following
  command line in the Console:

``` r
targets::tar_make()
```

This last step will produce all the objects created during the
analytical process. It also will generate the materials (a .html report
with the main results, .tiff figures, and .html file-based supplementary
materials) that are deemed to be used for a scientific publication. Some
materials will take time to be built because they include several high
resolution figures, so please be patient.

Once the analytical pipeline ended, you will can access any created
object using `targets::tar_read(OBJECT_NAME)` in the Console to read it
in the Console or in the viewer pane depending on the class of the
object, or using `targets::tar_load(OBJECT_NAME)` in the Console to load
the object in the global environment. You can load all the created
objects in one go using `targets::tar_load_everything()` in the Console.
All the figures and .html files generated during the run of the
analytical pipeline will appear in a new `out/` folder placed at the
root of the project.

## Verify the code

The main interest of running the analytical pipeline is to allow an
exact reproduction of the analysis process. However, you will not see
what are the computations actually performed behind the scene. To have
more information about it, you can open the `_targets.R` file placed at
the root of the project. This file essentially includes a list of
`tar_target()` functions, with the name of the object created as first
argument, and the operations performed to get that object as second
argument. A quick way of looking at the code could be to copy the name
of a function of interest and paste it in the Console. Another way,
which will provide you a more confortable view of the code, is to go to
the [Github repository](https://github.com/pydemull/APACo) of the
{APACo} package. The files placed in the `R/` folder contain the code of
each of the functions developped for the project.

## Licenses

### Code

The code of this package is provided under GNU General Public License
Version 3.0 (please see LICENSE.md file).

### Datasets

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />The
datasets included in the package are provided under
<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">License
Creative Commons Attribution NonCommercial-NoDerivatives 4.0
International (CC BY-NC-ND 4.0)</a>.