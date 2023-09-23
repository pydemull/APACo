
# APACo

## Get an overview of the package

This package was build to perform, share, and reproduce the analytical
pipeline implemented to get the results of the ‘APA&Co’ project
([ClinicalTrials.gov ID:
NCT04732923](https://clinicaltrials.gov/study/NCT04732923?term=APA%26Co&rank=4)).
Briefly, the ‘APA&Co’ project was conducted to observe the maintainance
of physical condition, physical activity, and motivation for physical
activity in coronary artery disease patients at 1 year after following a
cardiac rehabilitation program. This research project was managed by the
hospital center of Cholet (France). This package is not intented to
provide functions that could be reused in other projects. Some of the
functions actually could be used for this purpose, but most of them were
designed only to work with the data of the project.

The package includes the raw data as well as all the materials required
to reproduce the analyses of the project.

## Reproduce the analytical pipeline

The analytical pipeline implemented to the get the results of the
project is based on packages that have been designed to make all
analyses reproducible: the {renv} package, and the {targets} package.
{renv} will allow you to restore all the exact package dependencies that
have been used to perform analyses and to get the final materials of the
project. {target} will provide you the structure to automatically drive
the analyses. Please follow the steps below if you want to reproduce the
analyses of the project.

- Step 1: Intall R and RStudio on your PC.
- Step 2: Download the zip file of the {APACo} package and unzip it on
  your PC.
- Step 3 : Open the project in RStudio by double-clicking on the .Rproj
  file of the unzipped folder.
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

This last step will produce all the objects created for the required
analyses. It also will generate .html files corresponding to the
materials considered for scientific publication. Some materials may take
time to be built, so please be patient.

Once the analytical pipeline ended, you will can access any created
object using `targets::tar_read(OBJECT_NAME)` to read it in the Console
or the viewer pane depending on the class of the object, or using
`targets::tar_load(OBJECT_NAME)` to load the object in the global
environment. You can load all the created objects using
`targets::tar_load_everything()`. All the figures and .html reports will
appear in a new ‘out/’ folder placed at the root of the project during
the run of the analytical pipeline.

## Verify the code

If you want to look at the code implemented in the functions of the
{APACo} package used for the analyses, please take a look at the
dedicated [Github repository](https://github.com/pydemull/APACo). The
files placed in the ‘R/’ folder are what you want.
