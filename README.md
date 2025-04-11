
<!-- README.md is generated from README.Rmd. Please edit that file -->

# README

**(WARNING: The procedure described in this README can be implemented
only using a directory that contains all the raw data. Due to space
contraints, the data directory has been left empty on GitHub. A
directory with all data can be found at \[TO BE CONFIGURED\].)**

The present directory contains the information to reproduce the results
of the ‘EPLPA’ project, which aimed at conducting sex comparisons
regarding physical literacy scores as well as comparisons of movement
behaviour metrics across physical literacy profiles in French children.

## Running the analytical pipeline to get the results of the ‘EPLPA’ project with R and RStudio

All data and statistical analyses have been implemented in an analytical
pipeline using the `{targets}` R package. This pipeline performs several
actions: importing data, cleaning and joining data, processing
accelerometer data files, getting results, making figures and tables,
generating a report, exporting relevant materials. After running the
analytical pipeline in RStudio, you will be able to interactively
explore the created objects (e.g., dataframes, figures, etc.). All the
results will be available in the generated report. Please follow the
steps below to run the pipeline:

- Step 1: [Install R](https://cran.rstudio.com/) (version: 4.1.1),
  [RStudio](https://posit.co/download/rstudio-desktop/), and the
  corresponding version of
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (if your are
  a Windows user) on your machine. To retrieve past releases of R, you
  can go [here](https://cran.r-project.org/bin/windows/base/old/) for
  Windows, and [here](https://cran.r-project.org/bin/macosx/) for Mac.
- Step 2: Double-click on the `EPLPA.Rproj` file to open the project in
  RStudio.
- Step 3: Restore the package dependencies of the project with `{renv}`
  using in the Console the command line shown below and then following
  the instructions proposed in the Console.

``` r
renv::restore()
```

This may take several tens of minutes so that all the required packages
are downloaded from the web and then installed in the project directory.

- Step 4: Run the analytical pipeline with `{targets}` using the
  following command line in the Console:

``` r
targets::tar_make()
```

This last step will produce all the objects relating to the analytical
process. It also will generate the materials that are deemed to be used
for a scientific publication. Because the pipeline will process 99
accelerometer data file with 15-s epochs and will also generate high
resolution figures relating to each accelerometer data file, running the
pipeline will take a lot of time, likely around an hour.

The list of the objects created during the analytical process is shown
in the Console after each ‘target’ expression. Once the analytical
pipeline ended, you will can read any created object running
`targets::tar_read(OBJECT_NAME)` in the Console. You also will can load
any object in the global environment running
`targets::tar_load(OBJECT_NAME)` in the Console. You also will can load
all the created objects in one go running
`targets::tar_load_everything()` in the Console. All the exported
materials will appear in the `out/` directory placed at the root of the
project directory.

To have more information about what are the computations actually
performed behind the scene, you can open the `_targets.R` file placed at
the root of the directory. This file essentially includes a list of
`targets::tar_target()` functions, with the name of the object created
as first argument, and the operations performed to get that object as
second argument.

## Codebook

Running the analytical pipeline produces two main datasets that are
exported as .csv files and that are also shown in an .html report. These
datasets are called as follows: `capl_res` and `capl_res_4_valid_days`.
These datasets are those used to get the results of the project. This
section describes the variables available in these two datasets.

### Dataset 1: capl_res

### Dataset 2: capl_res_4_valid_days

## Licenses

### Code

The code of the pipeline is provided under GNU General Public License
Version 3.0 (please see LICENSE.md file).

### Data

<p xmlns:cc="http://creativecommons.org/ns#">
The accelerometer data files (.agd) and the project databases are
licensed under
<a href="https://creativecommons.org/licenses/by-nc-nd/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative
Commons Attribution-NonCommercial-NoDerivatives 4.0
International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nd.svg?ref=chooser-v1" alt=""></a>
</p>
