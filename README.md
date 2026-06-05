
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ‘EPLPA’: ‘Evaluation et promotion de la littératie physique à Avrillé’

## Overview of the content of the repository

The aim of the present repository is to allow the reproduction of the
exploratory analyses and results from the first series of measurements
of the ‘EPLPA’ project, which aimed at conducting sex comparisons
regarding physical literacy scores, as well as comparisons of movement
behaviour metrics across physical literacy profiles in French children.

The present repository is hosted on two different places: on GitHub
(<https://github.com/pydemull/EPLPA>), and on Open Science Framework
(<https://osf.io/w3kmf/>). Due to space constraints, the repository
hosted on GitHub does not contain the raw data that should be used along
with the code of the repository to obtain the results. Data are
available only in the Open Science Framework repository. Thus, if you
are interested in reading the code only, you can simply navigate online
in the GitHub repository. However, if you want to run the code to
reproduce the results of the project, you will have to go to the Open
Science Framework repository and download its content.

## Running the analytical pipeline to reproduce the results of the ‘EPLPA’ project

To get the results of the ‘EPLPA’ project, an analytical pipeline based
on R programming language has been implemented using the `{targets}`
package. Briefly, the pipeline consists of the following general steps:

- Analyzing the .agd files from the accelerometer (ActiGraph
  wGT3X-BT)-based measurements of movement behaviours to get a series of
  various metrics for each participant included in the study. These .agd
  files, which are anonymized, are available with 15-s epochs.
- Cleaning the databases that contain all the results from the field
  tests and questionnaires that were implemented to measure the
  dimensions of physical literacy.
- Joining data from the different databases.
- Performing statistical analyses.
- Generating a report containing all the results.
- Exporting relevant materials (main datasets, tables, and figures to be
  used for a scientific publication).

To run the pipeline and then reproduce the results of the project,
please follow the steps as described below:

- Step 1: Go to the repository hosted on the Open Science Framework
  platform (<https://osf.io/w3kmf/files/osfstorage>) and download on
  your machine all the required content by clicking on the ‘Download
  this folder’ link. Then, unzip the folder on your machine.
- Step 2: [Install R](https://cran.rstudio.com/) (version: 4.4.1),
  [RStudio](https://posit.co/download/rstudio-desktop/), and [Rtools
  4.4](https://cran.r-project.org/bin/windows/Rtools/) (if your are a
  Windows user) on your machine. To retrieve past releases of R, you can
  go [here](https://cran.r-project.org/bin/windows/base/old/) for
  Windows, and [here](https://cran.r-project.org/bin/macosx/) for Mac.
- Step 3: Double-click on the `EPLPA.Rproj` file from the downloaded and
  unzipped repository to open the project in RStudio.
- Step 4: Configure RStudio to use the correct R version. To do this, go
  in RStudio to Tools \> Global Options… \> R Sessions \> Change… \>
  Choose a specific version of R \> Select the version 4.4.1. Close
  RStudio and restart as described in Step 3.
- Step 5: Install the project’s package dependencies by reading the
  command lines shown below and running them in the Console. Please be
  careful about all the comments and DO NOT run all the command lines at
  a time. Instead, for each command line, run the line, answer when
  required to the questions asked in the Console, and then wait for the
  end of the actions currently performed.

``` r
# --------------------------------------------------------------
# Install {groundhog}
# --------------------------------------------------------------
require(devtools)
install_version("groundhog", version = "3.2.3", repos = "http://cran.us.r-project.org")

# --------------------------------------------------------------
# Set Git infos
# --------------------------------------------------------------
# Unfortunately, for unclear reasons, it seems that {groundhog} 
# needs to set a Git user name and email address for installing GitHub
# (not CRAN) packages. If you have already Git installed on your 
# machine and you have already a Git user name and a Git email 
# address, DO NOT run the next command line below and go to the 
# next command line. If you don't have a Git user name and email 
# address, set dummy information by running the command line right 
# below before running the subsequent command line. You will can 
# update these settings when you will use Git in the future.

# WARNING: Be sure to have read the text just above before running the line right
# below.
git2r::config(global = TRUE, user.name = "default", user.email = "default")

# --------------------------------------------------------------
# Install {activAnalyzer} GitHub R package
# --------------------------------------------------------------
library("groundhog")
groundhog.library("pydemull/activAnalyzer", "2025-03-21", tolerate.R.version='4.4.1')

# During this task, you could have some package installation problems for unclear 
# reasons. During some trials for running the pipeline on other machines than the 
# one that was used to develop the code and get the results of the project, we 
# encountered two problems that seem commonly faced by R users:

# Problem n°1 - The task fails at the end with an error message that looks like this: 
# Error: package or namespace load failed for ‘activAnalyzer’:
# .onLoad failed in loadNamespace () for 'rlang', details:
#  call: utils::packageVersion(pkg)
#  error: there is no package called ‘cli’
#
# This problem means that the installation of {cli} failed. To manage this problem, 
# follow the steps below:

# STEP 1:
# Restart R, and then run the command lines below:
# library("groundhog")
# groundhog.library("cli", "2025-03-21", tolerate.R.version='4.4.1')
# Repeat STEP 1 for each package that could not be found.

# STEP 2:
# Restart R, and then run the command lines below:
# library("groundhog")
# groundhog.library("pydemull/activAnalyzer", "2025-03-21", tolerate.R.version='4.4.1')

# Problem n°2 - The package installation may fail because you are not allowed to replace
# the current version of a package dependency. In the error message, it could be 
# indicated the presence of a '00LOCK' folder or file that prevents the modification 
# of the package. This can happen when something was wrong during a previous 
# installation attempt regarding that package. To resolve this issue, following 
# the step below could help:

# Restart R, and then run the command lines below:
# install.packages("pacman")
# pacman::p_unlock()
# groundhog.library("pydemull/activAnalyzer", "2025-03-21", tolerate.R.version='4.4.1')

# --------------------------------------------------------------
# Restart R
# --------------------------------------------------------------
## In RStudio, go to the 'Session' tab and click on 'Restart R'.

# --------------------------------------------------------------
# Install {activAnalyzer.batch} GitHub R package
# --------------------------------------------------------------
library("groundhog")
groundhog.library("pydemull/activAnalyzer.batch", "2025-03-21", tolerate.R.version='4.4.1')

# During this task, you could have some package installation problems for unclear 
# reasons. To try to resolve these problems, please process as suggested for the 
# {activAnalyzer} package.

# --------------------------------------------------------------
# Restart R
# --------------------------------------------------------------
## In RStudio, go to the 'Session' tab and click on 'Restart R'.

# --------------------------------------------------------------
# Install CRAN  R package dependencies
# --------------------------------------------------------------
## Set the CRAN R package dependencies
pkgs_cran <- c(
  "bigutilsr",
  "capl",
  "correlation",
  "dplyr",
  "factoextra",
  "flextable",
  "forcats",
  "ggpp",
  "ggplot2",
  "ggrain",
  "gtsummary",
  "Hmisc",
  "hms",
  "janitor",
  "npmv",
  "officer",
  "patchwork",
  "performance",
  "purrr",
  "quarto",
  "rankFD",
  "readr",
  "report",
  "scales",
  "sessioninfo",
  "skimr",
  "targets",
  "tarchetypes",
  "tibble",
  "tidyr"
)

## Install packages
library("groundhog")
groundhog.library(pkgs_cran, "2025-03-21", tolerate.R.version='4.4.1')

# --------------------------------------------------------------
# Restart R
# --------------------------------------------------------------
## In RStudio, go to the 'Session' tab and click on 'Restart R'.
```

- Step 6: Run the analytical pipeline using the following command lines
  in the Console:

``` r
library("groundhog")
groundhog.library("targets", "2025-03-21", tolerate.R.version='4.4.1')
tar_make()
```

This last step will produce all the objects related to the analytical
process. It will also generate the materials that are intended to be
used for a scientific publication. Because the pipeline will process 98
accelerometer data files with 15-s epochs and will also generate high
resolution figures related to each accelerometer data file, running the
pipeline will take a lot of time, likely more than an hour.

While the pipeline is running, the list of the objects that are created
is shown in the Console. Once the analytical pipeline has finished, you
can read any created object by running `targets::tar_read(OBJECT_NAME)`
in the Console. You can also load any object in the global environment
by running `targets::tar_load(OBJECT_NAME)` in the Console and you can
load all the created objects in one go by running
`targets::tar_load_everything()` in the Console. All the exported
materials will appear in the `out/` directory placed at the root of the
project directory.

To learn more about what are the computations actually performed behind
the scene, you can open the `_targets.R` file placed at the root of the
directory. This file essentially includes a list of
`targets::tar_target()` functions, with the name of the object created
as first argument, and the operations performed to get that object as
second argument.

## Codebook for the main (cleaned) datasets on which stand the main results of the pipeline

The two main datasets that are exported while running the analytical
pipeline and from which the final results of the project are obtained
are named as follows: `capl_res.csv` and `capl_res_4_valid_days.csv`.
The datasets are exported to the directory named `./out`. Below are the
definitions of the variables present in these datasets.

### Codebook for Dataset 1: `capl_res.csv`

This dataset contains the variables used to make sex comparisons
regarding physical literacy scores.

| Variable | Definition |
|:---|:---|
| id | Participant identification number. Numbers go from 1 to 171. Missing identification numbers correspond to children who did not want to participate to the study or who did not complete any field test or questionnaire. |
| school | Name of the school where the participant was recruited. |
| gender | Sex of the participant. We are aware that gender is different from sex. While we used sex information reported by the participant, the package used to analyse physical literacy scores required a column called ‘gender’ to indicate whether the participant was a boy or a girl. |
| age | Participant age in years. |
| camsa_skill_score1 | Skill score obtained during the first pass of the CAMSA test. |
| camsa_time1 | Time taken during the first pass of the CAMSA test, in seconds. |
| camsa_skill_score2 | Skill score obtained during the second pass of the CAMSA test. |
| camsa_time2 | Time taken during the second pass of the CAMSA test, in seconds. |
| plank_time | Plank test performance, in seconds. |
| pacer_lap_distance | Lap distance used for the PACER test, in meters. |
| pacer_laps | Number of stages completed during the PACER test. |
| pa_guideline | Response to the question about physical activity guidelines: 1 = ‘20 minutes’, 2 = ‘30 minutes’, 3 = ‘60 minutes or 1 hour’, 4 = ‘120 minutes or 2 hours’. |
| crf_means | Response to the question about the definition of cardiorespiratory fitness: 1 = ‘How well the muscles can push, pull, or stretch’, 2 = ‘How well the heart can pump blood and the lungs can provide oxygen’, 3 = ‘Having a healthy weight for our height’, 4 = ‘Our ability to do sports that we like’. |
| ms_means | Response to question about the definition of muscular strength and endurance: 1 = ‘How well the muscles can push, pull, or stretch’, 2 = ‘How well the heart can pump blood and the lungs can provide oxygen’, 3 = ‘Having a healthy weight for our height’, 4 = ‘Our ability to do sports that we like’. |
| sports_skill | Response to the question about the method to improve one’s sports skills: 1 = ‘Read a book about kicking and catching a ball’, 2 = ‘Wait until you get older’, 3 = ‘Try exercising or being more active’, 4 = ‘Watch a video, take a lesson, or have a coach teach you how to kick and catch’. |
| pa_is | Response to complete the first blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| pa_is_also | Response to complete the second blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| improve | Response to complete the third blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| increase | Response to complete the fourth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| when_cooling_down | Response to complete the fifth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘Strech’, 3 = ‘Endurance’, 4 = ‘Pulse’, 7 = ‘Good’, 8 = ‘Strength’. |
| heart_rate | Response to complete the sixth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| csappa1 | Response to the first question regarding predilection for physical activity: ‘Some kids don’t like playing active games (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids really like playing active games’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa2 | Response to the first question regarding adequacy for physical activity: ‘Some kids are good at active games (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids find active games hard to play’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa3 | Response to the second question regarding predilection for physical activity: ‘Some kids don’t have much fun playing sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids have a good time playing sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa4 | Response to the second question regarding adequacy for physical activity: ‘Some kids do well in most sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids feel they aren’t good at sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa5 | Response to the third question regarding predilection for physical activity: ‘Some kids don’t like playing sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids really enjoy playing sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa6 | Response to the third question regarding adequacy for physical activity: ‘Some kids learn to play active games easily (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids find it hard learning to play active games’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| why_active1 | Response to the question ‘Why are you active? I am active because… being active is fun’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| why_active2 | Response to the question ‘Why are you active? I am active because… I enjoy being active’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| why_active3 | Response to the question ‘Why are you active? I am active because… I like being active’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| feelings_about_pa1 | Response to the question ‘How do you feel about being active? When it comes to playing active games, I think I am pretty good.’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| feelings_about_pa2 | Response to the question ‘How do you feel about being active? I think I do well at activities compared to other children’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| feelings_about_pa3 | Response to the question ‘How do you feel about being active? When it comes to being active, I have good skills’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| self_report_pa | Number of days, over the past seven days, during which the participant self-reported being physically active for at least 60 minutes. |
| non_wear_time1 | Number of non-wear minutes on day 1. |
| non_wear_time2 | Number of non-wear minutes on day 2. |
| non_wear_time3 | Number of non-wear minutes on day 3. |
| non_wear_time4 | Number of non-wear minutes on day 4. |
| non_wear_time5 | Number of non-wear minutes on day 5. |
| non_wear_time6 | Number of non-wear minutes on day 6. |
| non_wear_time7 | Number of non-wear minutes on day 7. |
| wear_time1 | Number of wear minutes on day 1. |
| wear_time2 | Number of wear minutes on day 2. |
| wear_time3 | Number of wear minutes on day 3. |
| wear_time4 | Number of wear minutes on day 4. |
| wear_time5 | Number of wear minutes on day 5. |
| wear_time6 | Number of wear minutes on day 6. |
| wear_time7 | Number of wear minutes on day 7. |
| steps1 | Number of steps taken during day 1. |
| steps2 | Number of steps taken during day 2. |
| steps3 | Number of steps taken during day 3. |
| steps4 | Number of steps taken during day 4. |
| steps5 | Number of steps taken during day 5. |
| steps6 | Number of steps taken during day 6. |
| steps7 | Number of steps taken during day 7. |
| time_on1 | Dummy time of the start of the considered period of measurement on day 1. It is 06:00 by default. |
| time_on2 | Dummy time of the start of the considered period of measurement on day 2. It is 06:00 by default. |
| time_on3 | Dummy time of the start of the considered period of measurement on day 3. It is 06:00 by default. |
| time_on4 | Dummy time of the start of the considered period of measurement on day 4. It is 06:00 by default. |
| time_on5 | Dummy time of the start of the considered period of measurement on day 5. It is 06:00 by default. |
| time_on6 | Dummy time of the start of the considered period of measurement on day 6. It is 06:00 by default. |
| time_on7 | Dummy time of the start of the considered period of measurement on day 7. It is 06:00 by default. |
| time_off1 | Dummy time of the end of the considered period of measurement on day 1. It is obtained by adding estimated wearing time to 06:00. |
| time_off2 | Dummy time of the end of the considered period of measurement on day 2. It is obtained by adding estimated wearing time to 06:00. |
| time_off3 | Dummy time of the end of the considered period of measurement on day 3. It is obtained by adding estimated wearing time to 06:00. |
| time_off4 | Dummy time of the end of the considered period of measurement on day 4. It is obtained by adding estimated wearing time to 06:00. |
| time_off5 | Dummy time of the end of the considered period of measurement on day 5. It is obtained by adding estimated wearing time to 06:00. |
| time_off6 | Dummy time of the end of the considered period of measurement on day 6. It is obtained by adding estimated wearing time to 06:00. |
| time_off7 | Dummy time of the end of the considered period of measurement on day 7. It is obtained by adding estimated wearing time to 06:00. |
| pacer_laps_20m | Number of 20-meter laps completed during the PACER test. |
| pacer_score | CAPL-2 score corresponding to the performance at the PACER test. |
| pacer_interpretation | Interpretation of the score obtained at the PACER test. |
| plank_score | CAPL-2 score corresponding to the performance at the plank test. |
| plank_interpretation | Interpretation of the score obtained at the plank test. |
| camsa_time_score1 | Score corresponding to the time taken during the first trial of the CAMSA test. |
| camsa_time_score2 | Score corresponding to the time taken during the second trial of the CAMSA test. |
| camsa_skill_time_score1 | Total score obtained during the first trial of the CAMSA test. |
| camsa_skill_time_score2 | Total score obtained during the second trial of the CAMSA test. |
| camsa_score | CAPL-2 score corresponding to the CAMSA test. |
| camsa_interpretation | Interpretation of the score obtained at the CAMSA test. |
| pc_score | CAPL-2 score corresponding to the physical competence domain. |
| pc_interpretation | Interpretation of the score obtained for the physical competence domain. |
| pc_status | Completion status regarding physical competence domain assessment. |
| valid_days | Number of days with 10 hours or more of accelerometer wear. |
| step_average | Average daily step count obtained using the valid days. |
| step_score | CAPL-2 score corresponding to the average daily step count. |
| step_interpretation | Interpretation of the score obtained for the average daily step count. |
| self_report_pa_score | CAPL-2 score corresponding to self-reported physical activity. |
| db_score | CAPL-2 score corresponding to the daily behaviour domain. |
| db_interpretation | Interpretation of the score obtained for the daily behaviour domain. |
| db_status | Completion status regarding the daily behaviour domain assessment. |
| predilection_score | CAPL-2 score corresponding to predilection for physical activity. |
| adequacy_score | CAPL-2 score corresponding to adequacy with physical activity. |
| intrinsic_motivation_score | CAPL-2 score corresponding to intrinsic motivation for physical activity. |
| pa_competence_score | CAPL-2 score corresponding to self-efficacy in physical activity. |
| mc_score | CAPL-2 score corresponding to the motivation and confidence domain. |
| mc_interpretation | Interpretation of the score obtained for the motivation and confidence domain. |
| mc_status | Completion status regarding the motivation and confidence domain. |
| pa_guideline_score | CAPL-2 score corresponding to the questions about physical activity guidelines. |
| crf_means_score | CAPL-2 score corresponding to the question about cardiorespiratory fitness. |
| ms_means_score | CAPL-2 score corresponding to the question about muscular strength and endurance. |
| sports_skill_score | CAPL-2 score corresponding to the question about methods to improve sports skills. |
| fill_in_the_blanks_score | CAPL-2 score corresponding to the fill-in-the-blank text. |
| ku_score | CAPL-2 score corresponding to the knowledge and understanding domain. |
| ku_interpretation | Interpretation of the score obtained for the knowledge and understanding domain. |
| ku_status | Completion status regarding the knowledge and understanding domain. |
| capl_score | Total CAPL-2 score. |
| capl_interpretation | Interpretation of the total CAPL-2 score. |
| capl_status | Completion status regarding the whole CAPL-2 evaluation. |

### Codebook for Dataset 2: `capl_res_4_valid_days.csv`

This dataset contains the variables used to make comparisons of movement
behaviour metrics across the different physical literacy profiles. Only
the participants who had 4 valid days or more of accelerometer
measurement are present in the dataset.

| Variable | Definition |
|:---|:---|
| id | Participant identification number. Numbers go from 1 to 171. Missing identification numbers correspond to children who did not want to participate to the study or who did not complete any field test or questionnaire. |
| valid_days | Number of days with 10 hours or more of wear time. |
| wear_time | Daily average of wear time. |
| ig | Daily average of the intensity gradient. |
| alpha | Power-law exponent alpha. It provides information on the relative proportion of short and long sedentary bouts. The higher the alpha coefficient, the more the individual tends to accumulate sedentary time using relatively short bouts. Alpha is computed using all the sedentary bouts from the valid days of the measurement period using the formula provided by Chastin and Granat (2010; DOI: 10.1016/j.gaitpost.2009.09.002). |
| school | Name of the school where the participant was recruited. |
| gender | Sex of the participant. We are aware that gender is different from sex. While we used sex information reported by the participant, the package used to analyse physical literacy scores required a column called ‘gender’ to indicate whether the participant was a boy or a girl. |
| age | Participant age in years. |
| camsa_skill_score1 | Skill score obtained during the first pass of the CAMSA test. |
| camsa_time1 | Time taken during the first pass of the CAMSA test, in seconds. |
| camsa_skill_score2 | Skill score obtained during the second pass of the CAMSA test. |
| camsa_time2 | Time taken during the second pass of the CAMSA test, in seconds. |
| plank_time | Plank test performance, in seconds. |
| pacer_lap_distance | Lap distance used for the PACER test, in meters. |
| pacer_laps | Number of stages completed during the PACER test. |
| pa_guideline | Response to the question about physical activity guidelines: 1 = ‘20 minutes’, 2 = ‘30 minutes’, 3 = ‘60 minutes or 1 hour’, 4 = ‘120 minutes or 2 hours’. |
| crf_means | Response to the question about the definition of cardiorespiratory fitness: 1 = ‘How well the muscles can push, pull, or stretch’, 2 = ‘How well the heart can pump blood and the lungs can provide oxygen’, 3 = ‘Having a healthy weight for our height’, 4 = ‘Our ability to do sports that we like’. |
| ms_means | Response to question about the definition of muscular strength and endurance: 1 = ‘How well the muscles can push, pull, or stretch’, 2 = ‘How well the heart can pump blood and the lungs can provide oxygen’, 3 = ‘Having a healthy weight for our height’, 4 = ‘Our ability to do sports that we like’. |
| sports_skill | Response to the question about the method to improve one’s sports skills: 1 = ‘Read a book about kicking and catching a ball’, 2 = ‘Wait until you get older’, 3 = ‘Try exercising or being more active’, 4 = ‘Watch a video, take a lesson, or have a coach teach you how to kick and catch’. |
| pa_is | Response to complete the first blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| pa_is_also | Response to complete the second blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| improve | Response to complete the third blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| increase | Response to complete the fourth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| when_cooling_down | Response to complete the fifth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘Strech’, 3 = ‘Endurance’, 4 = ‘Pulse’, 7 = ‘Good’, 8 = ‘Strength’. |
| heart_rate | Response to complete the sixth blank in the fill-in-the-blank text: 1 = ‘fun’, 2 = ‘strech’, 3 = ‘endurance’, 4 = ‘pulse’, 7 = ‘good’, 8 = ‘strength’. |
| csappa1 | Response to the first question regarding predilection for physical activity: ‘Some kids don’t like playing active games (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids really like playing active games’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa2 | Response to the first question regarding adequacy for physical activity: ‘Some kids are good at active games (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids find active games hard to play’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa3 | Response to the second question regarding predilection for physical activity: ‘Some kids don’t have much fun playing sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids have a good time playing sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa4 | Response to the second question regarding adequacy for physical activity: ‘Some kids do well in most sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids feel they aren’t good at sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa5 | Response to the third question regarding predilection for physical activity: ‘Some kids don’t like playing sports (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids really enjoy playing sports’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| csappa6 | Response to the third question regarding adequacy for physical activity: ‘Some kids learn to play active games easily (1 = ’Really true for me’, 2 = ‘Sort of true for me’) BUT Other kids find it hard learning to play active games’ (3 = ‘Really true for me’, 4 = ‘Sort of true for me’). |
| why_active1 | Response to the question ‘Why are you active? I am active because… being active is fun’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| why_active2 | Response to the question ‘Why are you active? I am active because… I enjoy being active’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| why_active3 | Response to the question ‘Why are you active? I am active because… I like being active’: 1 = ‘Not true for me’, 2 = ‘Not really true for me’, 3 = ‘Sometimes true for me’, 4 = ‘Often true for me’, 5 = ‘Very true for me’. |
| feelings_about_pa1 | Response to the question ‘How do you feel about being active? When it comes to playing active games, I think I am pretty good.’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| feelings_about_pa2 | Response to the question ‘How do you feel about being active? I think I do well at activities compared to other children’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| feelings_about_pa3 | Response to the question ‘How do you feel about being active? When it comes to being active, I have good skills’: 1 = ‘Not like me at all’, 2 = ‘Not really like me’, 3 = ‘Sometimes like me’, 4 = ‘Quite a lot like me’, 5 = ‘Really like me’. |
| self_report_pa | Number of days, over the past seven days, during which the participant self-reported being physically active for at least 60 minutes. |
| non_wear_time1 | Number of non-wear minutes on day 1. |
| non_wear_time2 | Number of non-wear minutes on day 2. |
| non_wear_time3 | Number of non-wear minutes on day 3. |
| non_wear_time4 | Number of non-wear minutes on day 4. |
| non_wear_time5 | Number of non-wear minutes on day 5. |
| non_wear_time6 | Number of non-wear minutes on day 6. |
| non_wear_time7 | Number of non-wear minutes on day 7. |
| wear_time1 | Number of wear minutes on day 1. |
| wear_time2 | Number of wear minutes on day 2. |
| wear_time3 | Number of wear minutes on day 3. |
| wear_time4 | Number of wear minutes on day 4. |
| wear_time5 | Number of wear minutes on day 5. |
| wear_time6 | Number of wear minutes on day 6. |
| wear_time7 | Number of wear minutes on day 7. |
| steps1 | Number of steps taken during day 1. |
| steps2 | Number of steps taken during day 2. |
| steps3 | Number of steps taken during day 3. |
| steps4 | Number of steps taken during day 4. |
| steps5 | Number of steps taken during day 5. |
| steps6 | Number of steps taken during day 6. |
| steps7 | Number of steps taken during day 7. |
| time_on1 | Dummy time of the start of the considered period of measurement on day 1. It is 06:00 by default. |
| time_on2 | Dummy time of the start of the considered period of measurement on day 2. It is 06:00 by default. |
| time_on3 | Dummy time of the start of the considered period of measurement on day 3. It is 06:00 by default. |
| time_on4 | Dummy time of the start of the considered period of measurement on day 4. It is 06:00 by default. |
| time_on5 | Dummy time of the start of the considered period of measurement on day 5. It is 06:00 by default. |
| time_on6 | Dummy time of the start of the considered period of measurement on day 6. It is 06:00 by default. |
| time_on7 | Dummy time of the start of the considered period of measurement on day 7. It is 06:00 by default. |
| time_off1 | Dummy time of the end of the considered period of measurement on day 1. It is obtained by adding estimated wearing time to 06:00. |
| time_off2 | Dummy time of the end of the considered period of measurement on day 2. It is obtained by adding estimated wearing time to 06:00. |
| time_off3 | Dummy time of the end of the considered period of measurement on day 3. It is obtained by adding estimated wearing time to 06:00. |
| time_off4 | Dummy time of the end of the considered period of measurement on day 4. It is obtained by adding estimated wearing time to 06:00. |
| time_off5 | Dummy time of the end of the considered period of measurement on day 5. It is obtained by adding estimated wearing time to 06:00. |
| time_off6 | Dummy time of the end of the considered period of measurement on day 6. It is obtained by adding estimated wearing time to 06:00. |
| time_off7 | Dummy time of the end of the considered period of measurement on day 7. It is obtained by adding estimated wearing time to 06:00. |
| pacer_laps_20m | Number of 20-meter laps completed during the PACER test. |
| pacer_score | CAPL-2 score corresponding to the performance at the PACER test. |
| pacer_interpretation | Interpretation of the score obtained at the PACER test. |
| plank_score | CAPL-2 score corresponding to the performance at the plank test. |
| plank_interpretation | Interpretation of the score obtained at the plank test. |
| camsa_time_score1 | Score corresponding to the time taken during the first trial of the CAMSA test. |
| camsa_time_score2 | Score corresponding to the time taken during the second trial of the CAMSA test. |
| camsa_skill_time_score1 | Total score obtained during the first trial of the CAMSA test. |
| camsa_skill_time_score2 | Total score obtained during the second trial of the CAMSA test. |
| camsa_score | CAPL-2 score corresponding to the CAMSA test. |
| camsa_interpretation | Interpretation of the score obtained at the CAMSA test. |
| pc_score | CAPL-2 score corresponding to the physical competence domain. |
| pc_interpretation | Interpretation of the score obtained for the physical competence domain. |
| pc_status | Completion status regarding physical competence domain assessment. |
| step_average | Average daily step count obtained using the valid days. |
| step_score | CAPL-2 score corresponding to the average daily step count. |
| step_interpretation | Interpretation of the score obtained for the average daily step count. |
| self_report_pa_score | CAPL-2 score corresponding to self-reported physical activity. |
| db_score | CAPL-2 score corresponding to the daily behaviour domain. |
| db_interpretation | Interpretation of the score obtained for the daily behaviour domain. |
| db_status | Completion status regarding the daily behaviour domain assessment. |
| predilection_score | CAPL-2 score corresponding to predilection for physical activity. |
| adequacy_score | CAPL-2 score corresponding to adequacy with physical activity. |
| intrinsic_motivation_score | CAPL-2 score corresponding to intrinsic motivation for physical activity. |
| pa_competence_score | CAPL-2 score corresponding to self-efficacy in physical activity. |
| mc_score | CAPL-2 score corresponding to the motivation and confidence domain. |
| mc_interpretation | Interpretation of the score obtained for the motivation and confidence domain. |
| mc_status | Completion status regarding the motivation and confidence domain. |
| pa_guideline_score | CAPL-2 score corresponding to the questions about physical activity guidelines. |
| crf_means_score | CAPL-2 score corresponding to the question about cardiorespiratory fitness. |
| ms_means_score | CAPL-2 score corresponding to the question about muscular strength and endurance. |
| sports_skill_score | CAPL-2 score corresponding to the question about methods to improve sports skills. |
| fill_in_the_blanks_score | CAPL-2 score corresponding to the fill-in-the-blank text. |
| ku_score | CAPL-2 score corresponding to the knowledge and understanding domain. |
| ku_interpretation | Interpretation of the score obtained for the knowledge and understanding domain. |
| ku_status | Completion status regarding the knowledge and understanding domain. |
| capl_score | Total CAPL-2 score. |
| capl_interpretation | Interpretation of the total CAPL-2 score. |
| capl_status | Completion status regarding the whole CAPL-2 evaluation. |

## Licenses

### Code

The code of the pipeline is provided under GNU General Public License
Version 3.0 (please see LICENSE.md file).

### Data

<p xmlns:cc="http://creativecommons.org/ns#">

The raw data (accelerometer .agd files and Excel databases) and the
materials exported to the `out/` directory while running the pipeline
are licensed under
<a href="https://creativecommons.org/licenses/by-nc-nd/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative
Commons Attribution-NonCommercial-NoDerivatives 4.0
International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nd.svg?ref=chooser-v1" alt=""></a>
</p>
