
# Load packages ----
library(targets)
library(tarchetypes)

# Set target options ----
tar_option_set(
  packages = c(
    "activAnalyzer.batch",
    "capl",
    "correlation",
    "dplyr",
    "factoextra",
    "flextable",
    "forcats",
    "ggalluvial",
    "ggplot2",
    "ggrain",
    "gtsummary",
    "hms",
    "npmv",
    "patchwork",
    "purrr",
    "rankFD",
    "readr",
    "scales",
    "skimr",
    "tidyr"
    )
)

tar_source()

# Define pipeline ----
list(
  
# IMPORT & CLEAN DATA ----
  ## Set config file path (for accelerometer data analysis) ----
  tar_target(
    name = config_file,
    command = "config.csv",
    format = "file"
  ),
  
  ## Set BASE file path (for CAPL-2 data analysis) ----
  tar_target(
    name = base_file,
    command = "./data/BASE.xlsx",
    format = "file"
  ),
  
  ## Set DEMO file path (for demographic data analysis) ----
  tar_target(
    name = demo_file,
    command = "./data/DEMO.csv",
    format = "file"
  ),
  
  ## Set DATES file path (for accelerometer data analysis) ----
  tar_target(
    name = dates_file,
    command = "./data/DATES.csv",
    format = "file"
  ),
  
  ## Set AGD file directory path (for accelerometer data analysis) ----
  tar_target(
    name = agd_dir,
    command = "./data/agd/",
    format = "file"
  ),
  
  ## Get accelerometer data analysis config ----
  tar_target(
    name = pa_metrics_config,
    command = read_csv2(config_file)
  ),
  
  ## Make a table for accelerometer data analysis settings ----
  tar_target(
    name = tab_pa_metrics_config,
    command = pa_metrics_config |>
      select(-COMMENTS) |>
      filter(
        CODE_NAME %in% c(
          "AXIS_CHILD",
          "SED_CUTPOINT_CHILD", 
          "MPA_CUTPOINT_CHILD",     
          "VPA_CUTPOINT_CHILD",      
          "EPOCH_TARGET_CHILD",       
          "FRAME_CHILD",
          "ALLOWANCE_FRAME_CHILD",       
          "STREAM_FRAME_CHILD",     
          "VALID_WEAR_TIME_START",
          "VALID_WEAR_TIME_END",
          "MINIMUM_WEAR_TIME"
        )
      )
  ),
  
  ## Get movement behaviour metrics ----
  tar_target(
    name = pa_data,
    command = process_all_agd(
      agd_dir = agd_dir,
      config_path = config_file,
      demo_path = demo_file,
      dates_path = dates_file,
      id_config = 3,
      content = "option_3"
    )
    ),
  
  ## Import data ----
  ### Demographic data
  tar_target(
    name = DEMO, 
    command = read_csv2(demo_file)
  ),
  ### Physical skills
  tar_target(
    name = COMPT_PHY, 
    command = import_capl_data(base_file, sheet_name = "COMPT_PHY")
  ),
  ### Knowledge and understanding
  tar_target(
    name = CONN_COMPR, 
    command = import_capl_data(base_file, sheet_name = "CONN_COMPR")
  ),
  ### Motivation & confidence
  tar_target(
    name = MOTIV_CONF, 
    command = import_capl_data(base_file, sheet_name = "MOTIV_CONF")
  ),
  ### Self-reported physical activity
  tar_target(
    name = COMP_PHY, 
    command = import_capl_data(base_file, sheet_name = "COMP_PHY")
  ),
  
  ## Select and transpose steps and time data required for getting CAPL-2 results ----
  tar_target(
    name = PA_METRICS,
    command = pa_data$results_by_day |>
      group_by(id) |>
      mutate(
        num_day = seq_along(id),
        non_wear_time = 0
      ) |>
      ungroup() |>
      filter(num_day <= 7) |> # keep only the 7 first days of measurement
      select(id, num_day, non_wear_time, wear_time, total_steps) |>
      rename(steps = total_steps) |>
      pivot_wider(
        id_cols = id,
        names_from = num_day,
        values_from = c(non_wear_time:steps),
        names_sep = ""
      ) |>
      mutate( 
        # Set dummy starting wear times to allow CAPL-2 data analysis
        time_on1 = "06:00",
        time_on2 = "06:00",
        time_on3 = "06:00",
        time_on4 = "06:00",
        time_on5 = "06:00",
        time_on6 = "06:00",
        time_on7 = "06:00",
        # Set end wear times based on starting times and detected wear times obtained
        # with the {activAnalyzer.batch} package
        time_off1 = substr(as.character(as_hms(6 * 3600 + wear_time1 * 60)), 1, 5),
        time_off2 = substr(as.character(as_hms(6 * 3600 + wear_time2 * 60)), 1, 5),
        time_off3 = substr(as.character(as_hms(6 * 3600 + wear_time3 * 60)), 1, 5),
        time_off4 = substr(as.character(as_hms(6 * 3600 + wear_time4 * 60)), 1, 5),
        time_off5 = substr(as.character(as_hms(6 * 3600 + wear_time5 * 60)), 1, 5),
        time_off6 = substr(as.character(as_hms(6 * 3600 + wear_time6 * 60)), 1, 5),
        time_off7 = substr(as.character(as_hms(6 * 3600 + wear_time7 * 60)), 1, 5)
      ) |> 
      rename(identifiant = id)
  ), 
  
  ## Combine all datasets ----
  tar_target(
    name = df_raw,
    command = list(DEMO, COMPT_PHY, CONN_COMPR, MOTIV_CONF, COMP_PHY, PA_METRICS) |>
      reduce(full_join, by = c("identifiant"))
  ),
  
  ## Recode, rename, and select data ----
  tar_target(
    name = df_cleaned,
    command = df_raw |>
      mutate(
        across(c(identifiant, ecole, genre), as.factor),
        genre = fct_recode(
          genre,
          "girl" = "F",
          "boy" = "H"
          ),
        self_report_pa = ifelse(self_report_pa == 0, 1, self_report_pa),
        Q1_L1 = case_when(
          Q1_L1 == "Certains jeunes n aiment pas jouer a des jeux actifs TRES VRAI"   ~ 1,
          Q1_L1 == "Certains jeunes n aiment pas jouer a des jeux actifs PLUTOT VRAI" ~ 2,
          Q1_L1 == "D autres jeunes aiment beaucoup les jeux actifs TRES VRAI"        ~ 3,
          Q1_L1 == "D autres jeunes aiment beaucoup les jeux actifs PLUTOT VRAI"      ~ 4
        ),
        Q1_L2 = case_when(
          Q1_L2 == "Certains jeunes sont bons aux jeux actifs TRES VRAI"                      ~ 1,
          Q1_L2 == "Certains jeunes sont bons aux jeux actifs PLUTOT VRAI"                    ~ 2,
          Q1_L2 == "D autres jeunes trouvent que les jeux actifs sont difficiles TRES VRAI"   ~ 3,
          Q1_L2 == "D autres jeunes trouvent que les jeux actifs sont difficiles PLUTOT VRAI" ~ 4
        ),
        Q1_L3 = case_when(
          Q1_L3 == "Certains jeunes n ont pas plaisir a faire du sport TRES VRAI"   ~ 1,
          Q1_L3 == "Certains jeunes n ont pas plaisir a faire du sport PLUTOT VRAI" ~ 2,
          Q1_L3 == "D autres jeunes ont du plaisir a faire du sport TRES VRAI"      ~ 3,
          Q1_L3 == "D autres jeunes ont du plaisir a faire du sport PLUTOT VRAI"    ~ 4
        ),
        Q1_L4 = case_when(
          Q1_L4 == "Certains jeunes sont bons dans la plupart des sports TRES VRAI"                    ~ 1,
          Q1_L4 == "Certains jeunes sont bons dans la plupart des sports PLUTOT VRAI"                  ~ 2,
          Q1_L4 == "D autres jeunes ont l impression qu ils ne sont pas bon dans le sport TRES VRAI"   ~ 3,
          Q1_L4 == "D autres jeunes ont l impression qu ils ne sont pas bon dans le sport PLUTOT VRAI" ~ 4
        ),
        Q1_L5 = case_when(
          Q1_L5 == "Certains jeunes n aiment pas faire du sport TRES VRAI"      ~ 1,
          Q1_L5 == "Certains jeunes n aiment pas faire du sport PLUTOT VRAI"    ~ 2,
          Q1_L5 == "D autres jeunes aiment beaucoup faire du sport TRES VRAI"   ~ 3,
          Q1_L5 == "D autres jeunes aiment beaucoup faire du sport PLUTOT VRAI" ~ 4
        ),
        Q1_L6 = case_when(
          Q1_L6 == "Certains jeunes apprenent facilement les jeux actifs TRES VRAI"                               ~ 1,
          Q1_L6 == "Certains jeunes apprenent facilement les jeux actifs PLUTOT VRAI"                             ~ 2,
          Q1_L6 == "D autres jeunes trouvent que c est difficile d apprendre a jouer aux jeux actifs TRES VRAI"   ~ 3,
          Q1_L6 == "D autres jeunes trouvent que c est difficile d apprendre a jouer aux jeux actifs PLUTOT VRAI" ~ 4
        ),
        Q2_L1 = case_when(
          Q2_L1 == "Pas vrai pour moi"          ~ 1,
          Q2_L1 == "Pas vraiment vrai pour moi" ~ 2,
          Q2_L1 == "Parfois vrai pour moi"      ~ 3,
          Q2_L1 == "Souvent vrai pour moi"      ~ 4,
          Q2_L1 == "Tres vrai pour moi"         ~ 5
        ),
        Q2_L2 = case_when(
          Q2_L2 == "Pas vrai pour moi"          ~ 1,
          Q2_L2 == "Pas vraiment vrai pour moi" ~ 2,
          Q2_L2 == "Parfois vrai pour moi"      ~ 3,
          Q2_L2 == "Souvent vrai pour moi"      ~ 4,
          Q2_L2 == "Tres vrai pour moi"         ~ 5
        ),
        Q2_L3 = case_when(
          Q2_L3 == "Pas vrai pour moi"          ~ 1,
          Q2_L3 == "Pas vraiment vrai pour moi" ~ 2,
          Q2_L3 == "Parfois vrai pour moi"      ~ 3,
          Q2_L3 == "Souvent vrai pour moi"      ~ 4,
          Q2_L3 == "Tres vrai pour moi"         ~ 5
        ),
        Q3_L1 = case_when(
          Q3_L1 == "Ne me represente pas du tout"  ~ 1,
          Q3_L1 == "Ne me represente pas vraiment" ~ 2,
          Q3_L1 == "Me represente parfois"         ~ 3,
          Q3_L1 == "Me represente pas mal"         ~ 4,
          Q3_L1 == "Me represente beaucoup"        ~ 5
        ),
        Q3_L2 = case_when(
          Q3_L2 == "Ne me represente pas du tout"  ~ 1,
          Q3_L2 == "Ne me represente pas vraiment" ~ 2,
          Q3_L2 == "Me represente parfois"         ~ 3,
          Q3_L2 == "Me represente pas mal"         ~ 4,
          Q3_L2 == "Me represente beaucoup"        ~ 5
        ),
        Q3_L3 = case_when(
          Q3_L3 == "Ne me represente pas du tout"  ~ 1,
          Q3_L3 == "Ne me represente pas vraiment" ~ 2,
          Q3_L3 == "Me represente parfois"         ~ 3,
          Q3_L3 == "Me represente pas mal"         ~ 4,
          Q3_L3 == "Me represente beaucoup"        ~ 5
        ),
        `Réponse Q.1` = case_when(
          `Réponse Q.1` == "a) 20 minutes"               ~ 1,
          `Réponse Q.1` == "b) 30 minutes"               ~ 2,
          `Réponse Q.1` == "c) 60 minutes ou 1 heure"    ~ 3,
          `Réponse Q.1` == "d) 120 minutes ou 2 heures"  ~ 4
        ),
        `Réponse Q.2` = case_when(
          `Réponse Q.2` == "a) A quel point les muscles peuvent bien pousser, tirer ou s'étirer"                               ~ 1,
          `Réponse Q.2` == "b) A quel point le cœur peut bien pomper le sang et les poumons peuvent bien fournir de l'oxygène" ~ 2,
          `Réponse Q.2` == "c)  Avoir un poids santé par rapport à sa taille."                                                 ~ 3,
          `Réponse Q.2` == "d) Notre capacité à pratiquer des sports que l'on aime."                                           ~ 4
        ),
        `Réponse Q.3` = case_when(
          `Réponse Q.3` == "a) A quel point les muscles peuvent bien pousser, tirer ou s'étirer"                                ~ 1,
          `Réponse Q.3` == "b) A quel point le cœur peut bien pomper le sang et les poumons peuvent bien fournir de l'oxygène"  ~ 2,
          `Réponse Q.3` == "c)  Avoir un poids santé par rapport à sa taille."                                                  ~ 3,
          `Réponse Q.3` == "d) Notre capacité à pratiquer des sports que l'on aime."                                            ~ 4
        ),
        `Réponse Q.4` = case_when(
          `Réponse Q.4` == "a) Lire un livre qui explique comment frapper ou attraper un ballon"                                                     ~ 1,
          `Réponse Q.4` == "b) Attendre d'être vieux"                                                                                                ~ 2,
          `Réponse Q.4` == "c) Essayer de faire de l'exercice ou d'être beaucoup actif"                                                              ~ 3,
          `Réponse Q.4` == "d) Regarder une vidéo, suivre un cours ou demander à un entraineur de t'apprendre comment frapper et attraper un ballon" ~ 4
        ),
        `Réponse Q.5 - 1er espace` = case_when(
          `Réponse Q.5 - 1er espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 1er espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 1er espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 1er espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 1er espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 1er espace` == "Le pouls"      ~ 4
        ),
        `Réponse Q.5 - 2eme espace` = case_when(
          `Réponse Q.5 - 2eme espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 2eme espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 2eme espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 2eme espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 2eme espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 2eme espace` == "Le pouls"      ~ 4
        ),
        `Réponse Q.5 - 3eme espace` = case_when(
          `Réponse Q.5 - 3eme espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 3eme espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 3eme espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 3eme espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 3eme espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 3eme espace` == "Le pouls"      ~ 4
        ),
        `Réponse Q.5 - 4eme espace` = case_when(
          `Réponse Q.5 - 4eme espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 4eme espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 4eme espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 4eme espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 4eme espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 4eme espace` == "Le pouls"      ~ 4
        ),
        `Réponse Q.5 - 5eme espace` = case_when(
          `Réponse Q.5 - 5eme espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 5eme espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 5eme espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 5eme espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 5eme espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 5eme espace` == "Le pouls"      ~ 4
        ),
        `Réponse Q.5 - 6eme espace` = case_when(
          `Réponse Q.5 - 6eme espace` == "Amusante"      ~ 1,
          `Réponse Q.5 - 6eme espace` == "Bonne"         ~ 7,
          `Réponse Q.5 - 6eme espace` == "Son endurance" ~ 3,
          `Réponse Q.5 - 6eme espace` == "Sa force"      ~ 8,
          `Réponse Q.5 - 6eme espace` == "S'étirer"      ~ 2,
          `Réponse Q.5 - 6eme espace` == "Le pouls"      ~ 4
        )
      ) |>
      rename(
        id = identifiant,
        school = ecole,
        gender = genre,
        pacer_laps = PACER,
        plank_time = PLANCHE,
        camsa_skill_score1 = "CAMSA HAB 1",
        camsa_time1 = "CAMSA TEMPS 1",
        camsa_skill_score2 = "CAMSA HAB 2",
        camsa_time2 = "CAMSA TEMPS 2",
        csappa1 = Q1_L1,
        csappa2 = Q1_L2,
        csappa3 = Q1_L3,
        csappa4 = Q1_L4,
        csappa5 = Q1_L5,
        csappa6 = Q1_L6,
        why_active1 = Q2_L1,
        why_active2 = Q2_L2,
        why_active3 = Q2_L3,
        feelings_about_pa1 = Q3_L1,
        feelings_about_pa2 = Q3_L2,
        feelings_about_pa3 = Q3_L3,
        pa_guideline = "Réponse Q.1",
        crf_means = "Réponse Q.2",
        ms_means = "Réponse Q.3",
        sports_skill = "Réponse Q.4",
        pa_is = "Réponse Q.5 - 1er espace",
        pa_is_also = "Réponse Q.5 - 2eme espace",
        improve = "Réponse Q.5 - 3eme espace",
        increase = "Réponse Q.5 - 4eme espace",
        when_cooling_down = "Réponse Q.5 - 5eme espace",
        heart_rate = "Réponse Q.5 - 6eme espace"
      ) |>
      mutate(
        across(c(camsa_skill_score1:self_report_pa), as.integer)
      ) |> 
      ## The 'poids' ('weight' when translated to french) column was initially 
      ## added because it was needed for the activAnalyzer.batch functions. It
      ## can now be removed because we used no weight data.
      select(-poids) |> 
      arrange(id)
  ), 
  
# GLOBAL DESCRIPTIVE ANALYSIS ----

  ## Get a table with the percentages of participants per number of valid days for ----
  ## the accelerometer-based measurement of movement behaviours ----
  tar_target(
    name = tab_percents_num_valid_days,
    command = pa_data$all_metrics |>
      count(valid_days) |>
      mutate(
        prop = format(janitor::round_half_up(n / sum(n) * 100, digits = 1), nsmall = 1),
        n = as.character(n)
      ) |>
      rename(
        "N" = n,
        "%" = prop
      ) |> 
      pivot_longer(cols = c("N", "%"), names_to = " ", values_to = "stat") |> 
      pivot_wider(names_from = valid_days, values_from = stat) |> 
      flextable() |>
      bold(part = "header") |>
      add_header_row(
        values = c("", "Number of valid days of accelerometer wear"),
        colwidths = c(1, 7),
        top = TRUE
      ) |> 
      align(i = 1, align = "left", part = "header") |> 
      align(i = 2, align = "left", part = "header") |> 
      align(i = 1:2, align = "left", part = "body") |> 
      italic(i = 1, j = 1)
    
  ),

  ## Get the proportion of participants with at least 4 valid days of measurement
  ## of movement behaviours
  tar_target(
    name = prop_4_valid_days,
    command = (
      pa_data$all_metrics |> 
        mutate(if_4_valid_days = ifelse(valid_days >= 4, "yes", "no")) |> 
        count(if_4_valid_days) |> 
        mutate(prop = format(janitor::round_half_up(n / sum(n) * 100, digits = 1), nsmall = 1))
      )[2, 3]
  ),

  ## Get CAPL-2 results ----
  tar_target(
    name = capl_res,
    command =
      { 
        
        ### Set seed
        set.seed(123)
        
        ### Get initial CAPL-2 results
        capl_res <- get_capl(df_cleaned)
        
        ### The capl() function failed to compute scores for ID 55, thus requiring
        ### manual computations
        
        #### Update Predilection score for ID 55
        predilection_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "predilection_score"][[1]], digits = 3))
        
        #### Update Adequacy score for ID 55
        adequacy_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "adequacy_score"][[1]]), digits = 3)
        
        #### Update Intrinsic motivation score for ID 55
        intrinsic_motivation_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "intrinsic_motivation_score"][[1]]), digits = 3)
        
        #### Update PA competence score for ID 55
        pa_competence_score_55 <-
          capl_res[capl_res$id == "55", "pa_competence_score"][[1]]
        
        ### Update MC score for ID 55
        capl_res[capl_res$id == "55", "mc_score"] <-
          get_mc_score(
            predilection_score_55,
            adequacy_score_55,
            intrinsic_motivation_score_55,
            pa_competence_score_55
          )[[1]]
        
        #### Update MC score interpretation for ID 55
        capl_res[capl_res$id == "55", "mc_interpretation"] <-
          get_capl_interpretation(capl_res[capl_res$id == "55", "age"][[1]],
                                  capl_res[capl_res$id == "55", "gender"][[1]],
                                  capl_res[capl_res$id == "55", "mc_score"][[1]],
                                  "mc")[[1]]
        
        #### Update CAPL-2 status for ID 55
        capl_res[capl_res$id == "55", "capl_status"] <- "complete"
       
        ### Update all CAPL-2 status and interpretation variables
        capl_res <- 
          capl_res |> 
          mutate(
            capl_score = ifelse(capl_status != "complete", NA, capl_score),
            capl_interpretation = ifelse(capl_status != "complete", NA, capl_interpretation),
            across(where(~is.character(.x)), as.factor),
            across(
              c(
              capl_interpretation,
              pacer_interpretation,
              camsa_interpretation,
              plank_interpretation,
              pc_interpretation,
              step_interpretation,
              db_interpretation,
              mc_interpretation,
              ku_interpretation
              ),
              # Please see the recode_capl_interpretation_vars.R file to understand 
              # the function shown below
              recode_capl_interpretation_vars
            )
          )
        }
  ), 
  
  ## Get the figure showing the CAPL-2 scores ----
  tar_target(
    name = p_capl_all_domains,
    
    ### This section heavily uses a personal plotting function (see
    ### plot_score_distri.R file)
    
    command = 
      {
        
        ### Get figures for PC scores ----
        #### Select PC scores
        pc_scores <-
          capl_res |>
          select(
            id,
            pacer_score,
            camsa_score,
            plank_score,
            pc_score
            ) |>
          pivot_longer(
            cols = pacer_score:pc_score,
            names_to = "Item",
            values_to = "Score"
            ) |>
          mutate(
            Item = fct_relevel(Item, "pacer_score", "camsa_score", "plank_score", "pc_score"),
            Item = fct_recode(
              Item,
              "PACER Shuttle Run (/10)" = "pacer_score",
              "CAMSA Score (/10)" = "camsa_score",
              "Plank (/10)" = "plank_score",
              "Physical Competence (/30)" = "pc_score"
            )
          ) 
        
        #### Get the numbers of students with valid data for each item
        n_pc_scores <-
          pc_scores |>
          group_by(Item) |>
          summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
        
        #### Get plots with score distributions
        ##### Pacer
        p_pc_1 <-
          plot_score_distri(
            data1 = pc_scores,
            data2 = n_pc_scores,
            item = "PACER Shuttle Run (/10)",
            color = "#333378",
            text_y = 0.45,
            breaks_x = seq(0, 10, 2),
            limits_x = c(0, 10)
          )
        
        ##### CAMSA
        p_pc_2 <-
          plot_score_distri(
            data1 = pc_scores,
            data2 = n_pc_scores,
            item = "CAMSA Score (/10)",
            color = "#333378",
            text_y = 0.45,
            breaks_x = seq(0, 10, 2),
            limits_x = c(0, 10)
          )
        
        ##### Plank
        p_pc_3 <-
          plot_score_distri(
            data1 = pc_scores,
            data2 = n_pc_scores,
            item = "Plank (/10)",
            color = "#333378",
            text_y = 0.45,
            breaks_x = seq(0, 10, 2),
            limits_x = c(0, 10)
          )
        
        ##### PC score
        p_pc_4 <-
          plot_score_distri(
            data1 = pc_scores,
            data2 = n_pc_scores,
            item = "Physical Competence (/30)",
            color = "#333378",
            text_y = 0.45,
            breaks_x = seq(0, 30, 10),
            limits_x = c(0, 30)
          )
        
        ### Get figures for DB scores ----
        #### Select DB scores
        db_scores <-
          capl_res |>
          select(
            id,
            step_score,
            self_report_pa_score,
            db_score
            ) |>
          pivot_longer(
            cols = step_score:db_score,
            names_to = "Item",
            values_to = "Score"
            ) |>
          mutate(
            Item = fct_relevel(Item, "step_score", "self_report_pa_score", "db_score"),
            Item = fct_recode(
              Item,
              "Average Daily Sept Count (/25)" = "step_score",
              "Self-Rep. Num. of Days with MVPA (/5)" = "self_report_pa_score",
              "Daily Behaviour (/30)" = "db_score"
            )
          ) 
        
        #### Get the numbers of students with valid data for each item
        n_db_scores <-
          db_scores |>
          group_by(Item) |>
          summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
        
        #### Get plots with score distributions
        ##### Steps
        p_db_1 <-
          plot_score_distri(
            data1 = db_scores,
            data2 = n_db_scores,
            item = "Average Daily Sept Count (/25)",
            color = "#9C8E84",
            text_y = 0.47,
            breaks_x = seq(0, 25, 5),
            limits_x = c(0, 25)
          )
        
        ##### Self-reported PA
        p_db_2 <-
          plot_score_distri(
            data1 = db_scores,
            data2 = n_db_scores,
            item = "Self-Rep. Num. of Days with MVPA (/5)",
            color = "#9C8E84",
            text_y = 0.47,
            breaks_x = seq(0, 5, 1),
            limits_x = c(0, 5)
          )
        
        ##### DB score
        p_db_3 <-
          plot_score_distri(
            data1 = db_scores,
            data2 = n_db_scores,
            item = "Daily Behaviour (/30)",
            color = "#9C8E84",
            text_y = 0.47,
            breaks_x = seq(0, 30, 10),
            limits_x = c(0, 30)
          )
        
        ### Get figures for MC scores ----
        #### Select MC scores
        mc_scores <-
          capl_res |>
          select(
            id,
            predilection_score,
            adequacy_score,
            intrinsic_motivation_score,
            pa_competence_score,
            mc_score
          ) |>
          pivot_longer(cols = predilection_score:mc_score,
                       names_to = "Item",
                       values_to = "Score") |>
          mutate(
            Item = fct_relevel(
              Item,
              "intrinsic_motivation_score",
              "pa_competence_score",
              "predilection_score",
              "adequacy_score",
              "mc_score"
            ),
            Item = fct_recode(
              Item,
              "Intrinsic Motivation (/7.5)" = "intrinsic_motivation_score",
              "Competence (/7.5)" = "pa_competence_score",
              "Predilection (/7.5)" = "predilection_score",
              "Adequacy (/7.5)" = "adequacy_score",
              "Motivation and Confidence (/30)" = "mc_score"
            )
          )
        
        #### Get the numbers of students with valid data for each item
        n_mc_scores <-
          mc_scores |>
          group_by(Item) |>
          summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
        
        #### Get plots with score distributions
        ##### Intrinsic motivation
        p_mc_1 <-
          plot_score_distri(
            data1 = mc_scores,
            data2 = n_mc_scores,
            item = "Intrinsic Motivation (/7.5)",
            color = "#EF6723",
            text_y = 0.45,
            breaks_x = seq(0, 7.5, 1.5),
            limits_x = c(0, 7.5)
          )
        
        ##### Competence
        p_mc_2 <-
          plot_score_distri(
            data1 = mc_scores,
            data2 = n_mc_scores,
            item = "Competence (/7.5)",
            color = "#EF6723",
            text_y = 0.45,
            breaks_x = seq(0, 7.5, 1.5),
            limits_x = c(0, 7.5)
          )
        
        ##### Predilection
        p_mc_3 <-
          plot_score_distri(
            data1 = mc_scores,
            data2 = n_mc_scores,
            item = "Predilection (/7.5)",
            color = "#EF6723",
            text_y = 0.45,
            breaks_x = seq(0, 7.5, 1.5),
            limits_x = c(0, 7.5)
          )
        
        ##### Adequacy
        p_mc_4 <-
          plot_score_distri(
            data1 = mc_scores,
            data2 = n_mc_scores,
            item = "Adequacy (/7.5)",
            color = "#EF6723",
            text_y = 0.45,
            breaks_x = seq(0, 7.5, 1.5),
            limits_x = c(0, 7.5)
          )
        
        ##### MC score
        p_mc_5 <-
          plot_score_distri(
            data1 = mc_scores,
            data2 = n_mc_scores,
            item = "Motivation and Confidence (/30)",
            color = "#EF6723",
            text_y = 0.45,
            breaks_x = seq(0, 30, 10),
            limits_x = c(0, 30)
          )
        
        ### Get figures for KU scores ----
        #### Select KU scores
        ku_scores <-
          capl_res |>
          select(
            id,
            fill_in_the_blanks_score,
            pa_guideline_score,
            crf_means_score,
            ms_means_score,
            sports_skill_score,
            ku_score
          ) |>
          pivot_longer(cols = fill_in_the_blanks_score:ku_score,
                       names_to = "Item",
                       values_to = "Score") |>
          mutate(
            Item = fct_relevel(
              Item,
              "fill_in_the_blanks_score",
              "pa_guideline_score",
              "crf_means_score",
              "ms_means_score",
              "sports_skill_score",
              "ku_score"
            ),
            Item = fct_recode(
              Item,
              "PA Comprehension and Understanding (/6)" = "fill_in_the_blanks_score",
              "Daily PA Guidelines (/1)" = "pa_guideline_score",
              "Cardiorespiratory Fitness Definition (/1)" = "crf_means_score",
              "Muscular Strength & Endurance Definition (/1)" = "ms_means_score",
              "Improve Sport Skill (/1)" = "sports_skill_score",
              "Knowledge and Understanding (/10)" = "ku_score"
            )
          )
        
        #### Get the numbers of students with valid data for each item
        n_ku_scores <-
          ku_scores |>
          group_by(Item) |>
          summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
        
        #### Get plots with score distributions
        ##### Physical activity comprehension and understanding
        p_ku_1 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            item = "PA Comprehension and Understanding (/6)",
            color = "#00A79F",
            text_y = 0.45,
            breaks_x = seq(0, 6, 2),
            limits_x = c(0, 6)
          )
        
        ##### Daily PA guidelines
        p_ku_2 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            type = "disc",
            item = "Daily PA Guidelines (/1)",
            color = "#00A79F",
            breaks_x = seq(0, 1, 1),
            limits_x = c(0, 1)
          )
        
        ##### Cardiorespiratory fitness definition
        p_ku_3 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            type = "disc",
            item = "Cardiorespiratory Fitness Definition (/1)",
            color = "#00A79F",
            breaks_x = seq(0, 1, 1),
            limits_x = c(0, 1)
          )
        
        ##### Muscular strength & endurance definition
        p_ku_4 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            type = "disc",
            item = "Muscular Strength & Endurance Definition (/1)",
            color = "#00A79F",
            breaks_x = seq(0, 1, 1),
            limits_x = c(0, 1)
          )
        
        ##### Improve sport skill
        p_ku_5 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            type = "disc",
            item = "Improve Sport Skill (/1)",
            color = "#00A79F",
            breaks_x = seq(0, 1, 1),
            limits_x = c(0, 1)
          )
        
        ##### KU score
        p_ku_6 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            item = "Knowledge and Understanding (/10)",
            color = "#00A79F",
            text_y = 0.45,
            breaks_x = seq(0, 10, 2),
            limits_x = c(0, 10)
          )
        
        ### Get figure for CAPL-2 ----
        #### Select data
        capl_score <-
          capl_res |>
          select(id, capl_score) |>
          pivot_longer(
            cols = capl_score,
            names_to = "Item",
            values_to = "Score"
            ) |>
          mutate(Item = fct_recode(Item, "Physical Literacy (/100)" = "capl_score"))
        
        #### Get the numbers of students with valid data
        n_capl_score <-
          capl_score |>
          group_by(Item) |>
          summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
        
        #### Get plot with score distribution
        p_capl <-
          plot_score_distri(
            data1 = capl_score,
            data2 = n_capl_score,
            item = "Physical Literacy (/100)",
            color = "#D4D11B",
            text_y = 0.45,
            breaks_x = seq(0, 100, 20),
            limits_x = c(0, 100)
          )
        
        ### Get final figure ----
        #### Build blank plot
        blank_plot <-
          plot_score_distri(
            data1 = pc_scores,
            data2 = n_pc_scores,
            item = "PACER Shuttle Run (/10)",
            color = "white",
            text_y = 0.45,
            breaks_x = seq(0, 10, 2),
            limits_x = c(0, 10)
          ) +
          geom_rect(
            aes(
              xmin = -2,
              xmax = 20,
              ymin = -2,
              ymax = 100
            ),
            fill  = "white",
            color = "white"
          ) 
        
        #### Gather domain plots
        p_pl_domains <-
          p_pc_4      + p_db_3      + p_mc_5     +  p_ku_6 +
          p_pc_1      + p_db_1      + p_mc_1     +  p_ku_1 +
          p_pc_2      + p_db_2      + p_mc_2     +  p_ku_2 +
          p_pc_3      + blank_plot  + p_mc_3     +  p_ku_3 +
          blank_plot  + blank_plot  + p_mc_4     +  p_ku_4 +
          blank_plot  + blank_plot  + blank_plot +  p_ku_5 +
          plot_layout(
            nrow = 6,
            byrow = TRUE,
            axis_titles  = "collect"
          )
        
        #### Build final figure
        p_capl_all_domains <-
          (plot_spacer() + (p_capl + labs(y = "")) + plot_spacer()) / p_pl_domains + plot_layout(heights = c(1, 6))
        
        #### Return final figure
        p_capl_all_domains
        
      }
  ),
  
  ## Get CAPL-2 descriptive statistics ----
  tar_target(
    name = desc_stats_capl_all,
    command = capl_res |> 
      tbl_summary(
        include = c(
          pc_score, 
          pacer_score,
          camsa_score,
          plank_score,
          db_score, 
          step_score,
          self_report_pa_score,
          mc_score, 
          intrinsic_motivation_score,
          pa_competence_score,
          predilection_score,
          adequacy_score,
          ku_score, 
          fill_in_the_blanks_score,
          pa_guideline_score,
          crf_means_score,
          ms_means_score,
          sports_skill_score,
          capl_score
          ),
        label = list(
          pc_score = "Physical competence (/30)",
          pacer_score = "PACER shuttle run (/10)",
          camsa_score = "CAMSA (/10)",
          plank_score = "Plank (/10)",
          db_score = "Daily behaviour (/30)",
          step_score = "Average daily step count (/25)",
          self_report_pa_score = "Self-reported number of days with MVPA (/5)",
          mc_score = "Motivation and confidence (/30)",
          intrinsic_motivation_score = "Intrinsic motivation (/7.5)",
          pa_competence_score = "Competence (/7.5)",
          predilection_score = "Predilection (/7.5)",
          adequacy_score = "Adequacy (/7.5)",
          ku_score = "Knowledge and understanding (/10)",
          fill_in_the_blanks_score = "PA comprehension and understanding (/6)",
          pa_guideline_score = "Daily PA guidelines (/1)",
          crf_means_score = "Cardiorespiratory fitness definition (/1)",
          ms_means_score = "Muscular strength and endurance definition (/1)",
          sports_skill_score = "Improve sport skill (/1)",
          capl_score = "Physical literacy (/100)"
        ),
        missing = "no",
        statistic = list(
          all_continuous() ~ "{median} ({p25} - {p75})  \n{mean} ± {sd}"
        ),
        type = list(
          self_report_pa_score = "continuous",
          ku_score = "continuous",
          fill_in_the_blanks_score = "continuous"
        ),
        digits = list(all_continuous() ~ 1)
      ) |> 
      modify_header(
        list(
          label = c("**Score**"),
          stat_0 = "**Statistics**"
        )
      ) |> 
      modify_footnote(all_stat_cols() ~ "Median (Q1 - Q3), mean ± SD, or n (%) of participants who obtained a score of 1/1.") |> 
      add_n()
  ),
  
  ## Get CAPL-2 interpretation statistics ----
  tar_target(
    name = p_interpretation, 
    command = capl_res |>
      select(c(id, ends_with("interpretation"))) |>
      pivot_longer(
        cols = c(pacer_interpretation:capl_interpretation),
        names_to = "score",
        values_to = "interpretation"
      ) |>
      mutate(
        score = as.factor(score),
        score = fct_relevel(
          score,
          "pacer_interpretation",
          "camsa_interpretation",
          "plank_interpretation",
          "pc_interpretation",
          "step_interpretation",
          "db_interpretation",
          "mc_interpretation",
          "ku_interpretation",
          "capl_interpretation"
        ),
        score = fct_recode(
          score,
          "PACER Shuttle Run" = "pacer_interpretation",
          "CAMSA Score" = "camsa_interpretation",
          "Plank" = "plank_interpretation",
          "Physical Competence" = "pc_interpretation",
          "Average Daily Step Count" = "step_interpretation",
          "Daily Behaviour" = "db_interpretation",
          "Motivation and Confidence" = "mc_interpretation",
          "Knowledge and Understanding" = "ku_interpretation",
          "Physical Literacy" = "capl_interpretation"
        )
      ) |>
      count(score, interpretation, .drop = FALSE) |>
      group_by(score) |>
      mutate(prop = janitor::round_half_up(n / sum(n) * 100)) |>
      ggplot(aes(x = interpretation, y = prop)) +
      geom_bar(stat = "identity", aes(fill = interpretation)) +
      geom_text(aes(label = paste0(format(prop, nsmall = 1), "%")), size = 3, vjust = -0.3) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "", y = "%", fill = "Interpretation") +
      facet_wrap(~ score) +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold")
      )
    ),
  
  ## Get the figure showing the CAPL-2 scores by sex ----
  tar_target(
    name = p_capl_all_domains_by_sex,
    
    ### This section heavily uses a personal plotting function (see
    ### plot_score_distri.R file)
    
    command = {
      
      ### Get figures for PC scores ----
      #### Select PC scores
      pc_scores_by_sex <-
        capl_res |>
        select(
          id,
          gender,
          pacer_score,
          camsa_score,
          plank_score,
          pc_score
          ) |>
        pivot_longer(
          cols = pacer_score:pc_score,
          names_to = "Item",
          values_to = "Score"
          ) |>
        mutate(
          Item = fct_relevel(Item, "pacer_score", "camsa_score", "plank_score", "pc_score"),
          Item = fct_recode(
            Item,
            "PACER Shuttle Run (/10)" = "pacer_score",
            "CAMSA Score (/10)" = "camsa_score",
            "Plank (/10)" = "plank_score",
            "Physical Competence (/30)" = "pc_score"
          )
        ) 
      
      #### Get the numbers of students with valid data for each item
      n_pc_scores_by_sex <-
        pc_scores_by_sex |>
        group_by(gender, Item) |>
        summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
      
      #### Get plots with score distributions
      ##### Pacer
      p_pc_1_by_sex  <-
        plot_score_distri(
          data1 = pc_scores_by_sex ,
          data2 = n_pc_scores_by_sex ,
          item = "PACER Shuttle Run (/10)",
          by_sex = "yes",
          color = "#333378",
          text_y = 1.55,
          breaks_x = seq(0, 10, 2),
          limits_x = c(0, 10)
        )
      
      ##### CAMSA
      p_pc_2_by_sex  <-
        plot_score_distri(
          data1 = pc_scores_by_sex ,
          data2 = n_pc_scores_by_sex ,
          item = "CAMSA Score (/10)",
          by_sex = "yes",
          color = "#333378",
          text_y = 1.55,
          breaks_x = seq(0, 10, 2),
          limits_x = c(0, 10)
        )
      
      ##### Plank
      p_pc_3_by_sex <-
        plot_score_distri(
          data1 = pc_scores_by_sex ,
          data2 = n_pc_scores_by_sex ,
          item = "Plank (/10)",
          by_sex = "yes",
          color = "#333378",
          text_y = 1.55,
          breaks_x = seq(0, 10, 2),
          limits_x = c(0, 10)
        )
      
      ##### PC score
      p_pc_4_by_sex  <-
        plot_score_distri(
          data1 = pc_scores_by_sex ,
          data2 = n_pc_scores_by_sex ,
          item = "Physical Competence (/30)",
          by_sex = "yes",
          color = "#333378",
          text_y = 1.55,
          breaks_x = seq(0, 30, 10),
          limits_x = c(0, 30)
        )
      
      ### Get figures for DB scores ----
      #### Select DB scores
      db_scores_by_sex <-
        capl_res |>
        select(
          id,
          gender,
          step_score,
          self_report_pa_score,
          db_score
          ) |>
        pivot_longer(
          cols = step_score:db_score,
          names_to = "Item",
          values_to = "Score"
          ) |>
        mutate(
          Item = fct_relevel(Item, "step_score", "self_report_pa_score", "db_score"),
          Item = fct_recode(
            Item,
            "Average Daily Sept Count (/25)" = "step_score",
            "Self-Rep. Num. of Days with MVPA (/5)" = "self_report_pa_score",
            "Daily Behaviour (/30)" = "db_score"
          )
        ) 
      
      #### Get the numbers of students with valid data for each item
      n_db_scores_by_sex <-
        db_scores_by_sex |>
        group_by(gender, Item) |>
        summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
      
      #### Get plots with score distributions
      ##### Steps
      p_db_1_by_sex <-
        plot_score_distri(
          data1 = db_scores_by_sex,
          data2 = n_db_scores_by_sex,
          item = "Average Daily Sept Count (/25)",
          by_sex = "yes",
          color = "#9C8E84",
          text_y = 1.55,
          breaks_x = seq(0, 25, 5),
          limits_x = c(0, 25)
        )
      
      ##### Self-reported PA
      p_db_2_by_sex <-
        plot_score_distri(
          data1 = db_scores_by_sex,
          data2 = n_db_scores_by_sex,
          item = "Self-Rep. Num. of Days with MVPA (/5)",
          by_sex = "yes",
          color = "#9C8E84",
          text_y = 1.55,
          breaks_x = seq(0, 5, 1),
          limits_x = c(0, 5)
        )
      
      ##### DB score
      p_db_3_by_sex <-
        plot_score_distri(
          data1 = db_scores_by_sex,
          data2 = n_db_scores_by_sex,
          item = "Daily Behaviour (/30)",
          by_sex = "yes",
          color = "#9C8E84",
          text_y = 1.55,
          breaks_x = seq(0, 30, 10),
          limits_x = c(0, 30)
        )
      
      ### Get figures for MC scores ----
      #### Select MC scores
      mc_scores_by_sex <-
        capl_res |>
        select(
          id,
          gender,
          predilection_score,
          adequacy_score,
          intrinsic_motivation_score,
          pa_competence_score,
          mc_score
        ) |>
        pivot_longer(cols = predilection_score:mc_score,
                     names_to = "Item",
                     values_to = "Score") |>
        mutate(
          Item = fct_relevel(
            Item,
            "intrinsic_motivation_score",
            "pa_competence_score",
            "predilection_score",
            "adequacy_score",
            "mc_score"
          ),
          Item = fct_recode(
            Item,
            "Intrinsic Motivation (/7.5)" = "intrinsic_motivation_score",
            "Competence (/7.5)" = "pa_competence_score",
            "Predilection (/7.5)" = "predilection_score",
            "Adequacy (/7.5)" = "adequacy_score",
            "Motivation and Confidence (/30)" = "mc_score"
          )
        )
      
      #### Get the numbers of students with valid data for each item
      n_mc_scores_by_sex <-
        mc_scores_by_sex |>
        group_by(gender, Item) |>
        summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
      
      #### Get plots with score distributions
      ##### Intrinsic motivation
      p_mc_1_by_sex <-
        plot_score_distri(
          data1 = mc_scores_by_sex,
          data2 = n_mc_scores_by_sex,
          item = "Intrinsic Motivation (/7.5)",
          by_sex = "yes",
          color = "#EF6723",
          text_y = 1.55,
          breaks_x = seq(0, 7.5, 1.5),
          limits_x = c(0, 7.5)
        )
      
      ##### Competence
      p_mc_2_by_sex <-
        plot_score_distri(
          data1 = mc_scores_by_sex,
          data2 = n_mc_scores_by_sex,
          item = "Competence (/7.5)",
          by_sex = "yes", 
          color = "#EF6723",
          text_y = 1.55,
          breaks_x = seq(0, 7.5, 1.5),
          limits_x = c(0, 7.5)
        )
      
      ##### Predilection
      p_mc_3_by_sex <-
        plot_score_distri(
          data1 = mc_scores_by_sex,
          data2 = n_mc_scores_by_sex,
          item = "Predilection (/7.5)",
          by_sex = "yes", 
          color = "#EF6723",
          text_y = 1.55,
          breaks_x = seq(0, 7.5, 1.5),
          limits_x = c(0, 7.5)
        )
      
      ##### Adequacy
      p_mc_4_by_sex <-
        plot_score_distri(
          data1 = mc_scores_by_sex,
          data2 = n_mc_scores_by_sex,
          item = "Adequacy (/7.5)",
          by_sex = "yes", 
          color = "#EF6723",
          text_y = 1.55,
          breaks_x = seq(0, 7.5, 1.5),
          limits_x = c(0, 7.5)
        )
      
      ##### MC score
      p_mc_5_by_sex <-
        plot_score_distri(
          data1 = mc_scores_by_sex,
          data2 = n_mc_scores_by_sex,
          item = "Motivation and Confidence (/30)",
          by_sex = "yes", 
          color = "#EF6723",
          text_y = 1.55,
          breaks_x = seq(0, 30, 10),
          limits_x = c(0, 30)
        )
      
      ### Get figures for KU scores ----
      #### Select KU scores
      ku_scores_by_sex <-
        capl_res |>
        select(
          id,
          gender,
          fill_in_the_blanks_score,
          pa_guideline_score,
          crf_means_score,
          ms_means_score,
          sports_skill_score,
          ku_score
        ) |>
        pivot_longer(cols = fill_in_the_blanks_score:ku_score,
                     names_to = "Item",
                     values_to = "Score") |>
        mutate(
          Item = fct_relevel(
            Item,
            "fill_in_the_blanks_score",
            "pa_guideline_score",
            "crf_means_score",
            "ms_means_score",
            "sports_skill_score",
            "ku_score"
          ),
          Item = fct_recode(
            Item,
            "PA Comprehension and Understanding (/6)" = "fill_in_the_blanks_score",
            "Daily PA Guidelines (/1)" = "pa_guideline_score",
            "Cardiorespiratory Fitness Definition (/1)" = "crf_means_score",
            "Muscular Strength & Endurance Definition (/1)" = "ms_means_score",
            "Improve Sport Skill (/1)" = "sports_skill_score",
            "Knowledge and Understanding (/10)" = "ku_score"
          )
        )
      
      #### Get the numbers of students with valid data for each item
      n_ku_scores_by_sex <-
        ku_scores_by_sex |>
        group_by(gender, Item) |>
        summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
      
      #### Get plots with score distributions
      ##### Physical activity comprehension and understanding
      p_ku_1_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          item = "PA Comprehension and Understanding (/6)",
          by_sex = "yes", 
          color = "#00A79F",
          text_y = 1.55,
          breaks_x = seq(0, 6, 2),
          limits_x = c(0, 6)
        )
      
      ##### Daily PA guidelines
      p_ku_2_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          type = "disc",
          item = "Daily PA Guidelines (/1)",
          by_sex = "yes", 
          color = "#00A79F",
          breaks_x = seq(0, 1, 1),
          limits_x = c(0, 1)
        )
      
      ##### Cardiorespiratory fitness definition
      p_ku_3_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          type = "disc",
          item = "Cardiorespiratory Fitness Definition (/1)",
          by_sex = "yes", 
          color = "#00A79F",
          breaks_x = seq(0, 1, 1),
          limits_x = c(0, 1)
        )
      
      ##### Muscular strength & endurance definition
      p_ku_4_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          type = "disc",
          item = "Muscular Strength & Endurance Definition (/1)",
          by_sex = "yes", 
          color = "#00A79F",
          breaks_x = seq(0, 1, 1),
          limits_x = c(0, 1)
        )
      
      ##### Improve sport skill
      p_ku_5_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          type = "disc",
          item = "Improve Sport Skill (/1)",
          by_sex = "yes",
          color = "#00A79F",
          breaks_x = seq(0, 1, 1),
          limits_x = c(0, 1)
        )
      
      ##### KU score
      p_ku_6_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          item = "Knowledge and Understanding (/10)",
          by_sex = "yes", 
          color = "#00A79F",
          text_y = 1.55,
          breaks_x = seq(0, 10, 2),
          limits_x = c(0, 10)
        )
      
      ### Get figure for CAPL-2 ----
      #### Select data
      capl_score_by_sex <-
        capl_res |>
        select( id, gender, capl_score) |>
        pivot_longer(
          cols = capl_score,
          names_to = "Item",
          values_to = "Score"
          ) |>
        mutate(Item = fct_recode(Item, "Physical Literacy (/100)" = "capl_score"))
      
      #### Get the numbers of students with valid data
      n_capl_score_by_sex <-
        capl_score_by_sex |>
        group_by(gender, Item) |>
        summarise(n = sum(ifelse(!is.na(Score), 1, 0)))
      
      #### Get plots with score distributions
      p_capl_by_sex <-
        plot_score_distri(
          data1 = capl_score_by_sex,
          data2 = n_capl_score_by_sex,
          item = "Physical Literacy (/100)",
          by_sex = "yes", 
          color = "#D4D11B",
          text_y = 1.55,
          breaks_x = seq(0, 100, 20),
          limits_x = c(0, 100)
        )
      
      ### Get final figure ----
      ##### Build blank plot
      blank_plot_by_sex <-
        plot_score_distri(
          data1 = pc_scores_by_sex,
          data2 = n_pc_scores_by_sex,
          item = "PACER Shuttle Run (/10)",
          color = "white",
          text_y = 1.555,
          breaks_x = seq(0, 10, 2),
          limits_x = c(0, 10)
        ) +
        geom_rect(
          aes(
            xmin = -2,
            xmax = 20,
            ymin = -2,
            ymax = 100
          ),
          fill  = "white",
          color = "white"
        ) 
      
      ##### Gather domain plots
      p_pl_domains_by_sex <-
        p_pc_4_by_sex      + p_db_3_by_sex      + p_mc_5_by_sex     +  p_ku_6_by_sex +
        p_pc_1_by_sex      + p_db_1_by_sex      + p_mc_1_by_sex     +  p_ku_1_by_sex +
        p_pc_2_by_sex      + p_db_2_by_sex     + p_mc_2_by_sex     +  p_ku_2_by_sex +
        p_pc_3_by_sex      + blank_plot_by_sex  + p_mc_3_by_sex     +  p_ku_3_by_sex +
        blank_plot_by_sex  + blank_plot_by_sex  + p_mc_4_by_sex     +  p_ku_4_by_sex +
        blank_plot_by_sex  + blank_plot_by_sex  + blank_plot_by_sex +  p_ku_5_by_sex +
        plot_layout(
          nrow = 6,
          byrow = TRUE,
          axis_titles  = "collect"
        )
      
      ##### Get final figure
      p_pl_by_sex <-
        (plot_spacer() + (p_capl_by_sex + labs(y = "")) + plot_spacer()) / p_pl_domains_by_sex + plot_layout(heights = c(1, 6), guides = 'collect')
    }
  ),
  
# BETWEEN-SEX COMPARISON OF CAPL-2 SCORES ----
  ## Get CAPL-2 descriptive statistics by sex ----
  tar_target(
    name = n_girls,
    command = nrow(filter(capl_res, gender == "girl"))
  ),
  tar_target(
    name = n_boys,
    command = nrow(filter(capl_res, gender == "boy"))
  ),
  tar_target(
    name = desc_stats_capl_by_sex,
    command = capl_res |> 
      tbl_summary(
        by = gender,
        include = c(pc_score, 
                    pacer_score,
                    camsa_score,
                    plank_score,
                    db_score, 
                    step_score,
                    self_report_pa_score,
                    mc_score, 
                    intrinsic_motivation_score,
                    pa_competence_score,
                    predilection_score,
                    adequacy_score,
                    ku_score, 
                    fill_in_the_blanks_score,
                    pa_guideline_score,
                    crf_means_score,
                    ms_means_score,
                    sports_skill_score,
                    capl_score),
        
        label = list(
          pc_score = "Physical competence (/30)",
          pacer_score = "PACER shuttle run (/10)",
          camsa_score = "CAMSA (/10)",
          plank_score = "Plank (/10)",
          db_score = "Daily behaviour (/30)",
          step_score = "Average daily step count (/25)",
          self_report_pa_score = "Self-reported number of days with MVPA (/5)",
          mc_score = "Motivation and confidence (/30)",
          intrinsic_motivation_score = "Intrinsic motivation (/7.5)",
          pa_competence_score = "Competence (/7.5)",
          predilection_score = "Predilection (/7.5)",
          adequacy_score = "Adequacy (/7.5)",
          ku_score = "Knowledge and understanding (/10)",
          fill_in_the_blanks_score = "PA comprehension and understanding (/6)",
          pa_guideline_score = "Daily PA guidelines (/1)",
          crf_means_score = "Cardiorespiratory fitness definition (/1)",
          ms_means_score = "Muscular strength and endurance definition (/1)",
          sports_skill_score = "Improve sport skill (/1)",
          capl_score = "Physical literacy (/100)"
        ),
        missing = "no",
        statistic = list(
          all_continuous() ~ "{median} ({p25} - {p75})  \n{mean} ± {sd}"
        ),
        type = list(
          pacer_score = "continuous", 
          self_report_pa_score = "continuous",
          intrinsic_motivation_score = "continuous", 
          predilection_score = "continuous", ku_score = "continuous",
          fill_in_the_blanks_score = "continuous"
        ),
        digits = list(all_continuous() ~ 1)
      ) |> 
      add_overall()  |>
      modify_footnote(all_stat_cols() ~ "Median (Q1 - Q3), mean ± SD, or n (%) of participants who obtained a score of 1/1.") |> 
      gtsummary::modify_header(list(
        label = c("**Score**"), 
        stat_0 = "**All participants**  \nN = {N}",
        stat_1 = "**Girls**  \nN = {n}",
        stat_2 = "**Boys**  \nN = {n}"
      )
      ) 
  ), 
  
  ## Get interpretation statistics by sex ----
  tar_target(
    name = p_interpretation_by_sex, 
    command = capl_res |>
      select(c(id, gender, ends_with("interpretation"))) |>
      pivot_longer(
        cols = c(pacer_interpretation:capl_interpretation),
        names_to = "score",
        values_to = "interpretation"
      ) |>
      mutate(
        score = as.factor(score),
        score = fct_relevel(
          score,
          "pacer_interpretation",
          "camsa_interpretation",
          "plank_interpretation",
          "pc_interpretation",
          "step_interpretation",
          "db_interpretation",
          "mc_interpretation",
          "ku_interpretation",
          "capl_interpretation"
        ),
        score = fct_recode(
          score,
          "PACER Shuttle Run" = "pacer_interpretation",
          "CAMSA Score" = "camsa_interpretation",
          "Plank" = "plank_interpretation",
          "Physical Competence" = "pc_interpretation",
          "Average Daily Step Count" = "step_interpretation",
          "Daily Behaviour" = "db_interpretation",
          "Motivation and Confidence" = "mc_interpretation",
          "Knowledge and Understanding" = "ku_interpretation",
          "Physical Literacy" = "capl_interpretation"
        )
      ) |>
      count(gender, score, interpretation, .drop = FALSE) |>
      group_by(gender, score) |>
      mutate(prop = round(n / sum(n) * 100, 0)) |>
      ggplot(aes(x = interpretation, y = prop)) +
      geom_bar(stat = "identity",
               aes(fill = gender),
               position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = paste0(prop, "%"), group = gender),
        position = position_dodge(width = 0.9),
        size = 2,
        vjust = -0.3
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = c("hotpink", "royalblue2"),
                        labels = c("Girls", "Boys")) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "", y = "%", fill = "Sex") +
      facet_wrap(~ score) +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold")
      )
  ), 


  ## Comparison of CAPL-2 scores between girls and boys

  ### Test
  tar_target(
      name = capl_comp_sex,
      command = rank.two.samples(
        capl_score ~ gender,
        data = capl_res,
        conf.level = 0.95,
        alternative = "two.sided",
        rounds = 4,
        permu = TRUE,
        nperm = 10000
      )
    ),
  
  ## Multivariate comparisons of CAPL-2 domain scores between girls and boys ----
  
    ### Build a plot to visualize multivariate distributions ----
    tar_target(
      name = p_multiv_comp_sex_dom,
      command = capl_res |>
        select(id, gender, pc_score, db_score, mc_score, ku_score) |>
        pivot_longer(
          cols = c(pc_score, db_score, mc_score, ku_score),
          names_to = "score",
          values_to = "val"
        ) |>
        mutate(
          gender = factor(gender, labels = c("Girls", "Boys")),
          score = factor(
            score,
            levels = c("pc_score", "db_score", "mc_score", "ku_score"),
            labels = c(
              "Physical \n competence (/30)",
              "Daily \n behaviour (/30)",
              "Motivation \nand confidence (/30)",
              "Knowledge \nand understanding (/10)"
            )
          )
        ) |>
        ggplot(aes(x = "", y = val, fill = gender, color = gender)) +
        geom_rain(
          rain.side = "l",
          boxplot.args = list(color = "black"),
          boxplot.args.pos = list(
            position = ggpp::position_dodgenudge(x = -0.05, width = 0.3), width = 0.2
          ),
          point.args = list(alpha = 0.3),
          point.args.pos = list(
            position = ggpp::position_dodgenudge(x = 0.3, width = 0.2)),
          violin.args = list(alpha = 0.3),
        ) +
        scale_color_manual(values = c("hotpink", "royalblue")) +
        scale_fill_manual(values = c("hotpink", "royalblue")) +
        labs(x = NULL, y = "Score", color = "Sex", fill = "Sex") +
        facet_wrap(~ score, scales = "free", nrow = 1) +
        theme_bw() +
        theme(
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = "grey40"),
          strip.background = element_rect(fill = "grey40", color = "grey40"),
          strip.text = element_text(color = "white", face = "bold", size = 8),
          panel.border = element_rect(color = "grey40")
        )
    ),

    ### Test global between-sex differences for physical literacy domain scores ----
    #### Raw output
    tar_target(
      name = domain_global_multicomp_sex,
      command = nonpartest(
        pc_score | db_score | mc_score | ku_score ~ gender,
        data = capl_res,
        permreps = 1000,
        plots = FALSE
      )
    ), 

    #### Formated output
    tar_target(
      name = domain_global_multicomp_sex_rel_eff,
      command = capl_res |> 
        dplyr::select(gender, pc_score, db_score, mc_score, ku_score) |> 
        tidyr::pivot_longer(
          cols = c(pc_score, db_score, mc_score, ku_score), 
          names_to = "Score",
          values_to = "val"
        ) |> 
        dplyr::mutate(Score = factor(
          Score,
          levels = c("pc_score", "db_score", "mc_score", "ku_score"),
          labels = c(
            "Physical competence (/30)",
            "Daily behaviour (/30)",
            "Motivation and confidence (/30)",
            "Knowledge and understanding (/10)"
          )
        )) |> 
        tidyr::drop_na() |> 
        dplyr::group_by(gender, Score) |>
        dplyr::summarise(n = dplyr::n()) |> 
        tidyr::pivot_wider(id_cols = "Score", names_from = gender, values_from = n) |> 
        dplyr::mutate(`N Girls   \n(Min. / Max. Theo. Rel. Eff.)` = paste0(girl, "  \n(", janitor::round_half_up(girl/(2*(girl + boy)), 2), "/", janitor::round_half_up(1 - girl/(2*(girl + boy)), 2), ")")) |> 
        dplyr::mutate(`N Boys   \n(Min. / Max. Theo. Rel. Eff.)` = paste0(boy, "  \n(", janitor::round_half_up(boy/(2*(girl + boy)), 2), "/", janitor::round_half_up(1 - boy/(2*(girl + boy)), 2), ")")) |> 
        dplyr::left_join(
          domain_global_multicomp_sex$twogroupreleffects |> 
            t() |> 
            as.data.frame() |> 
            tibble::rownames_to_column(var = "Score") |> 
            dplyr::mutate(
              Score = as.factor(Score),
              Score = factor(
                Score,
                levels = c("pc_score", "db_score", "mc_score", "ku_score"),
                labels = c(
                  "Physical competence (/30)",
                  "Daily behaviour (/30)",
                  "Motivation and confidence (/30)",
                  "Knowledge and understanding (/10)"
                )
              ), 
              dplyr::across(c(girl, boy), ~janitor::round_half_up(.x, digits = 2))
            ) |>  
            dplyr::rename("Rel. Eff. Girls" = girl, "Rel. Eff. Boys" = boy)
        ) |> 
        dplyr::select(-c(girl, boy)) 
    ),
  
    ### Test local between-sex differences for physical literacy domain scores  ----
    tar_target(
      name = domain_local_multicomp_sex,
      command = capture.output(
        ssnonpartest(
        pc_score | db_score | mc_score | ku_score ~ gender,
        data = capl_res,
        test = c(1, 0, 0, 0),
        alpha = 0.05,
        factors.and.variables = TRUE
      )
      )
    ),

    ### Get a graphic with all significant sets of domain scores for 
    ### between-sex differences ----
    tar_target(
      name = domain_local_multicomp_sex_graph,
      command = get_multicomp_graph(
        scores = c("pc_score", "db_score", "mc_score", "ku_score"),
        ssnonpartest_out = domain_local_multicomp_sex,
        x_label = "CAPL-2 domain scores"
      ) +
        scale_x_discrete(
          labels = c(
            "Physical \ncompetence",
            "Daily \nbehaviour",
            "Motivation \nand confidence",
            "Knowledge \nand understanding"
          )
        ) +
        scale_fill_manual(values = c("grey95", "white", "grey95", "white")) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        coord_flip(expand = FALSE)
    ),
  
  ## Multivariate comparisons of CAPL-2 item scores between girls and boys ----

    ### Build a plot to visualize multivariate distributions ----
    tar_target(
      name = p_multiv_comp_sex_item,
      command = capl_res |>
        select(id, gender, pacer_score, plank_score, camsa_score, 
                 step_score, self_report_pa_score,
                 predilection_score, adequacy_score, intrinsic_motivation_score,
                 pa_competence_score, pa_guideline_score, crf_means_score,
                 ms_means_score, sports_skill_score, fill_in_the_blanks_score) |>
        pivot_longer(
          cols = c(pacer_score, plank_score, camsa_score, 
                   step_score, self_report_pa_score,
                   predilection_score, adequacy_score, intrinsic_motivation_score,
                   pa_competence_score, pa_guideline_score, crf_means_score,
                   ms_means_score, sports_skill_score, fill_in_the_blanks_score),
          names_to = "score",
          values_to = "val"
        ) |>
        mutate(
          gender = factor(gender, labels = c("Girls", "Boys")),
          score = factor(
            score,
            levels = c(
              "pacer_score", 
              "camsa_score", 
              "plank_score",
              "step_score", 
              "self_report_pa_score",
              "intrinsic_motivation_score",
              "pa_competence_score", 
              "predilection_score", 
              "adequacy_score", 
              "fill_in_the_blanks_score",
              "pa_guideline_score", 
              "crf_means_score",
              "ms_means_score", 
              "sports_skill_score"
              ),
            labels = c(
              "PACER shuttle \nrun (/10)",
              "CAMSA (/10)",
              "Plank (/10)",
              "Average daily \nstep count (/25)",
              "Self-reported number \nof days with MVPA (/5)",
              "Intrinsic \nmotivation (/7.5)",
              "Competence (/7.5)",
              "Predilection (/7.5)",
              "Adequacy (/7.5)",
              "PA comprehension \nand understanding (/6)",
              "Daily PA \nguidelines (/1)",
              "Cardiorespiratory fitness \ndefinition (/1)",
              "Muscular strength and \nendurance definition (/1)",
              "Improve sport skill (/1)"
            )
          )
        ) |>
        arrange(score) |> 
        ggplot(aes(x = "", y = val, fill = gender, color = gender)) +
        geom_rain(
          rain.side = "l",
          boxplot.args = list(color = "black"),
          boxplot.args.pos = list(
            position = ggpp::position_dodgenudge(x = -0.05, width = 0.3), width = 0.2
          ),
          point.args = list(alpha = 0.3),
          point.args.pos = list(
            position = ggpp::position_dodgenudge(x = 0.3, width = 0.2)),
          violin.args = list(alpha = 0.3),
        ) +
        scale_color_manual(values = c("hotpink", "royalblue")) +
        scale_fill_manual(values = c("hotpink", "royalblue")) +
        labs(x = NULL, y = "Score", color = "Sex", fill = "Sex") +
        facet_wrap(~ score, scales = "free") +
        theme_bw() +
        theme(
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = "grey40"),
          strip.background = element_rect(fill = "grey40", color = "grey40"),
          strip.text = element_text(color = "white", face = "bold", size = 8),
          panel.border = element_rect(color = "grey40")
        )
    ),

    ### Test global between-sex differences for physical literacy item scores ----
    #### Raw output
    tar_target(
      name = item_global_multicomp_sex,
      command = nonpartest(
          pacer_score | plank_score | camsa_score | 
          step_score	| self_report_pa_score |
          predilection_score | adequacy_score | intrinsic_motivation_score |
          pa_competence_score | pa_guideline_score | crf_means_score |
          ms_means_score | sports_skill_score | fill_in_the_blanks_score ~ gender,
        data = capl_res,
        permreps = 1000,
        plots = FALSE
      )
    ), 

    #### Formated output
    tar_target(
      name = item_global_multicomp_sex_rel_eff,
      command =  capl_res |> 
        dplyr::select(gender, 
                      pacer_score, 
                      camsa_score, 
                      plank_score,
                      step_score, 
                      self_report_pa_score,
                      intrinsic_motivation_score,
                      pa_competence_score, 
                      predilection_score, 
                      adequacy_score, 
                      fill_in_the_blanks_score,
                      pa_guideline_score, 
                      crf_means_score,
                      ms_means_score, 
                      sports_skill_score
        ) |> 
        tidyr::pivot_longer(
          cols = c(pacer_score, 
                   camsa_score, 
                   plank_score,
                   step_score, 
                   self_report_pa_score,
                   intrinsic_motivation_score,
                   pa_competence_score, 
                   predilection_score, 
                   adequacy_score, 
                   fill_in_the_blanks_score,
                   pa_guideline_score, 
                   crf_means_score,
                   ms_means_score, 
                   sports_skill_score), 
          names_to = "Score",
          values_to = "val"
        ) |> 
        dplyr::mutate(Score = factor(
          Score, 
          levels = c(
            "pacer_score", 
            "camsa_score", 
            "plank_score",
            "step_score", 
            "self_report_pa_score",
            "intrinsic_motivation_score",
            "pa_competence_score", 
            "predilection_score", 
            "adequacy_score", 
            "fill_in_the_blanks_score",
            "pa_guideline_score", 
            "crf_means_score",
            "ms_means_score", 
            "sports_skill_score"
          ),
          labels = c(
            "PACER shuttle run (/10)",
            "CAMSA (/10)",
            "Plank (/10)",
            "Average daily step count (/25)",
            "Self-reported number of days with MVPA (/5)",
            "Intrinsic motivation (/7.5)",
            "Competence (/7.5)",
            "Predilection (/7.5)",
            "Adequacy (/7.5)",
            "PA comprehension and understanding (/6)",
            "Daily PA guidelines (/1)",
            "Cardiorespiratory fitness definition (/1)",
            "Muscular strength and endurance definition (/1)",
            "Improve sport skill (/1)"
          )
        )) |> 
        tidyr::drop_na() |> 
        dplyr::group_by(gender, Score) |>
        dplyr::summarise(n = dplyr::n()) |> 
        tidyr::pivot_wider(id_cols = "Score", names_from = gender, values_from = n) |> 
        dplyr::mutate(`N Girls   \n(Min. / Max. Theo. Rel. Eff.)` = paste0(girl, "  \n(", janitor::round_half_up(girl/(2*(girl + boy)), 2), "/", janitor::round_half_up(1 - girl/(2*(girl + boy)), 2), ")")) |> 
        dplyr::mutate(`N Boys   \n(Min. / Max. Theo. Rel. Eff.)` = paste0(boy, "  \n(", janitor::round_half_up(boy/(2*(girl + boy)), 2), "/", janitor::round_half_up(1 - boy/(2*(girl + boy)), 2), ")")) |> 
        dplyr::left_join(
          item_global_multicomp_sex$twogroupreleffects |> 
            t() |> 
            as.data.frame() |> 
            tibble::rownames_to_column(var = "Score") |> 
            dplyr::mutate(
              Score = factor(
                Score,
                levels = c(
                  "pacer_score", 
                  "camsa_score", 
                  "plank_score",
                  "step_score", 
                  "self_report_pa_score",
                  "intrinsic_motivation_score",
                  "pa_competence_score", 
                  "predilection_score", 
                  "adequacy_score", 
                  "fill_in_the_blanks_score",
                  "pa_guideline_score", 
                  "crf_means_score",
                  "ms_means_score", 
                  "sports_skill_score"
                ),
                labels = c(
                  "PACER shuttle run (/10)",
                  "CAMSA (/10)",
                  "Plank (/10)",
                  "Average daily step count (/25)",
                  "Self-reported number of days with MVPA (/5)",
                  "Intrinsic motivation (/7.5)",
                  "Competence (/7.5)",
                  "Predilection (/7.5)",
                  "Adequacy (/7.5)",
                  "PA comprehension and understanding (/6)",
                  "Daily PA guidelines (/1)",
                  "Cardiorespiratory fitness definition (/1)",
                  "Muscular strength and endurance definition (/1)",
                  "Improve sport skill (/1)"
                )
              ),
              dplyr::across(c(girl, boy), ~janitor::round_half_up(.x, digits = 2))
            ) |> 
            dplyr::arrange(Score) |>  
            dplyr::rename("Rel. Eff. Girls" = girl, "Rel. Eff. Boys" = boy)
        ) |> 
        dplyr::select(-c(girl, boy))
    ),
    
    ### Test local between-sex differences for physical literacy item scores ----
    tar_target(
      name = item_local_multicomp_sex,
      command = capture.output(
        ssnonpartest(
            pacer_score | plank_score | camsa_score | 
            step_score	| self_report_pa_score |
            predilection_score | adequacy_score | intrinsic_motivation_score |
            pa_competence_score | pa_guideline_score | crf_means_score |
            ms_means_score | sports_skill_score | fill_in_the_blanks_score ~ gender,
        data = capl_res,
        test = c(1, 0, 0, 0),
        alpha = 0.05,
        factors.and.variables = TRUE
      )
      )
    ), 
  
# BETWEEN-PHYSICAL LITERACY PROFILE COMPARISONS OF MOVEMENT BEHAVIOURS ----
  
  ## Get IDs from participants with >= 4 valid days ----
  tar_target(
    name = ids_with_4_valid_days,
    command = pa_data$all_metrics |> 
      filter(valid_days >= 4) |> 
      pull(id)
  ),

  ## Comparisons of the movement behaviour metrics ----

  ### Get a data frame with both CAPL-2 data and movement behaviour metrics ----
  tar_target(
    name = capl_res_4_valid_days,
    command = pa_data$all_metrics |>
      filter(id %in% ids_with_4_valid_days) |> 
      select(-minutes_MPA, -minutes_VPA, -percent_MPA, -percent_VPA, -total_kcal, -pal, -mets_hours_mvpa) |>  # remove irrelevant variables
      left_join(
        capl_res |>
          mutate(id = as.numeric(as.character(id)))
      ) |> 
      # Remove participants with no CAPL-2 global interpretation
      filter(capl_interpretation != "Non available") |> 
      mutate(capl_interpretation = factor(capl_interpretation, levels = 
                                            c("Beginning", "Progressing", "Achieving", "Excelling")))
  ),

  ### Pivot data
  tar_target(
    name = capl_res_4_valid_days_piv,
    command =  capl_res_4_valid_days |> 
      select(-c(school, gender, age:capl_score, capl_status)) |>
      rename(
        "Valid days" = "valid_days",
        "Wear time (min)" = "wear_time",
        "VA total counts" = "total_counts_axis1",
        "VM total counts" = "total_counts_vm",
        "VA counts/min" = "axis1_per_min",
        "VM counts/min" = "vm_per_min",
        "SED time (min)" = "minutes_SED",
        "LPA time (min)" = "minutes_LPA",
        "MVPA time (min)" = "minutes_MVPA",
        "% Wear time SED" = "percent_SED",
        "% Wear time LPA" = "percent_LPA",
        "% Wear time MVPA" = "percent_MVPA",
        "Ratio MVPA / SED" = "ratio_mvpa_sed",
        "Step count" = "total_steps",
        "60-min max step accum." = "max_steps_60min",
        "30-min max step accum." = "max_steps_30min",
        "20-min max step accum." = "max_steps_20min",
        "5-min max step accum." = "max_steps_5min",
        "1-min max step accum." = "max_steps_1min",
        "60-min peak step accum." = "peak_steps_60min",
        "30-min peak step accum." = "peak_steps_30min",
        "20-min peak step accum." = "peak_steps_20min",
        "5-min peak step accum." = "peak_steps_5min",
        "1-min peak step accum." = "peak_steps_1min",
        "Intensity gradient" = "ig",
        "MX 8 hrs" = "M1/3",
        "MX 120 min" = "M120",
        "MX 60 min" = "M60",
        "MX 30 min" = "M30",
        "MX 15 min" = "M15",
        "MX 5 min" = "M5",
        "Number of SED breaks" = "mean_breaks",
        "Power low exponent alpha" = "alpha",
        "Median bout duration (min)" = "MBD",
        "Usual bout duration (min)" = "UBD",
        "Gini index" = "gini"
      ) |>
      pivot_longer(cols = c(everything(), -id, -capl_interpretation),
                   names_to = "Metric",
                   values_to = "Value") |>
      mutate(
        Metric = as.factor(Metric),
        Metric = fct_relevel(
          Metric,
          "Valid days",
          "Wear time (min)",
          "VA total counts",
          "VM total counts",
          "VA counts/min",
          "VM counts/min",
          "SED time (min)",
          "LPA time (min)",
          "MVPA time (min)",
          "% Wear time SED",
          "% Wear time LPA",
          "% Wear time MVPA",
          "Ratio MVPA / SED",
          "Step count",
          "60-min max step accum.",
          "30-min max step accum.",
          "20-min max step accum.",
          "5-min max step accum.",
          "1-min max step accum.",
          "60-min peak step accum.",
          "30-min peak step accum.",
          "20-min peak step accum.",
          "5-min peak step accum.",
          "1-min peak step accum.",
          "Intensity gradient",
          "MX 8 hrs",
          "MX 120 min",
          "MX 60 min",
          "MX 30 min",
          "MX 15 min",
          "MX 5 min",
          "Number of SED breaks",
          "Power low exponent alpha",
          "Median bout duration (min)",
          "Usual bout duration (min)",
          "Gini index"
        )
      ) 
  ),

  ### Get a plot with the distributions of all the movement behaviour metrics ----
  tar_target(
    name = p_distri_all_metrics,
    command = capl_res_4_valid_days_piv |>  
      ggplot(aes(x = 0, y = Value)) +
      geom_rain(
        fill = "grey90",
        point.args = rlang::list2(
          alpha = 0.3,
          size = 2
        )) +
      facet_wrap( ~ Metric, scales = "free")  +
      coord_flip(xlim = c(-0.1, 0.55)) +
      labs(x = NULL) +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(color = "grey40", angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_line(color = "grey40"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "grey40", color = "grey40"),
        strip.text = element_text(color = "white", face = "bold", size = 10),
        panel.border = element_rect(color = "grey40")
      )
  ),

  ### Make a PCA biplot to identify (un)correlated variables ----
  tar_target(
    name = p_biplot_metrics,
    command = {
      
      #### Select relevant variables and get PCA results
      res.pca <-
        prcomp(
          capl_res_4_valid_days |> 
        select(
          vm_per_min,
          percent_SED,
          percent_LPA,
          percent_MVPA,
          ratio_mvpa_sed,
          total_steps:gini
        ),
        center = TRUE,
        scale = TRUE
        )
      
      #### Get PCA biplot
      fviz_pca_var(
        res.pca,
        title = "PCA",
        ggtheme = theme_classic(),
        legend = "bottom", 
        repel = TRUE
      )
    }
  ),

  ### Set the metrics retained from PCA for further analysis ----
  tar_target(
    name = pca_selection,
    command = c(
        "vm_per_min",
        "percent_SED",
        "percent_LPA",
        "percent_MVPA",
        "total_steps",
        "max_steps_60min",
        "peak_steps_60min",
        "ig",
        "mean_breaks",
        "UBD",
        "gini"
        )
  ),

  ### Check correlations (Spearman) between the retained variables
  tar_target(
    name = check_cor_metrics,
    command = capl_res_4_valid_days |> 
      select(any_of(pca_selection)) |> 
      correlation(method = "spearman") |> 
      mutate(abs_rho = abs(rho)) |> 
      arrange(-abs_rho)
  ),

  ### Set the metrics retained from both PCA and correlation results for ----
  ### further analysis ----
  tar_target(
    name = selected_metrics,
    command = list(
      raw_names = c(
        "percent_SED",
        "percent_MVPA",
        "total_steps",
        "max_steps_60min",
        "ig",
        "mean_breaks",
        "UBD"
      ),
      new_names = c(
        "% Wear time SED",
        "% Wear time MVPA",
        "Step count",
        "60-min max step accum.",
        "Intensity gradient",
        "Number of SED breaks",
        "Usual bout duration (min)"
      )
    )
  ),

  ### Get a plot showing the distributions of the metrics selected ----
  tar_target(
    name = p_distri_all_metrics_by_profile,
    command = capl_res_4_valid_days_piv |>  
      filter(Metric %in% selected_metrics$new_names) |> 
      ggplot(aes(x = "", y = Value, fill = capl_interpretation, color = capl_interpretation)) +
      geom_rain(
        rain.side = "l",
        boxplot.args = list(color = "black"),
        boxplot.args.pos = list(
          position = ggpp::position_dodgenudge(x = -0.05, width = 0.3), width = 0.2
        ),
        point.args = list(alpha = 0.3),
        point.args.pos = list(
          position = ggpp::position_dodgenudge(x = 0.3, width = 0.2)),
        violin.args = list(alpha = 0.3),
      ) +
      scale_color_manual(values = scales::hue_pal()(5)[2:5]) +
      scale_fill_manual(values = scales::hue_pal()(5)[2:5]) +
      labs(x = NULL, y = "Value", color = "CAPL-2 profile", fill = "CAPL-2 profile") +
      facet_wrap(~ Metric, scales = "free", ncol = 3) +
      theme_bw() +
      theme(
        legend.title = element_text(face = "bold"),
        legend.position = "right",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "grey40"),
        strip.background = element_rect(fill = "grey40", color = "grey40"),
        strip.text = element_text(color = "white", face = "bold", size = 10),
        panel.border = element_rect(color = "grey40")
      )
  ),

  ### Build a table with summary statistics for the retained movement behaviour ----
  ### metrics across the physical literacy profiles ----
  tar_target(
    name = tbl_retained_metrics,
    command = capl_res_4_valid_days |> 
      tbl_summary(
        include = selected_metrics$raw_names,
        by = capl_interpretation,
        label = list(
          vm_per_min = "VM counts/min",
          percent_SED = "% Wear time SED",
          percent_LPA = "% Wear time LPA",
          percent_MVPA = "% Wear time MVPA",
          total_steps = "Step count",
          max_steps_60min = "60-min max step accum.",
          peak_steps_60min = "60-min peak step accum.",
          ig = "Intensity gradient",
          mean_breaks = "Number of SED breaks",
          UBD = "Usual bout duration (min)",
          gini = "Gini index"
        ),
        missing = "no",
        statistic = list(
          all_continuous() ~ "{median} \n({p25} - {p75})  \n{mean} ± {sd}"
        ),
        digits = list(all_continuous() ~ 1)
      )  |> 
      add_overall() |> 
      modify_header(label ~ "**Metric**") |> 
      modify_footnote(c(stat_0, stat_1, stat_2, stat_3, stat_4) ~ "Numbers are medians (Q1 - Q3) and means ± SD.
      SED = sedentary, MVPA = moderate-to-vigorous physical activity. All metrics are daily averages except usual bout duration that was based on the entire week of measurement."
                      ) |> 
      gtsummary::modify_header(list(
        stat_0 = "**All participants**  \nN = {N}"
      )
      )
  ),

  ### Multivariate comparisons for movement behaviour metrics ----
  #### Set formula
  tar_target(
    name = testing_formula_metrics, 
    command = {
      vars_formula <- paste(selected_metrics$raw_names, collapse = " | ")
      formula <- as.formula(paste0(vars_formula, "~ capl_interpretation"))
      return(formula)
    }
  ),

  #### Test for a global difference of movement behaviours between PL profiles ----
  ##### Raw output 
 tar_target(
    name = metrics_global_multicomp_profile,
    command = nonpartest(
      testing_formula_metrics,
      data = capl_res_4_valid_days,
      permreps = 1000,
      plots = FALSE
    )
  ),

  ##### Formated output
  tar_target(
    name = metrics_global_multicomp_profile_rel_eff,
    command = capl_res_4_valid_days |>
      dplyr::select(capl_interpretation,
                    percent_SED,
                    percent_MVPA,
                    total_steps,
                    max_steps_60min,
                    ig,
                    mean_breaks,
                    UBD) |> 
      tidyr::pivot_longer(
        cols = c(percent_SED,
                 percent_MVPA,
                 total_steps,
                 max_steps_60min,
                 ig,
                 mean_breaks,
                 UBD), 
        names_to = "Metric",
        values_to = "val"
      ) |> 
      dplyr::mutate(Metric = factor(
        Metric, 
        levels = c(
          "percent_SED",
          "percent_MVPA",
          "total_steps",
          "max_steps_60min",
          "ig",
          "mean_breaks",
          "UBD"
        ),
        labels = c(
          "% Wear time SED",
          "% Wear time MVPA",
          "Step count",
          "60-min max step accum.",
          "Intensity gradient",
          "Number of SED breaks",
          "Usual bout duration (min)"
        )
      )) |> 
      tidyr::drop_na() |> 
      dplyr::group_by(capl_interpretation, Metric) |>
      dplyr::summarise(n = dplyr::n()) |> 
      tidyr::pivot_wider(id_cols = "Metric", names_from = capl_interpretation, values_from = n) |> 
      dplyr::mutate("N Beginning \n(Min. / Max. Theo. Rel. Eff.)" = paste0(Beginning , "  \n(", janitor::round_half_up(Beginning /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), "/", janitor::round_half_up(1 - Beginning/(2*(Beginning  + Progressing + Achieving + Excelling)), 2), ")")) |> 
      dplyr::mutate("N Progressing  \n(Min. / Max. Theo. Rel. Eff.)" = paste0(Progressing  , "  \n(", janitor::round_half_up(Progressing  /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), "/", janitor::round_half_up(1 - Progressing /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), ")")) |> 
      dplyr::mutate("N Achieving    \n(Min. / Max. Theo. Rel. Eff.)" = paste0(Achieving   , "  \n(", janitor::round_half_up(Achieving   /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), "/", janitor::round_half_up(1 - Achieving  /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), ")")) |> 
      dplyr::mutate("N Excelling  \n(Min. / Max. Theo. Rel. Eff.)" = paste0(Excelling  , "  \n(", janitor::round_half_up(Excelling  /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), "/", janitor::round_half_up(1 - Excelling /(2*(Beginning  + Progressing + Achieving + Excelling)), 2), ")")) |> 
      dplyr::left_join(
        metrics_global_multicomp_profile$releffects |> 
          t() |> 
          as.data.frame() |> 
          tibble::rownames_to_column(var = "Metric") |> 
          dplyr::mutate(
            Metric = as.factor(Metric),
            Metric = forcats::fct_recode(Metric,
                                         "% Wear time SED" = "percent_SED",
                                         "% Wear time MVPA" = "percent_MVPA",
                                         "Step count" = "total_steps",
                                         "60-min max step accum." = "max_steps_60min",
                                         "Intensity gradient" = "ig",
                                         "Number of SED breaks" = "mean_breaks",
                                         "Usual bout duration (min)" = "UBD"
            ),
            dplyr::across(c(Beginning:Excelling), ~janitor::round_half_up(.x, digits = 2))
          ) |> 
          dplyr::arrange(Metric) |>  
          dplyr::rename(
            "Rel. Eff. Beginning" = Beginning, 
            "Rel. Eff. Progressing" = Progressing,
            "Rel. Eff. Achieving" = Achieving,
            "Rel. Eff. Excelling" = Excelling
          ) 
      ) |> 
      dplyr::select(-c(Beginning, Progressing, Achieving, Excelling))
  ),

  #### Test for local differences of movement behaviours between PL profiles ----
  tar_target(
    name = metrics_local_multicomp_profile,
    command = capture.output(ssnonpartest(
      testing_formula_metrics,
      data = capl_res_4_valid_days,
      test = c(1, 0, 0, 0),
      alpha = 0.05,
      factors.and.variables = TRUE
    )
    )
  ),

  ## Render report ----
  tar_quarto(report, "report.qmd")
  
  )
