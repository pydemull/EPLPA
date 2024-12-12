
# Load packages ----
library(targets)
library(tarchetypes)

# Set target options ----
tar_option_set(
  packages = c(
    "activAnalyzer.batch",
    "capl",
    "dplyr",
    "forcats",
    "ggplot2",
    "ggh4x",
    "ggrain",
    "gtsummary",
    "hms",
    "npmv",
    "patchwork",
    "purrr",
    "readr",
    "skimr",
    "tidyr"
    )
)

tar_source()

# Define pipeline ----
list(
  
  ## Set config file path ----
  tar_target(
    name = config_file,
    command = "config.csv",
    format = "file"
  ),
  
  ## Set BASE file path ----
  tar_target(
    name = base_file,
    command = "./data/BASE.xlsx",
    format = "file"
  ),
  
  ## Set DEMO file path ----
  tar_target(
    name = demo_file,
    command = "./data/DEMO.csv",
    format = "file"
  ),
  
  ## Set DATES file path ----
  tar_target(
    name = dates_file,
    command = "./data/DATES.csv",
    format = "file"
  ),
  
  ## Set AGD files directory path ----
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
  
  ## Make a table for accelerometer analysis settings ----
  tar_target(
    name = tab_pa_metrics_config,
    command = pa_metrics_config[grep("CHILD", pa_metrics_config$CODE_NAME), ] |>
      filter(CODE_NAME != "EQUATION_EE_CHILD") |>
      select(-COMMENTS) |>
      bind_rows(pa_metrics_config |> filter(
        CODE_NAME %in% c(
          "VALID_WEAR_TIME_START",
          "VALID_WEAR_TIME_END",
          "MINIMUM_WEAR_TIME"
        )
      ) |> select(-COMMENTS))
  ),
  
  ## Get physical activity metrics ----
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
  
  ## Select and transpose steps and time data required for getting CAPL results ----
  tar_target(
    name = PA_METRICS,
    command = pa_data$results_by_day |>
      group_by(id) |>
      mutate(
        num_day = seq_along(id),
        non_wear_time = 0
      ) |>
      ungroup() |>
      filter(num_day <= 7) |>
      select(id, num_day, non_wear_time, wear_time, total_steps) |>
      rename(steps = total_steps) |>
      pivot_wider(
        id_cols = id,
        names_from = num_day,
        values_from = c(non_wear_time:steps),
        names_sep = ""
      ) |>
      mutate(
        time_on1 = "06:00",
        time_on2 = "06:00",
        time_on3 = "06:00",
        time_on4 = "06:00",
        time_on5 = "06:00",
        time_on6 = "06:00",
        time_on7 = "06:00",
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
  
  ## Recode data, rename, and select data ----
  tar_target(
    name = df_cleaned,
    command = df_raw |>
      mutate(
        across(c(identifiant, ecole, genre), as.factor),
        genre = fct_recode(genre,
                           "girl" = "F",
                           "boy" = "H"),
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
      select(-poids) |> 
      arrange(id)
  ), 
  
  ## Get CAPL results ----
  tar_target(
    name = capl_res,
    command =
      { 
        
        # Get initial CAPL results
        capl_res <- get_capl(df_cleaned)
        
        # Adjust Predilection score for ID 55
        predilection_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "predilection_score"][[1]], digits = 3))
        
        # Adjust Adequacy score for ID 55
        adequacy_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "adequacy_score"][[1]]), digits = 3)
        
        # Adjust Intrinsic motivation score for ID 55
        intrinsic_motivation_score_55 <-
          as.numeric(format(capl_res[capl_res$id == "55", "intrinsic_motivation_score"][[1]]), digits = 3)
        
        # Adjust PA competence score for ID 55
        pa_competence_score_55 <-
          capl_res[capl_res$id == "55", "pa_competence_score"][[1]]
        
        # Adjust MC score for ID 55
        capl_res[capl_res$id == "55", "mc_score"] <-
          get_mc_score(
            predilection_score_55,
            adequacy_score_55,
            intrinsic_motivation_score_55,
            pa_competence_score_55
          )[[1]]
        
        # Adjust MC score interpretation for ID 55
        capl_res[capl_res$id == "55", "mc_interpretation"] <-
          get_capl_interpretation(capl_res[capl_res$id == "55", "age"][[1]],
                                  capl_res[capl_res$id == "55", "gender"][[1]],
                                  capl_res[capl_res$id == "55", "mc_score"][[1]],
                                  "mc")[[1]]
        
        # Adjust CAPL status for ID 55
        capl_res[capl_res$id == "55", "capl_status"] <- "complete"
       
        # Adjust all CAPL status
        capl_res <- 
          capl_res |> 
          mutate(
            capl_score = ifelse(capl_status != "complete", NA, capl_score),
            across(where(~is.character(.x)), as.factor)
            )
        }
  ), 
  
  ## Get the figure showing the CAPL-2 scores ----
  tar_target(
    name = p_capl_all_domains,
    command = 
      {
        
        ### Get figures for PC scores ----
        #### Select PC scores
        pc_scores <-
          capl_res |>
          select(id,
                 pacer_score,
                 camsa_score,
                 plank_score,
                 pc_score) |>
          pivot_longer(cols = pacer_score:pc_score,
                       names_to = "Item",
                       values_to = "Score") |>
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
          select(id,
                 step_score,
                 self_report_pa_score,
                 db_score) |>
          pivot_longer(cols = step_score:db_score,
                       names_to = "Item",
                       values_to = "Score") |>
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
              "Cardiorerspiratory Fitness Definition (/1)" = "crf_means_score",
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
        
        ##### Cardiorerspiratory fitness definition
        p_ku_3 <-
          plot_score_distri(
            data1 = ku_scores,
            data2 = n_ku_scores,
            type = "disc",
            item = "Cardiorerspiratory Fitness Definition (/1)",
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
          select(
            id,
            capl_score
            
          ) |>
          pivot_longer(cols = capl_score,
                       names_to = "Item",
                       values_to = "Score") |>
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
  
  ## Get descriptive statistics ----
  tar_target(
    name = desc_stats_capl_all,
    command = capl_res |> 
      tbl_summary(
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
          ku_score = "Knowledge and Understanding (/10)",
          fill_in_the_blanks_score = "PA comprehension and understanding (/6)",
          pa_guideline_score = "Daily PA guidelines (/1)",
          crf_means_score = "Cardiorespiratory fitness definition (/1)",
          ms_means_score = "Muscular strength and endurance definition (/1)",
          sports_skill_score = "Improve sport skill (/1)",
          capl_score = "Physical Literacy (/100)"
        ),
        missing = "no",
        statistic = list(
          all_continuous() ~ "{median} ({p25} - {p75})"
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
      modify_footnote(all_stat_cols() ~ "Median (Q1 - Q3); n (%) of participants who obtained a score of 1/1.") |> 
      add_n() |> 
      as_flex_table()
  ),
  
  ## Get interpretation stats ----
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
        interpretation = fct_na_value_to_level(interpretation, "Non available"),
        interpretation = fct_relevel(
          interpretation,
          "Non available",
          "beginning",
          "progressing",
          "achieving",
          "excelling"
        ),
        interpretation = fct_recode(
          interpretation,
          "Beginning" = "beginning",
          "Progressing" = "progressing",
          "Achieving" = "achieving",
          "Excelling" = "excelling"
        ),
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
      mutate(prop = janitor::round_half_up(n / sum(n) * 100, 1)) |>
      ggplot(aes(x = interpretation, y = prop)) +
      geom_bar(stat = "identity", aes(fill = interpretation)) +
      geom_text(aes(label = paste0(prop, "%")), size = 3, vjust = -0.3) +
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
      )),
  
  ## Get the figure showing the CAPL-2 scores by sex ----
  tar_target(
    name = p_capl_all_domains_by_sex,
    command = {
      
      ### Get figures for PC scores ----
      #### Select PC scores
      pc_scores_by_sex <-
        capl_res |>
        select(id,
               gender,
               pacer_score,
               camsa_score,
               plank_score,
               pc_score) |>
        pivot_longer(cols = pacer_score:pc_score,
                     names_to = "Item",
                     values_to = "Score") |>
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
        select(id,
               gender,
               step_score,
               self_report_pa_score,
               db_score) |>
        pivot_longer(cols = step_score:db_score,
                     names_to = "Item",
                     values_to = "Score") |>
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
            "Cardiorerspiratory Fitness Definition (/1)" = "crf_means_score",
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
      
      ##### Cardiorerspiratory fitness definition
      p_ku_3_by_sex <-
        plot_score_distri(
          data1 = ku_scores_by_sex,
          data2 = n_ku_scores_by_sex,
          type = "disc",
          item = "Cardiorerspiratory Fitness Definition (/1)",
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
        select(
          id,
          gender,
          capl_score
          
        ) |>
        pivot_longer(cols = capl_score,
                     names_to = "Item",
                     values_to = "Score") |>
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
  
  # Get descriptive statistics by sex ----
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
        by = gender,
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
          ku_score = "Knowledge and Understanding (/10)",
          fill_in_the_blanks_score = "PA comprehension and understanding (/6)",
          pa_guideline_score = "Daily PA guidelines (/1)",
          crf_means_score = "Cardiorespiratory fitness definition (/1)",
          ms_means_score = "Muscular strength and endurance definition (/1)",
          sports_skill_score = "Improve sport skill (/1)",
          capl_score = "Physical Literacy (/100)"
        ),
        missing = "no",
        statistic = list(
          all_continuous() ~ "{median} ({p25} - {p75})"
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
          stat_1 = "**Girls**",
          stat_2 = "**Boys**"
        )
      ) |> 
      modify_footnote(all_stat_cols() ~ "Median (Q1 - Q3); n (%) of participants who obtained a score of 1/1.") |> 
      add_n() |> 
      as_flex_table() 
  ), 
  
  ## Get interpretation stats by sex ----
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
        interpretation = fct_na_value_to_level(interpretation, "Non available"),
        interpretation = as.factor(interpretation),
        interpretation = fct_relevel(
          interpretation,
          "Non available",
          "beginning",
          "progressing",
          "achieving",
          "excelling"
        ),
        interpretation = fct_recode(
          interpretation,
          "Beginning" = "beginning",
          "Progressing" = "progressing",
          "Achieving" = "achieving",
          "Excelling" = "excelling"
        ),
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
                        labels = c("Girl", "Boy")) +
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
  
  ## Multivariate comparions of CAPL-2 score between girls and boys ----
  
    ### Build a spaghetti plot showing the profiles of girls and boys ----
    tar_target(
      name = p_multiv_comp_sex,
      command = {
      
      strip <- strip_themed(background_x = elem_list_rect(fill = c("hotpink", "royalblue")))
      
      capl_res |>
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
              "Physical \ncompetence",
              "Daily \nbehaviour",
              "Motivation & \ncompetence",
              "Knowledge & \nunderstanding"
            )
          )
        ) |>
        ggplot(aes(
          x = score,
          y = val,
          fill = gender,
          color = gender
        )) +
        geom_rain(
          id.long.var = "id",
          rain.side = "l",
          boxplot.args = list(
            color = "black",
            width = 0.07,
            outlier.shape = NA
          ),
          point.args = list(alpha = 0.5),
          violin.args = list(color = "black"),
          boxplot.args.pos = list(position = position_nudge(x = 0)),
          point.args.pos = list(position = position_nudge(x = 0.1)),
          violin.args.pos = list(position = position_nudge(x = 0.07)),
          line.args.pos = list(position = position_nudge(x = 0.1))
        ) +
        stat_summary(aes(group = gender), fun = "median", geom = "line", color = "black") +
        facet_wrap2(~ gender, nrow = 2, strip = strip) +
        scale_fill_manual(values = c("hotpink", "royalblue")) +
        scale_color_manual(values = c("hotpink", "royalblue")) +
        labs(x = "Physical literacy domain", y = "Score") +
        guides(color = "none", fill = "none") +
        theme_bw()
      }
    ),
  
    ### Test between-sex difference for CAPL-2 scores
    tar_target(
      name = capl_comp_sex,
      command = wilcox.test(
        capl_res[capl_res$gender == "girl", ]$capl_score,
        capl_res[capl_res$gender == "boy", ]$capl_score,
        alternative = "two.sided", 
        paired = FALSE
        )
    ),
  
    ### Test global between-sex difference for physical literacy domain scores ----
    tar_target(
      name = domain_global_multicomp_sex,
      command = nonpartest(
        pc_score | db_score | mc_score | ku_score ~ gender,
        data = capl_res,
          permreps = 1000,
        plots = FALSE
      )
    ), 
  
    ### Test local between-sex differences for physical literacy domain scores  ----
    tar_target(
      name = domain_local_multicomp_sex,
      command = ssnonpartest(
        pc_score | db_score | mc_score | ku_score ~ gender,
        data = capl_res,
        test = c(1, 0, 0, 0),
        alpha = 0.05,
        factors.and.variables = TRUE
      )
    ),
  
  ### Test global between-sex difference for physical literacy item scores ----
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
  
  ### Test local between-sex difference for physical literacy item scores ----
  tar_target(
    name = item_local_multicomp_sex,
    command = ssnonpartest(
      step_score	| self_report_pa_score |
        predilection_score | adequacy_score | intrinsic_motivation_score |
        pa_competence_score | pa_guideline_score | crf_means_score |
        ms_means_score | sports_skill_score | fill_in_the_blanks_score ~ gender,
      data = capl_res,
      test = c(1, 0, 0, 0),
      alpha = 0.05,
      factors.and.variables = TRUE
    )
  ), 
  
  ## Render report ----
  tar_quarto(report, "report.qmd")
  
  )
