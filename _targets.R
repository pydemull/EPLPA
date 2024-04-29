
# Load packages
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "activAnalyzer.batch",
    "capl",
    "dplyr",
    "forcats",
    "purrr",
    "readr",
    "tidyr"
    )
)

# Define pipeline
list(
  
  # Get physical activity metrics config
  tar_target(
    name = pa_metrics_config,
    command = read_csv2("config.csv")
  ),
  
  # Get physical activity metrics
  tar_target(
    name = pa_data,
    command = process_all_agd(
      agd_dir = "./data/agd/",
      config_path = "./config.csv",
      demo_path = "./data/DEMO.csv",
      dates_path = "./data/DATES.csv",
      id_config = 3,
      content = "option_3"
    )
    ),
  
  # Import data
  tar_target(
    name = DEMO, 
    command = read_csv2("./data/DEMO.csv") # Demographic data
  ),
  tar_target(
    name = COMPT_PHY, 
    command = import_capl_data("./data/BASE.xlsx", sheet_name = "COMPT_PHY") # Physical skills
  ),
  tar_target(
    name = CONN_COMPR, 
    command = import_capl_data("./data/BASE.xlsx", sheet_name = "CONN_COMPR") # Knowledge
  ),
  tar_target(
    name = MOTIV_CONF, 
    command = import_capl_data("./data/BASE.xlsx", sheet_name = "MOTIV_CONF") # Motivation & confidence
  ),
  tar_target(
    name = COMP_PHY, 
    command = import_capl_data("./data/BASE.xlsx", sheet_name = "COMP_PHY") # Self-reported PA
  ),
  
  # Select and transpose the steps data required for getting CAPL results
  tar_target(
    name = PA_METRICS,
    command = pa_data$results_by_day |>
      group_by(id) |>
      mutate(
        num_day = seq_along(id),
        non_wear_time = 24 - wear_time / 60
      ) |>
      ungroup() |>
      filter(num_day <= 7) |>
      select(id, num_day, non_wear_time, total_steps) |>
      rename(steps = total_steps) |>
      pivot_wider(
        id_cols = id,
        names_from = num_day,
        values_from = c(non_wear_time, steps),
        names_sep = ""
      ) |>
      rename(identifiant = id)
  ), 
  
  # Combine all datasets
  tar_target(
    name = df_raw,
    command = list(DEMO, COMPT_PHY, CONN_COMPR, MOTIV_CONF, COMP_PHY, PA_METRICS) |>
      reduce(full_join, by = c("identifiant"))
  ),
  
  # Recode data
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
        across(c(camsa_skill_score1:self_report_pa), as.integer),
        # Add dummy time_on/time_off data so that steps score can be computed
        time_on1 = "06:00",
        time_on2 = "06:00",
        time_on3 = "06:00",
        time_on4 = "06:00",
        time_on5 = "06:00",
        time_on6 = "06:00",
        time_on7 = "06:00",
        time_off1 = "23:00",
        time_off2 = "23:00",
        time_off3 = "23:00",
        time_off4 = "23:00",
        time_off5 = "23:00",
        time_off6 = "23:00",
        time_off7 = "23:00"
      ) |> 
      arrange(identifiant)
  ), 
  
  # Get CAPL results
  tar_target(
    name = capl_res,
    command =
      get_capl(df_cleaned) |>
      # Keep the participants who have completed the study only
      filter(as.numeric(as.character(identifiant)) <= 98)
  ), 
  
  # Render report
  tar_render(report, "report.Rmd", output_dir = "out/")
  )
