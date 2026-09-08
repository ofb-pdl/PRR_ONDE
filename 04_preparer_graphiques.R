##%###############################%##
#                                   #
#### Préparation des graphiques  ####
#                                   #
##%###############################%##

'%>%' <- dplyr::'%>%'

source("_config.R")

load("data/onde_data/to_update.rda")
to_update <- TRUE
load("data/processed_data/donnees_pour_graphiques.rda")

if (to_update) {
  library(sf)
  
  ## Cartes
  ### Préparation données
  
  ### popups
  
  produire_graph_pour_une_station <- 
    function(station_vec, onde_df, type_mod, mod_levels, mod_colors){
      
      prov <- onde_df %>%
        dplyr::mutate(
          Annee = factor(Annee, levels = min(Annee):max(Annee))
        ) %>% 
      dplyr::filter(code_station == station_vec) %>% 
          dplyr::mutate(label_p = paste0(libelle_type_campagne,'\n',{{type_mod}},'\n',date_campagne),
                 label_sta = paste0(libelle_station,' (',code_station,')'),
                 label_png = paste0("ONDE_dpt",code_departement,"_",label_sta)) %>% 
          dplyr::rename(modalite = {{type_mod}}) %>% 
          (function(df_temp) {
            dplyr::bind_rows(
              df_temp %>% 
                dplyr::filter(libelle_type_campagne == "complémentaire"),
              df_temp %>% 
                dplyr::filter(
                  libelle_type_campagne == "usuelle"
                ) %>% 
                tidyr::complete(
                  code_station, 
                  libelle_station,
                  Annee, 
                  Mois,
                  fill = list(
                    libelle_type_campagne = "usuelle",
                    modalite = "Donnée manquante"
                  )
                ) %>% 
                dplyr::mutate(
                  date_campagne = dplyr::if_else(
                    is.na(date_campagne),
                    lubridate::as_date(paste0(Annee, "-", as.numeric(Mois), "-25")),
                    date_campagne
                  )
                ) %>% 
                dplyr::filter(
                  date_campagne <= Sys.Date()
                )
            ) %>% 
              dplyr::arrange(
                Annee, Mois, dplyr::desc(libelle_type_campagne)
              )
            
          }) %>% 
          dplyr::mutate(
            modalite = stringr::str_wrap(modalite, 12) %>% 
              factor(levels = mod_levels)
          ) %>% 
        dplyr::mutate(Annee = Annee %>% 
                        as.character() %>% 
                        as.numeric())
      
      nom_station <- unique(prov$label_sta)
      nom_station_graph <- unique(prov$label_png)
      
      graph1 <- prov %>% 
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = Annee,
            y = as.numeric(Mois)
            )
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            fill = stringr::str_wrap(modalite, 12),
            shape = libelle_type_campagne,
            size = libelle_type_campagne,
          ), 
          col='black'
            ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = mod_colors, 
          breaks = levels(prov$modalite), 
          name = 'Modalités'
          ) +
        ggplot2::scale_shape_manual(
          values = c(21,22),
          name = 'Type campagne'
          ) +
        ggplot2::scale_size_manual(
          values = c(5,10),
          name = 'Type campagne'
          ) +
        ggplot2::scale_y_continuous(
          breaks = 1:12, 
          labels = 1:12, 
          limits = c(1, 12)
          ) +
        ggplot2::scale_x_continuous(
          breaks = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T),
          labels = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T)
          ) +
        ggplot2::labs(
          x = "", y = "Mois", 
          title = unique(prov$libelle_station),
          subtitle = unique(prov$code_station)
          ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          title = ggplot2::element_text(face = 'bold'),
          axis.text.x = ggplot2::element_text(size=10),
          axis.text.y = ggplot2::element_text(size=10),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes=list(shape = 22, size = 5))
          )
      
      graph1
    }
  
  if (dir.exists("www/png"))
    unlink("www/png", recursive = TRUE)
  if (dir.exists("www/popups"))
    unlink("www/popups", recursive = TRUE)
  
  dir.create("www/popups/3mod", recursive = TRUE)
  dir.create("www/popups/4mod", recursive = TRUE)
  
  save_popups <- function(graphs, dir) {
    purrr::walk(
      names(graphs),
      function(station) {
        tmp <- tempfile(pattern = "popup", fileext = ".png")
        
        ggplot2::ggsave(
          plot = graphs[[station]],
          filename = tmp,
          width = 14,
          height = 10,
          units = "cm",
          dpi = 150
        )
        
        png::readPNG(tmp) %>% 
          webp::write_webp(target = paste0(dir, station, ".webp"))
      }
    )
  }
  
  ### -> graphiques 3modalités
  purrr::map(
      .x = stations_onde_geo_usuelles$code_station, 
      .f = produire_graph_pour_une_station, 
      type_mod = lib_ecoul3mod,
      onde_df = onde_periode,
      mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible", "Observation\nimpossible", "Donnée\nmanquante"),
      mod_colors = mes_couleurs_3mod
      ) %>% 
    purrr::set_names(stations_onde_geo_usuelles$code_station) %>% 
    save_popups(dir = "www/popups/3mod/")
  
  ### -> graphiques 4modalités
  purrr::map(
      .x = stations_onde_geo_usuelles$code_station, 
      .f = produire_graph_pour_une_station, 
      type_mod = lib_ecoul4mod,
      onde_df = onde_periode,
      mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible\nfaible", "Ecoulement\nvisible\nacceptable", "Observation\nimpossible", "Donnée\nmanquante"),
      mod_colors = mes_couleurs_4mod
      ) %>% 
    purrr::set_names(stations_onde_geo_usuelles$code_station) %>% 
    save_popups(dir = "www/popups/4mod/")
  
  ### -> graphiques 3modalités anciennes stations
  purrr::map(
      .x = stations_inactives_onde_geo$code_station, 
      .f = produire_graph_pour_une_station, 
      type_mod = lib_ecoul3mod,
      onde_df = onde_anciennes_stations,
      mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible", "Observation\nimpossible", "Donnée\nmanquante"),
      mod_colors = mes_couleurs_3mod
    ) %>% 
    purrr::set_names(stations_inactives_onde_geo$code_station) %>% 
    save_popups(dir = "www/popups/3mod/")

  ### -> graphiques 4modalités anciennes stations
  purrr::map(
      .x = stations_inactives_onde_geo$code_station, 
      .f = produire_graph_pour_une_station, 
      type_mod = lib_ecoul4mod,
      onde_df = onde_anciennes_stations,
      mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible\nfaible", "Ecoulement\nvisible\nacceptable", "Observation\nimpossible", "Donnée\nmanquante"),
      mod_colors = mes_couleurs_4mod
    ) %>% 
    purrr::set_names(stations_inactives_onde_geo$code_station) %>% 
    save_popups(dir = "www/popups/4mod/")

  ### -> graphiques 3modalités stations onde+
  purrr::map(
    .x = stations_onde_plus_geo$code_station, 
    .f = produire_graph_pour_une_station, 
    type_mod = lib_ecoul3mod,
    onde_df = onde_plus,
    mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible", "Observation\nimpossible", "Donnée\nmanquante"),
    mod_colors = mes_couleurs_3mod
  ) %>% 
    purrr::set_names(stations_onde_plus_geo$code_station) %>% 
    save_popups(dir = "www/popups/3mod/")
  
  ### -> graphiques 4modalités stations onde+
  purrr::map(
    .x = stations_onde_plus_geo$code_station, 
    .f = produire_graph_pour_une_station, 
    type_mod = lib_ecoul4mod,
    onde_df = onde_plus,
    mod_levels = c("Assec", "Ecoulement\nnon visible", "Ecoulement\nvisible\nfaible", "Ecoulement\nvisible\nacceptable", "Observation\nimpossible", "Donnée\nmanquante"),
    mod_colors = mes_couleurs_4mod
  ) %>% 
    purrr::set_names(stations_onde_plus_geo$code_station) %>% 
    save_popups(dir = "www/popups/4mod/")
  
  
  
  ## Conditions d'écoulement lors des campagnes usuelles de l'année en cours
plot_bilan_prop <- function(data_bilan, lib_ecoulement, regional = FALSE, modalites = ggplot2::waiver()) {
  data_bilan %>% 
    
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = frq, 
        x = forcats::fct_rev(factor(Mois)),
        fill= forcats::fct_rev({{lib_ecoulement}}), 
        label=Label_p
        )
      ) +
    ggplot2::geom_bar(
      position = "stack", 
      stat = "identity",
      alpha = 0.7, 
      colour = 'black', 
      width = 0.7, 
      linewidth = 0.01
    ) +
    {
      if(regional == FALSE)
      ggplot2::facet_grid(~code_departement)
      } +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = Label_p),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3,
      colour = "black",
      fontface = "bold.italic"
    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Pourcentage (%)") +
    ggplot2::xlab("Mois") +
    ggplot2::scale_fill_manual(
      name = "Situation stations",
      values = c("Donnée manquante" = "grey90",
                 "Observation impossible" = "grey50",
                 "Assec" = "#d73027",
                 "Ecoulement non visible" = "#fe9929",
                 "Ecoulement visible faible" = "#FFFF80",
                 "Ecoulement visible acceptable" = "#4575b4",
                 "Ecoulement visible" = "#4575b4"
                 ),
      breaks = modalites,
      drop = TRUE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(size = 11, face = "bold"), 
      legend.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11, face = 'bold'),
      axis.text.y = ggplot2::element_text(size = 11, colour = 'black'),
      axis.text.x = ggplot2::element_text(size = 11, colour = 'black'),
      strip.text.x = ggplot2::element_text(size = 11, color = "black", face = "bold"),
      strip.background = ggplot2::element_rect(
        color="black", fill="grey80", linewidth = 1, linetype="solid"
      ),
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_line(colour = NA),
      legend.position = "bottom",
      plot.background = ggplot2::element_blank(),
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = FALSE)
      )
}

  bilan_cond_reg_typo_nat <- plot_bilan_prop(
    df_categ_obs_3mod_reg %>% 
      dplyr::mutate(lib_ecoul3mod = forcats::fct_rev(lib_ecoul3mod)),
    lib_ecoulement = lib_ecoul3mod, 
    regional = TRUE,
    modalites = c("Donnée manquante", "Observation impossible", "Assec", "Ecoulement non visible", "Ecoulement visible")
    )
  
  bilan_cond_reg_typo_dep <- plot_bilan_prop(
    df_categ_obs_4mod_reg %>% 
      dplyr::mutate(lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)), 
    lib_ecoulement = lib_ecoul4mod, 
    regional = TRUE,
    modalites = c("Donnée manquante", "Observation impossible", "Assec", "Ecoulement non visible", "Ecoulement visible faible", "Ecoulement visible acceptable")
  )
  
  bilan_cond_dep <- conf_dep %>% 
    purrr::map(
      function(d) {
        if (d %in% df_categ_obs_4mod$code_departement) {
          plot_bilan_prop(
            df_categ_obs_4mod %>% 
              dplyr::filter(code_departement == d) %>% 
              dplyr::mutate(lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)), 
            lib_ecoulement = lib_ecoul4mod,
            modalites = c("Donnée manquante", "Observation impossible", "Assec", "Ecoulement non visible", "Ecoulement visible faible", "Ecoulement visible acceptable")
          )
        }
      }
    ) %>% 
    purrr::set_names(conf_dep)
  
  ## Relevés pour le mois en cours les années précédentes
  
  preparer_donnees_mois <- function(mois_selectionne) {
    
    onde_usuel %>% 
      dplyr::mutate(
        mois_num = as.integer(Mois),
        mois_nom = factor(
          Mois,
          levels = c(
            "01", "02", "03", "04",
            "05", "06", "07", "08",
            "09", "10", "11", "12"
          ),
          labels = c(
            "Janvier", "Février", "Mars", "Avril",
            "Mai", "Juin", "Juillet", "Août",
            "Septembre", "Octobre", "Novembre", "Décembre"
          )
        )
      ) %>% 
      dplyr::filter(
        mois_num == mois_selectionne
      )
  }
  
  # Préparation des données pour les mois ONDE
  mois_onde <- 5:9
  
  donnees_mois <- purrr::map(
    mois_onde,
    preparer_donnees_mois
  )
  
  names(donnees_mois) <- c(
    "Mai",
    "Juin",
    "Juillet",
    "Août",
    "Septembre"
  )
  
  calculer_bilan_mois <- function(df_mois) {
    
    ## Données régionales (typologie nationale - 3 modalités)
    df_bilan_mois_reg_nat <- df_mois %>% 
      dplyr::group_by(
        Annee,
        lib_ecoul3mod
      ) %>% 
      dplyr::summarise(
        nb_station = dplyr::n_distinct(code_station),
        .groups = "drop"
      ) %>% 
      dplyr::group_by(Annee) %>% 
      dplyr::mutate(
        pct_station = nb_station / sum(nb_station) * 100
      ) %>% 
      dplyr::ungroup()
    
    
    ## Données régionales (typologie départementale - 4 modalités)
    df_bilan_mois_reg_dep <- df_mois %>% 
      dplyr::group_by(
        Annee,
        lib_ecoul4mod
      ) %>% 
      dplyr::summarise(
        nb_station = dplyr::n_distinct(code_station),
        .groups = "drop"
      ) %>% 
      dplyr::group_by(Annee) %>% 
      dplyr::mutate(
        pct_station = nb_station / sum(nb_station) * 100
      ) %>% 
      dplyr::ungroup()
    
    
    ## Données départementales (4 modalités)
    df_bilan_mois_dep <- df_mois %>% 
      dplyr::group_by(
        code_departement,
        Annee,
        lib_ecoul4mod
      ) %>% 
      dplyr::summarise(
        nb_station = dplyr::n_distinct(code_station),
        .groups = "drop"
      ) %>% 
      dplyr::group_by(
        code_departement,
        Annee
      ) %>% 
      dplyr::mutate(
        pct_station = nb_station / sum(nb_station) * 100
      ) %>% 
      dplyr::ungroup()
    
    
    return(
      list(
        reg_nat = df_bilan_mois_reg_nat,
        reg_dep = df_bilan_mois_reg_dep,
        dep = df_bilan_mois_dep
      )
    )
  }
  
  bilans_mois <- purrr::map(
    donnees_mois,
    calculer_bilan_mois
  )
  
  ## Fonction graphique
  plot_bilan_mois <- function(data_bilan, lib_ecoulement, regional = FALSE, modalites = ggplot2::waiver()) {
    
    data_bilan %>% 
      ggplot2::ggplot(
        ggplot2::aes(
          x = factor(Annee),
          y = pct_station,
          fill = forcats::fct_rev({{lib_ecoulement}})
        )
      ) +
      
      ggplot2::geom_bar(
        stat = "identity",
        position = "stack",
        alpha = 0.7,
        width = 0.7
      ) +
      
      {
        if(regional == FALSE)
          ggplot2::facet_grid(~code_departement)
      } +
      
      ggplot2::scale_fill_manual(
        name = "Situation stations",
        values = c(
          "Donnée manquante" = "grey90",
          "Observation impossible" = "grey50",
          "Assec" = "#d73027",
          "Ecoulement non visible" = "#fe9929",
          "Ecoulement visible faible" = "#FFFF80",
          "Ecoulement visible acceptable" = "#4575b4",
          "Ecoulement visible" = "#4575b4"
        ),
        breaks = modalites,
        drop = TRUE
      ) +
      
      ggplot2::ggtitle(
        glue::glue(
          "Évolution des conditions d'écoulement"
        )
      ) +
      
      ggplot2::ylab("") +
      ggplot2::xlab("Année") +
      
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        breaks = seq(0,100,20),
        expand = c(0,0),
        sec.axis = ggplot2::sec_axis(
          ~.,
          breaks = seq(0,100,20),
          labels = scales::label_percent(scale = 1)
        )
      ) +
      ggplot2::coord_cartesian(ylim = c(0,100)) +
      
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(
          size = 11,
          face = "bold"
        ),
        legend.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(
          size = 11,
          face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = 11,
          angle = 45,
          hjust = 1
        ),
        strip.text.x = ggplot2::element_text(
          size = 11,
          face = "bold"
        ),
        strip.background = ggplot2::element_rect(
          color = "black",
          fill = "grey80"
        ),
        legend.position = "bottom"
      ) +
      
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          nrow = 2,
          byrow = FALSE
        )
      )
  }
  
  
  creer_graphs_mois <- function(bilan_mois) {
    
    
    # Graph régional - typologie nationale
    bilan_mois_reg_typo_nat <- plot_bilan_mois(
      bilan_mois$reg_nat %>% 
        dplyr::mutate(
          lib_ecoul3mod = factor(
            lib_ecoul3mod,
            levels = c(
              "Donnée manquante",
              "Observation impossible",
              "Assec",
              "Ecoulement non visible",
              "Ecoulement visible"
            )
          ),
          lib_ecoul3mod = forcats::fct_rev(lib_ecoul3mod)
        ),
      lib_ecoulement = lib_ecoul3mod,
      regional = TRUE,
      modalites = c(
        "Donnée manquante",
        "Observation impossible",
        "Assec",
        "Ecoulement non visible",
        "Ecoulement visible"
      )
    )
    
    
    # Graph régional - typologie départementale
    bilan_mois_reg_typo_dep <- plot_bilan_mois(
      bilan_mois$reg_dep %>% 
        dplyr::mutate(
          lib_ecoul4mod = factor(
            lib_ecoul4mod,
            levels = c(
              "Donnée manquante",
              "Observation impossible",
              "Assec",
              "Ecoulement non visible",
              "Ecoulement visible faible",
              "Ecoulement visible acceptable"
            )
          ),
          lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)
        ),
      lib_ecoulement = lib_ecoul4mod,
      regional = TRUE,
      modalites = c(
        "Donnée manquante",
        "Observation impossible",
        "Assec",
        "Ecoulement non visible",
        "Ecoulement visible faible",
        "Ecoulement visible acceptable"
      )
    )
    
    
    # Graphs départementaux
    bilan_mois_dep <- conf_dep %>% 
      purrr::map(
        function(d) {
          
          plot_bilan_mois(
            bilan_mois$dep %>% 
              dplyr::filter(
                code_departement == d
              ) %>% 
              dplyr::mutate(
                lib_ecoul4mod = factor(
                  lib_ecoul4mod,
                  levels = c(
                    "Donnée manquante",
                    "Observation impossible",
                    "Assec",
                    "Ecoulement non visible",
                    "Ecoulement visible faible",
                    "Ecoulement visible acceptable"
                  )
                ),
                lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)
              ),
            lib_ecoulement = lib_ecoul4mod,
            modalites = c(
              "Donnée manquante",
              "Observation impossible",
              "Assec",
              "Ecoulement non visible",
              "Ecoulement visible faible",
              "Ecoulement visible acceptable"
            )
          )
        }
      ) %>% 
      purrr::set_names(conf_dep)
    
    
    list(
      reg_typo_nat = bilan_mois_reg_typo_nat,
      reg_typo_dep = bilan_mois_reg_typo_dep,
      dep = bilan_mois_dep
    )
  }
  
  ## Création des graphiques par mois
  graphs_mois <- purrr::map(
    names(bilans_mois),
    function(m) {
      
      ## Graph régional typologie nationale
      bilan_reg_nat <- plot_bilan_mois(
        bilans_mois[[m]]$reg_nat %>% 
          dplyr::mutate(
            lib_ecoul3mod = factor(
              lib_ecoul3mod,
              levels = c(
                "Donnée manquante",
                "Observation impossible",
                "Assec",
                "Ecoulement non visible",
                "Ecoulement visible"
              )
            ),
            lib_ecoul3mod = forcats::fct_rev(lib_ecoul3mod)
          ),
        lib_ecoulement = lib_ecoul3mod,
        regional = TRUE,
        modalites = c(
          "Donnée manquante",
          "Observation impossible",
          "Assec",
          "Ecoulement non visible",
          "Ecoulement visible"
        )
      )
      
      
      ## Graph régional typologie départementale
      bilan_reg_dep <- plot_bilan_mois(
        bilans_mois[[m]]$reg_dep %>% 
          dplyr::mutate(
            lib_ecoul4mod = factor(
              lib_ecoul4mod,
              levels = c(
                "Donnée manquante",
                "Observation impossible",
                "Assec",
                "Ecoulement non visible",
                "Ecoulement visible faible",
                "Ecoulement visible acceptable"
              )
            ),
            lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)
          ),
        lib_ecoulement = lib_ecoul4mod,
        regional = TRUE,
        modalites = c(
          "Donnée manquante",
          "Observation impossible",
          "Assec",
          "Ecoulement non visible",
          "Ecoulement visible faible",
          "Ecoulement visible acceptable"
        )
      )
      
      
      ## Graphs départementaux
      bilan_dep <- conf_dep %>% 
        purrr::map(
          function(d) {
            
            plot_bilan_mois(
              bilans_mois[[m]]$dep %>% 
                dplyr::filter(
                  code_departement == d
                ) %>% 
                dplyr::mutate(
                  lib_ecoul4mod = factor(
                    lib_ecoul4mod,
                    levels = c(
                      "Donnée manquante",
                      "Observation impossible",
                      "Assec",
                      "Ecoulement non visible",
                      "Ecoulement visible faible",
                      "Ecoulement visible acceptable"
                    )
                  ),
                  lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)
                ),
              lib_ecoulement = lib_ecoul4mod,
              modalites = c(
                "Donnée manquante",
                "Observation impossible",
                "Assec",
                "Ecoulement non visible",
                "Ecoulement visible faible",
                "Ecoulement visible acceptable"
              )
            )
            
          }
        ) %>% 
        purrr::set_names(conf_dep)
      
      
      list(
        reg_nat = bilan_reg_nat,
        reg_dep = bilan_reg_dep,
        dep = bilan_dep
      )
      
    }
  ) %>% 
    purrr::set_names(names(bilans_mois))
  
  ## Sévérité des assecs
  plot_heatmap <- function(df_heatmap) {
    df_heatmap %>% 
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = Annee, 
          y = forcats::fct_rev(factor(Mois)),
          fill = pourcentage_assecs
          )
      ) + 
      ggplot2::geom_tile(col = 'white', linewidth = 0.5) +
      ggplot2::scale_fill_gradientn(
        "% d\'assecs",
        colors = adjustcolor(c("#4575b4", hcl.colors(9, "OrRd", rev = T)),
                             alpha.f = 0.8),
        values = c(0, seq(0.0001, 1, length.out = 9)),
        limits = c(0,100),
        na.value = adjustcolor("grey90", alpha.f = 0.7)
      ) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = Label_p),
        size = 3.5,
        color = "black",
        fontface = 'bold.italic'
        ) +
      ggplot2::scale_size(guide = 'none') +
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_width(1),
        expand = c(0,0)
        ) +
      ggplot2::ylab("Mois") + 
      ggplot2::xlab(NULL) +
      ggplot2::ggtitle(glue::glue("Proportion de stations en assec")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 11,face = 'bold'), 
        axis.text.x = ggplot2::element_text(size=11,angle = 45,hjust = 1),
        axis.text.y = ggplot2::element_text(size=11),
        legend.position = 'right',
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }
  
  severite_assecs_reg <- plot_heatmap(heatmap_df)
  
  severite_assecs_dep <- conf_dep %>% 
    purrr::map(
      function(d) {
        plot_heatmap(
          heatmap_df_dep %>% 
            dplyr::filter(code_departement == d)
          )
      }
    ) %>% 
    purrr::set_names(conf_dep)
  
  ## Des assecs qui se suivent
  plot_assecs_consecutifs <- function(df_assecs, round_prec = 1) {
    df_assecs %>%
      dplyr::ungroup() %>% 
      dplyr::filter(label != '0 mois') %>% 
      dplyr::mutate(label = factor(
        label, 
        levels = c(
          paste0(5:2, " mois consécutifs"), "1 mois"
        )
      )
      ) %>% 
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = as.factor(Annee), y = pct, fill = label)
      ) + 
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(
        mapping = ggplot2::aes(y = pct, label = nb_station), 
        fontface ="italic",
        size = 3.5,
        position = ggplot2::position_stack(vjust = 0.5),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "1 mois" = "#FFFFB2FF",
          "2 mois consécutifs" = "#FECC5CFF",
          "3 mois consécutifs" = "#FD8D3CFF",
          "4 mois consécutifs" = "#F03B20FF",
          "5 mois consécutifs" = "#BD0026FF"
        ),
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(round_prec)) +
      ggplot2::scale_x_discrete(limits = factor(sort(unique(df_assecs$Annee)))) +
      ggplot2::ggtitle("Proportions et nombre de stations concernées") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 11,face = 'bold'), 
        axis.text.x = ggplot2::element_text(size = 11, angle = 45,hjust = 1),
        axis.text.y = ggplot2::element_text(size=11),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ylab(NULL) + 
      ggplot2::xlab(NULL)
  }
  
  assecs_consecutifs_reg <- plot_assecs_consecutifs(duree_assecs_df)
  
  assecs_consecutifs_dep <- conf_dep %>% 
    purrr::map(
      function(d) {
        plot_assecs_consecutifs(
          duree_assecs_df_dep %>% 
            dplyr::filter(code_departement == d),
          round_prec = .1
          )
      }
    ) %>% 
    purrr::set_names(conf_dep)

      ## Indice ONDE par département
  
  library(plotly)
  
  plot_indice_onde_superpose_interactif <- function(data_indice, departement) {
    
    # --- Préparation des données ---
    df <- data_indice %>%
      dplyr::filter(
        code_departement == departement,
        campagne_complete == TRUE,
        !is.na(indice_onde)
      ) %>%
      dplyr::mutate(
        annee = lubridate::year(date_campagne),
        date_superposee = as.Date(
          paste0("2000-", format(date_campagne, "%m-%d"))
        )
      ) %>%
      dplyr::arrange(annee, date_superposee)
    
    annees    <- sort(unique(df$annee))
    nb_annees <- length(annees)
    
    # Palette dégradée bleu foncé -> rouge foncé
    couleurs <- setNames(
      colorRampPalette(
        c(
          "#2C7BB6", # bleu
          "#00A6CA", # cyan
          "#00CCBC", # turquoise
          "#90EB9D", # vert clair
          "#FFFF8C", # jaune
          "#F9D057", # orange clair
          "#F29E2E", # orange
          "#E76818", # orange foncé
          "#D7191C"  # rouge
        )
      )(nb_annees),
      as.character(annees)
    )
    
    # --- Formes des points ---
    shapes <- c("usuelle" = "square", "complémentaire" = "circle")
    
    # Labels des mois en français ---
    mois_fr <- c(
      "Janv", "Fév", "Mars", "Avril", "Mai", "Juin",
      "Juil", "Août", "Sept", "Oct", "Nov", "Déc"
    )
    
    # Valeurs en millisecondes (milieu de chaque mois pour centrer le label)
    tickvals_mois <- as.numeric(
      as.POSIXct(
        paste0("2000-", sprintf("%02d", 1:12), "-15"),
        tz = "UTC"
      )
    ) * 1000
    
    # -------------------------------------------------------
    # --- Construction des traces dans une liste d'abord  ---
    # -------------------------------------------------------
    
    traces            <- list()
    idx               <- 1
    indices_par_annee <- list()
    
    for (a in annees) {
      
      df_annee      <- df %>% dplyr::filter(annee == a)
      couleur       <- couleurs[as.character(a)]
      indices_annee <- c()
      
      # -- Ligne continue --
      traces[[idx]] <- list(
        x           = df_annee$date_superposee,
        y           = df_annee$indice_onde,
        type        = "scatter",
        mode        = "lines",
        line        = list(color = couleur, width = 2),
        name        = as.character(a),
        legendgroup = as.character(a),
        showlegend  = TRUE,
        hoverinfo   = "skip"
      )
      indices_annee <- c(indices_annee, idx)
      idx <- idx + 1
      
      # -- Points par type de campagne --
      for (type in c("usuelle", "complémentaire")) {
        
        df_type <- df_annee %>%
          dplyr::filter(libelle_type_campagne == type)
        
        if (nrow(df_type) == 0) next
        
        traces[[idx]] <- list(
          x           = df_type$date_superposee,
          y           = df_type$indice_onde,
          type        = "scatter",
          mode        = "markers",
          marker      = list(
            color  = couleur,
            size   = 8,
            symbol = shapes[type]
          ),
          name        = as.character(a),
          legendgroup = as.character(a),
          showlegend  = FALSE,
          text        = paste0(
            "Année : ",  df_type$annee,   "<br>",
            "Date  : ",  format(df_type$date_superposee, "%d %b"), "<br>",
            "Indice : ", sprintf("%.2f", df_type$indice_onde), "<br>",
            "Type  : ",  df_type$libelle_type_campagne
          ),
          hoverinfo   = "text"
        )
        indices_annee <- c(indices_annee, idx)
        idx <- idx + 1
      }
      
      indices_par_annee[[as.character(a)]] <- indices_annee
    }
    
    nb_traces_total <- idx - 1
    
    # -------------------------------------------------------
    # --- Boutons du menu déroulant                       ---
    # -------------------------------------------------------
    
    btn_toutes <- list(
      method = "restyle",
      args   = list(list(visible = as.list(rep(TRUE,  nb_traces_total)))),
      label  = "Toutes"
    )
    
    btns_annees <- purrr::map(annees, function(a) {
      visible <- rep(FALSE, nb_traces_total)
      visible[indices_par_annee[[as.character(a)]]] <- TRUE
      list(
        method = "restyle",
        args   = list(list(visible = as.list(visible))),
        label  = as.character(a)
      )
    })
    
    tous_boutons <- c(list(btn_toutes), btns_annees)
    
    # -------------------------------------------------------
    # --- Assemblage du graphique plotly                  ---
    # -------------------------------------------------------
    
    p <- plotly::plot_ly()
    
    for (tr in traces) {
      p <- p %>% plotly::add_trace(
        x           = tr$x,
        y           = tr$y,
        type        = tr$type,
        mode        = tr$mode,
        line        = tr$line,
        marker      = tr$marker,
        name        = tr$name,
        legendgroup = tr$legendgroup,
        showlegend  = tr$showlegend,
        text        = tr$text,
        hoverinfo   = tr$hoverinfo
      )
    }
    
    # --- Layout ---
    p <- p %>%
      plotly::layout(
        
        autosize = FALSE,
        width = 1000,
        height = 700,
        
        title = list(
          text = paste0("Indice ONDE départemental - ", departement),
          font = list(size = 15)
        ),
        
        # Axe X : mois en français, de janvier à décembre
        xaxis = list(
          range     = c(
            as.numeric(as.POSIXct("2000-01-01", tz = "UTC")) * 1000,
            as.numeric(as.POSIXct("2000-12-31", tz = "UTC")) * 1000
          ),
          tickmode  = "array",
          tickvals  = tickvals_mois,
          ticktext  = mois_fr,
          tickangle = 0
        ),
        
        yaxis = list(
          title = "Indice ONDE",
          range = c(0, 10.5),
          dtick = 1
        ),
        
        legend = list(
          orientation = "h",
          x           = 0,
          y           = -0.08,
          title       = list(text = "<b>Année</b>")
        ),
        
        hovermode = "closest",
        margin    = list(b = 190, t = 70, l = 60, r = 30),
        
        # -- Menu déroulant --
        updatemenus = list(
          list(
            type        = "dropdown",
            direction   = "down",
            x           = 0.12,
            xanchor     = "left",
            y           = 1.15,
            yanchor     = "top",
            showactive  = TRUE,
            active      = 0,
            buttons     = tous_boutons,
            bgcolor     = "#f0f0f0",
            bordercolor = "#cccccc",
            font        = list(size = 13)
          )
        ),
        
        # -- Annotations --
        annotations = list(
          
          list(
            text      = "<b>Sélectionner : </b>",
            x = 0, xref = "paper", xanchor = "left",
            y = 1.13, yref = "paper", yanchor = "top",
            showarrow = FALSE,
            font      = list(size = 13)
          ),
          
          list(
            text      = "<b>Type de campagne :</b>",
            x = 0, xref = "paper", xanchor = "left",
            y = -0.26, yref = "paper", yanchor = "top",
            showarrow = FALSE,
            font      = list(size = 13)
          ),
          
          list(
            text      = "<span style='font-size:20px'>■</span>  usuelle",
            x = 0, xref = "paper", xanchor = "left",
            y = -0.34, yref = "paper", yanchor = "top",
            showarrow = FALSE,
            font      = list(size = 12, color = "grey40")
          ),
          
          list(
            text      = "<span style='font-size:20px'>●</span>  complémentaire",
            x = 0.18, xref = "paper", xanchor = "left",
            y = -0.34, yref = "paper", yanchor = "top",
            showarrow = FALSE,
            font      = list(size = 12, color = "grey40")
          )
        )
      )
    
    p <- p %>%
      plotly::config(
        responsive = TRUE
      )
    
    return(p)
  }

  ## Sauvegarde
  save(
    bilan_cond_reg_typo_nat,
    bilan_cond_reg_typo_dep,
    bilan_cond_dep,
    graphs_mois,
    plot_indice_onde_superpose_interactif,
    indice_onde,
    severite_assecs_reg,
    severite_assecs_dep,
    assecs_consecutifs_reg,
    assecs_consecutifs_dep,
    file = "data/processed_data/graphiques.rda"
    )
}
