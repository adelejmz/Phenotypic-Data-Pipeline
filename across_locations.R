## ============================================================
## QTL PYRAMIDING — PRE-ANALYSIS PIPELINE across location (scarlet and kelse separatly or together )
## ACROSS LOCATIONS — FINAL, STABLE, ERROR-FREE
## ============================================================

## 0) PACKAGES --------------------------------------------------
pkgs <- c("readxl","dplyr","stringr","ggplot2","grid","ggtext","ggnewscale")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

## SAFE DEFAULT OPERATOR ---------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 0) return(y)
  if (all(is.na(x))) return(y)
  x
}

## 1) PATHS -----------------------------------------------------
base_dir <- "C:/Users/adele.jamalzei/Desktop/Dissertation/QTL Pyramiding Project/2025 Data/Analysis/Analysis"
file_qtl <- file.path(base_dir, "Master_QTL_Pyramiding_Simplified.xlsx")

out_dir <- file.path(base_dir, "PreAnalysis_Results", "Across_Locations")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## 2) CLEAN COLUMN NAMES ---------------------------------------
clean_names <- function(x){
  x |>
    stringr::str_trim() |>
    stringr::str_replace_all("[\\s\\.]+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()
}

## 3) LOAD DATA ------------------------------------------------
raw_data <- readxl::read_excel(file_qtl)
names(raw_data) <- clean_names(names(raw_data))

required_cols <- c(
  "location","year","qtn6b","elf","gw2",
  "recurrent_parent","donor_parent"
)

raw_data <- raw_data %>%
  mutate(across(all_of(required_cols), ~ str_trim(as.character(.))))


# 4) FORCE NUMERIC TRAITS -------------------------------------
cols_to_ignore <- c(
  "location","year","qtn6b","elf","gw2",
  "recurrent_parent","donor_parent","block","entry"
)

numeric_traits <- setdiff(names(raw_data), cols_to_ignore)

raw_data <- raw_data %>%
  mutate(across(all_of(numeric_traits), ~ suppressWarnings(as.numeric(.))))

## 5) PRETTY LABELS --------------------------------------------
pretty_names <- list(
  tiller_number      = "Tiller Number",
  prod_tiller_number = "Productive Tiller Number",
  h_date             = "Heading Date",
  plant_ht           = "Plant Height",
  prot_12            = "Protein (12%)",
  prot_ai            = "Protein (As Is)",
  prot_db            = "Protein (Dry Basis)",
  moisture           = "Grain Moisture",
  hardness           = "Grain Hardness",
  twtgl              = "Test Weight (g/L)",
  test_wt            = "Test Weight (lb/bu)",
  dtm                = "Days to Maturity",
  grain_length       = "Grain Length",
  grain_width        = "Grain Width",
  spike_length       = "Spike Length",
  grain_number       = "Grain Number",
  spikelets_no       = "Spikelet Number",
  grain_weight       = "Grain Weight",
  tgw                = "Thousand Grain Weight",
  biomass_dry        = "Dry Biomass",
  biomass_green      = "Green Biomass",
  yield_g_p          = "Yield (g/plot)",
  yield_kg_ha        = "Yield (kg/ha)"
)

pretty_units <- list(
  tiller_number="count", prod_tiller_number="count",
  h_date="days", plant_ht="cm",
  prot_12="%", prot_ai="%", prot_db="%",
  moisture="%", hardness="",
  twtgl="g/L", test_wt="lb/bu",
  dtm="days",
  grain_length="mm", grain_width="mm",
  spike_length="cm",
  grain_number="count", spikelets_no="count",
  grain_weight="g", tgw="g",
  biomass_dry="g", biomass_green="g",
  yield_g_p="g/plot", yield_kg_ha="kg/ha"
)

## 6) INTROGRESSION STATUS -------------------------------------
raw_data <- raw_data %>%
  mutate(
    introgression_status = case_when(
      qtn6b == "+" & elf == "+" & gw2 == "+" ~ "MT",
      qtn6b == "-" & elf == "-" & gw2 == "-" ~ "WT",
      TRUE ~ NA_character_
    )
  )

## 7) PREP PLOTTING DATA ---------------------------------------
plot_data <- raw_data %>%
  filter(!is.na(introgression_status)) %>%
  mutate(
    location = factor(location),
    introgression_status = factor(introgression_status, levels = c("WT","MT")),
    recurrent_parent = factor(recurrent_parent, levels = c("Kelse","Scarlet")),
    donor_parent     = factor(donor_parent, levels = c("Kingbird","PI683503"))
  )

## 8) COLORS ---------------------------------------------------
col_recurrent <- "#2b6cb0"
col_donor     <- "#2f855a"

## 9) STRIP LABELLER -------------------------------------------
parent_labeller <- function(labels) {
  out <- lapply(names(labels), function(d) {
    if (d == "recurrent_parent") {
      paste0("<span style='color:", col_recurrent, ";'><b>", labels[[d]], "</b></span>")
    } else if (d == "donor_parent") {
      paste0("<span style='color:", col_donor, ";'><b>", labels[[d]], "</b></span>")
    } else labels[[d]]
  })
  names(out) <- names(labels)
  out
}

## 10) PARENT LEGEND DUMMY -------------------------------------
legend_parent <- data.frame(
  parent_status = factor(
    c("Recurrent parent","Donor parent"),
    levels = c("Recurrent parent","Donor parent")
  ),
  x = factor("WT", levels = c("WT","MT")),
  y = NA
)

## 11) TRAITS TO PLOT ------------------------------------------
traits_to_plot <- numeric_traits

cat("Plotting", length(traits_to_plot), "traits across locations\n")

## 12) PLOTTING LOOP -------------------------------------------
for (tr in traits_to_plot) {
  
  df_tr <- plot_data %>%
    mutate(
      exclude_trait = case_when(
        location == "Othello" & tr == "dtm" ~ TRUE,
        location == "Almota"  & tr %in% c(
          "grain_number","grain_width",
          "grain_length","grain_weight","tgw"
        ) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!exclude_trait, !is.na(.data[[tr]])) %>%
    select(-exclude_trait)
  
  if (nrow(df_tr) < 5) next
  
  pretty <- pretty_names[[tr]] %||% tr
  unit   <- pretty_units[[tr]] %||% ""
  ylab   <- if (unit == "") pretty else paste0(pretty, " (", unit, ")")
  
  p <- ggplot(df_tr,
              aes(x = introgression_status,
                  y = .data[[tr]],
                  fill = introgression_status)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.85) +
    geom_jitter(width = 0.15, size = 1.8, alpha = 0.7, color = "black") +
    
    facet_grid(location ~ recurrent_parent + donor_parent,
               labeller = parent_labeller) +
    
    scale_fill_manual(
      values = c("WT"="#bdbdbd","MT"="#d73027"),
      name = "**Introgression Status**",
      labels = c("WT"="Wild Type","MT"="Mutant Type")
    ) +
    
    new_scale_color() +
    geom_point(
      data = legend_parent,
      aes(x = x, y = y, color = parent_status),
      inherit.aes = FALSE, alpha = 0
    ) +
    scale_color_manual(
      values = c(
        "Recurrent parent" = col_recurrent,
        "Donor parent"     = col_donor
      ),
      name = "**Parent Status**"
    ) +
    
    guides(
      fill  = guide_legend(order = 2),
      color = guide_legend(order = 1,
                           override.aes = list(alpha=1,size=4,shape=16))
    ) +
    
    labs(
      title = paste0(
        pretty,
        " – Introgression Status by Recurrent Parent × Donor Parent | Across Locations"
      ),
      x = "Introgression Status",
      y = ylab
    ) +
    
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face="bold", hjust=0.5),
      legend.position = "top",
      legend.title = element_markdown(),
      legend.text  = element_markdown(),
      strip.text   = element_markdown(face="bold")
    )
  
  ggsave(
    filename = file.path(out_dir,
                         paste0("FullComparison_", tr, "_AcrossLocations.png")),
    plot = p,
    width = 14,
    height = 9,
    dpi = 320
  )
}

cat("\n===== ACROSS-LOCATIONS PRE-ANALYSIS COMPLETED SUCCESSFULLY =====\n")
