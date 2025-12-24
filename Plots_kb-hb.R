## ============================================================
## QTL PYRAMIDING — PRE-ANALYSIS PIPELINE (POSTER VERSION)
## FINAL — FIXED NAMES + LOCATION-AWARE TRAITS + PARENT LEGEND
## ============================================================

## 0) PACKAGES --------------------------------------------------
pkgs <- c("readxl","dplyr","stringr","ggplot2","grid","ggtext")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

## 0B) VECTOR-SAFE DEFAULT OPERATOR -----------------------------
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 0) return(y)
  if (all(is.na(x))) return(y)
  x
}

## 1) PATHS ----------------------------------------------------
base_dir <- "C:/Users/adele.jamalzei/Desktop/Dissertation/QTL Pyramiding Project/2025 Data/Analysis/Analysis"
file_qtl <- file.path(base_dir, "Master_QTL_Pyramiding_Simplified.xlsx")

out_dir <- file.path(base_dir, "PreAnalysis_Results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## 2) CLEAN COLUMN NAMES ---------------------------------------
clean_names <- function(x){
  x |>
    str_trim() |>
    str_replace_all("[\\s\\.]+", "_") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()
}

## 3) LOAD DATA ------------------------------------------------
raw_data <- read_excel(file_qtl)
names(raw_data) <- clean_names(names(raw_data))

## 4) FIX EXCEL-INDUCED NAME DAMAGE ----------------------------
rename_safe <- function(df, old, new) {
  if (old %in% names(df)) names(df)[names(df) == old] <- new
  df
}

raw_data <- raw_data |>
  rename_safe("twtgl", "twt_g_l") |>
  rename_safe("biomassdry", "biomass_dry") |>
  rename_safe("biomassgreen", "biomass_green") |>
  rename_safe("yield_gp", "yield_g_p") |>
  rename_safe("yield_kgha", "yield_kg_ha")

## 5) REQUIRED METADATA ----------------------------------------
required_cols <- c(
  "location","year",
  "qtn6b","elf","gw2",
  "recurrent_parent","donor_parent"
)

missing_cols <- setdiff(required_cols, names(raw_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

raw_data <- raw_data %>%
  mutate(
    location = str_trim(as.character(location)),
    year = str_trim(as.character(year)),
    recurrent_parent = str_trim(as.character(recurrent_parent)),
    donor_parent = str_trim(as.character(donor_parent)),
    qtn6b = str_trim(as.character(qtn6b)),
    elf   = str_trim(as.character(elf)),
    gw2   = str_trim(as.character(gw2))
  )

## 6) FORCE NUMERIC TRAITS -------------------------------------
numeric_traits <- c(
  "tiller_number","prod_tiller_number","h_date","plant_ht",
  "prot_12","prot_ai","prot_db","moisture","hardness",
  "twt_g_l","test_wt","dtm",
  "grain_length","grain_width","spike_length",
  "grain_number","spikelets_no","grain_weight","tgw",
  "biomass_dry","biomass_green","yield_g_p","yield_kg_ha"
)

numeric_traits <- intersect(numeric_traits, names(raw_data))

raw_data <- raw_data %>%
  mutate(across(all_of(numeric_traits), ~ suppressWarnings(as.numeric(.))))

## 7) PRETTY LABELS --------------------------------------------
pretty_names <- list(
  tiller_number       = "Tiller Number",
  prod_tiller_number  = "Productive Tiller Number",
  h_date              = "Heading Date",
  plant_ht            = "Plant Height",
  prot_12             = "Protein 12%",
  prot_ai             = "Protein AI",
  prot_db             = "Protein DB",
  moisture            = "Grain Moisture",
  hardness            = "Grain Hardness",
  twt_g_l             = "Test Weight (g/L)",
  test_wt             = "Test Weight (lb/bu)",
  dtm                 = "Days to Maturity",
  grain_length        = "Grain Length",
  grain_width         = "Grain Width",
  spike_length        = "Spike Length",
  grain_number        = "Grain Number",
  spikelets_no        = "Spikelet Number",
  grain_weight        = "Grain Weight",
  tgw                 = "Thousand Grain Weight",
  biomass_dry         = "Dry Biomass",
  biomass_green       = "Green Biomass",
  yield_g_p           = "Yield (g/plot)",
  yield_kg_ha         = "Yield (kg/ha)"
)

pretty_units <- list(
  tiller_number="count",
  prod_tiller_number="count",
  h_date="days",
  plant_ht="cm",
  prot_12="%",
  prot_ai="%",
  prot_db="%",
  moisture="%",
  hardness="",
  twt_g_l="g/L",
  test_wt="lb/bu",
  dtm="days",
  grain_length="mm",
  grain_width="mm",
  spike_length="cm",
  grain_number="count",
  spikelets_no="count",
  grain_weight="g",
  tgw="g",
  biomass_dry="g",
  biomass_green="g",
  yield_g_p="g/plot",
  yield_kg_ha="kg/ha"
)

## 8) INTROGRESSION STATUS -------------------------------------
raw_data <- raw_data %>%
  mutate(
    introgression_status = case_when(
      qtn6b == "+" & elf == "+" & gw2 == "+" ~ "MT",
      qtn6b == "-" & elf == "-" & gw2 == "-" ~ "WT",
      TRUE ~ NA_character_
    )
  )

## 9) LOCATION-SPECIFIC TRAIT EXCLUSIONS ------------------------
traits_excluded_by_location <- list(
  Pullman = character(0),
  Othello = c("dtm"),
  Almota  = c("grain_number","grain_width","grain_length","grain_weight","tgw")
)

## 10) COLORS --------------------------------------------------
col_recurrent <- "#2b6cb0"
col_donor     <- "#2f855a"

parent_labeller <- labeller(
  recurrent_parent = function(x)
    paste0("<span style='color:", col_recurrent, ";'><b>", x, "</b></span>"),
  donor_parent = function(x)
    paste0("<span style='color:", col_donor, ";'><b>", x, "</b></span>")
)

## 11) DUMMY DATA FOR PARENT LEGEND -----------------------------
legend_parent <- data.frame(
  parent_status = factor(
    c("Recurrent parent","Donor parent"),
    levels = c("Recurrent parent","Donor parent")
  ),
  x = c("WT","WT"),
  y = c(NA, NA)
)

## 12) LOCATION LOOP --------------------------------------------
locations <- sort(unique(raw_data$location))
locations <- locations[locations != "" & !is.na(locations)]

for (loc in locations) {
  
  data_loc <- raw_data %>% filter(location == loc)
  if (nrow(data_loc) == 0) next
  
  year_label <- paste(sort(unique(data_loc$year)), collapse = "_")
  
  plot_dir <- file.path(out_dir, loc, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  excluded_traits <- traits_excluded_by_location[[loc]] %||% character(0)
  
  traits_to_plot <- setdiff(
    intersect(names(pretty_names), names(raw_data)),
    excluded_traits
  )
  
  plot_data <- data_loc %>%
    filter(!is.na(introgression_status)) %>%
    mutate(
      introgression_status = factor(introgression_status, levels = c("WT","MT")),
      recurrent_parent = factor(recurrent_parent, levels = c("Kelse","Scarlet")),
      donor_parent     = factor(donor_parent, levels = c("Kingbird","PI683503"))
    )
  
  for (tr in traits_to_plot) {
    
    df_tr <- plot_data %>% filter(!is.na(.data[[tr]]))
    if (nrow(df_tr) < 5) next
    
    pretty <- pretty_names[[tr]] %||% tr
    unit   <- pretty_units[[tr]] %||% ""
    ylab   <- if (unit == "") pretty else paste0(pretty, " (", unit, ")")
    
    p <- ggplot(
      df_tr,
      aes(x = introgression_status, y = .data[[tr]], fill = introgression_status)
    ) +
      geom_boxplot(outlier.shape = NA, alpha = 0.85) +
      geom_jitter(width = 0.15, size = 1.8, alpha = 0.7, color = "black") +
      
      facet_wrap(
        ~ recurrent_parent + donor_parent,
        ncol = 4,
        labeller = parent_labeller
      ) +
      
      scale_fill_manual(
        values = c("WT" = "#bdbdbd", "MT" = "#d73027"),
        name = "Introgression Status",
        labels = c(
          "WT" = "Wild Type (without pyramided QTL)",
          "MT" = "Mutant Type (with pyramided QTL)"
        )
      ) +
      
      ## Parent Status legend
      geom_point(
        data = legend_parent,
        aes(x = x, y = y, color = parent_status),
        inherit.aes = FALSE,
        alpha = 0
      ) +
      scale_color_manual(
        values = c(
          "Recurrent parent" = col_recurrent,
          "Donor parent"     = col_donor
        ),
        labels = c(
          "Recurrent parent" =
            paste0("<span style='color:", col_recurrent, ";'><b>Recurrent parent</b></span>"),
          "Donor parent" =
            paste0("<span style='color:", col_donor, ";'><b>Donor parent</b></span>")
        ),
        name = "Parent Status"
      ) +
      
      guides(
        fill  = guide_legend(order = 1),
        color = guide_legend(order = 2,
                             override.aes = list(alpha = 1, size = 3, shape = 16))
      ) +
      
      labs(
        title = paste0(
          pretty,
          " – Introgression Status by Recurrent Parent × Donor Parent | ",
          loc, " ", year_label
        ),
        x = "Introgression Status",
        y = ylab
      ) +
      
      theme_bw(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        legend.text  = ggtext::element_markdown(),
        strip.text   = ggtext::element_markdown()
      )
    
    ggsave(
      file.path(plot_dir, paste0("FullComparison_", tr, "_", loc, ".png")),
      p,
      width = 10,
      height = 6,
      dpi = 320
    )
  }
}

cat("\n===== PRE-ANALYSIS COMPLETED SUCCESSFULLY =====\n")


# Run this to see how many rows are being picked for each "box" in your plot
check_counts <- raw_data %>%
  filter(!is.na(introgression_status)) %>%
  group_by(location, recurrent_parent, donor_parent, introgression_status) %>%
  tally()

print(check_counts)
