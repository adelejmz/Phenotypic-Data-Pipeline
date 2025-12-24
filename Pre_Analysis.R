## ============================================================
## QTL PYRAMIDING — FULL PRE-ANALYSIS PIPELINE (MULTI-LOCATION)
## Using: Master_QTL_Pyramiding_Simplified.xlsx
## ============================================================

## 0) PACKAGES --------------------------------------------------
pkgs <- c(
  "readxl","dplyr","tidyr","stringr","ggplot2",
  "e1071","reshape2","Hmisc","viridis","forcats","car"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

`%||%` <- function(x,y) if(!is.null(x) && !is.na(x)) x else y

## 1) PATHS -----------------------------------------------------
base_dir<- "C:/Users/adele.jamalzei/Desktop/Dissertation/QTL Pyramiding Project/2025 Data/Analysis/Analysis"
file_qtl<- file.path(base_dir, "Master_QTL_Pyramiding_Simplified.xlsx")
out_dir_base <- file.path(base_dir, "PreAnalysis_Results")
dir.create(out_dir_base, recursive = TRUE, showWarnings = FALSE)

## 2) CLEAN NAMES FUNCTION -------------------------------------
clean_names <- function(x){
  x %>%
    str_trim() %>%
    str_replace_all("[\\s\\.]+","_") %>%
    str_replace_all("[^A-Za-z0-9_]","") %>%
    tolower()
}

# ============================================================
# LOCATION-SPECIFIC TRAIT EXCLUSIONS (AUTHORITATIVE)
# ============================================================
# ============================================================
# LOCATION-SPECIFIC TRAIT EXCLUSIONS (UPDATED & CORRECT)
# ============================================================

traits_excluded_by_location <- list(
  
  ## Othello: everything except DTM
  Othello = c("dtm"),
  
  ## Pullman: ALL traits available
  Pullman = character(0),
  
  ## Almota: missing grain traits only
  Almota = c(
    "grain_number",
    "grain_width",
    "grain_length",
    "grain_weight",
    "tgw"
  )
)


## 3) LOAD DATA (GLOBAL) ---------------------------------------
raw_data <- readxl::read_excel(file_qtl)
names(raw_data) <- clean_names(names(raw_data))

## trim location strings just in case (e.g., "Pullman ")
if ("location" %in% names(raw_data)) {
  raw_data$location <- stringr::str_trim(raw_data$location)
}

cat("Rows in full dataset:", nrow(raw_data), "\n")
cat("Columns:\n")
print(names(raw_data))

## 3B) FORCE KNOWN TRAITS TO NUMERIC (SAFE) --------------------
numeric_trait_cols <- c(
  "tiller_number","prod_tiller_number","h_date","plant_ht",
  "prot_12","prot_ai","prot_db","moisture","hardness",
  "twtgl","test_wt","dtm",
  "grain_length","grain_width","spike_length","grain_number",
  "spikelets_no","grain_weight","tgw","biomassdry","biomassgreen",
  "yield_gp","yield_kgha"
)

numeric_trait_cols <- intersect(numeric_trait_cols, names(raw_data))

raw_data <- raw_data %>%
  mutate(across(all_of(numeric_trait_cols), ~ suppressWarnings(as.numeric(.))))

## factors for group counts at the end
factor_cols <- c("bloc","entry","year")

## 4) PRETTY NAMES + UNITS --------------------------------------
## these use the CLEANED column names
pretty_names <- list(
  biomassdry          = "Dry_Biomass",
  biomassgreen        = "Green_Biomass",
  grain_number        = "Grain_Number",
  grain_weight        = "Grain_Weight",
  hardness            = "Grain_Hardness",
  moisture            = "Grain_Moisture",
  plant_ht            = "Plant_Height",
  prod_tiller_number  = "Productive_Tiller_Number",
  prot_12             = "Protein_Concentration_12",
  prot_ai             = "Protein_Concentration_AI",
  prot_db             = "Protein_Concentration_DB",
  grain_length        = "Grain_Length",
  grain_width         = "Grain_Width",
  spike_length        = "Spike_Length",
  spikelets_no        = "Spikelet_Number",
  test_weight         = "Test_Weight_lb_per_bu",
  test_wt             = "Test_Weight_lb_per_bu",
  twtgl               = "Test_Weight_g_per_L",
  tgw                 = "Thousand_Grain_Weight",
  tiller_number       = "Tiller_Number",
  yield_gp            = "Yield_g_per_Plot",
  yield_kgha          = "Yield_kg_per_Hectare",
  dtm                 = "Days_to_Maturity",
  h_date              = "Heading_Date"
)

pretty_units <- list(
  biomassdry          = "g",
  biomassgreen        = "g",
  grain_number        = "count",
  grain_weight        = "g",
  hardness            = "",
  moisture            = "%",
  plant_ht            = "cm",
  prod_tiller_number  = "count",
  prot_12             = "%",
  prot_ai             = "%",
  prot_db             = "%",
  grain_length        = "mm",
  grain_width         = "mm",
  spike_length        = "cm",
  spikelets_no        = "count",
  test_weight         = "lb/bu",
  test_wt             = "lb/bu",
  twtgl               = "g/L",
  tgw                 = "g",
  tiller_number       = "count",
  yield_gp            = "g/plot",
  yield_kgha          = "kg/ha",
  dtm                 = "days",
  h_date              = "days"
)


## traits you never use / want dropped globally (these names are after cleaning)
traits_to_drop <- c("yield_bua","yield_lbsplot")# legacy; probably not present
non_trait_numeric <- c("plot","bloc","entry","plot_len","plot_wid")

## 5) LOOP OVER LOCATIONS ---------------------------------------

if (!"location" %in% names(raw_data)) {
  stop("Column 'location' not found in dataset. Check your simplified file.")
}

locations <- sort(unique(raw_data$location))
locations <- locations[!is.na(locations)]

cat("\nLocations found:\n")
print(locations)

for (loc in locations) {
  cat("\n=============================\n")
  cat("Processing location:", loc, "\n")
  cat("=============================\n")
  
  ## ------------------------------------------------------------
  ## 5A) SUBSET DATA FOR THIS LOCATION
  ## ------------------------------------------------------------
  data_loc <- raw_data %>% dplyr::filter(location == loc)
  if (nrow(data_loc) == 0) next
  
  ## ensure 'year' exists and is a factor
  if ("year" %in% names(data_loc)) {
    if (!is.factor(data_loc$year)) {
      data_loc$year <- as.factor(as.character(data_loc$year))
    }
  } else {
    data_loc$year <- factor("Unknown")
  }
  
  year_label <- paste(sort(unique(as.character(data_loc$year))), collapse = "_")
  
  ## ------------------------------------------------------------
  ## 5B) CREATE LOCATION-SPECIFIC OUTPUT FOLDERS
  ## ------------------------------------------------------------
  loc_dir <- file.path(out_dir_base, loc)
  dir.create(loc_dir, showWarnings = FALSE, recursive = TRUE)
  
  subdirs <- c(
    "plots/histograms","plots/density","plots/boxplots","plots/violins",
    "plots/qqplots","correlation","summary_stats","missing_data",
    "outliers","group_counts","data_issues","regression"
  )
  for (sd in subdirs) {
    dir.create(file.path(loc_dir, sd), showWarnings = FALSE, recursive = TRUE)
  }
  
  hist_dir<- file.path(loc_dir,"plots/histograms")
  dens_dir<- file.path(loc_dir,"plots/density")
  box_dir<- file.path(loc_dir,"plots/boxplots")
  violin_dir<- file.path(loc_dir,"plots/violins")
  qq_dir<- file.path(loc_dir,"plots/qqplots")
  corr_dir<- file.path(loc_dir,"correlation")
  summary_dir <- file.path(loc_dir,"summary_stats")
  missing_dir <- file.path(loc_dir,"missing_data")
  outlier_dir <- file.path(loc_dir,"outliers")
  group_dir<- file.path(loc_dir,"group_counts")
  issues_dir<- file.path(loc_dir,"data_issues")
  reg_dir<- file.path(loc_dir,"regression")
  
  ## ------------------------------------------------------------
  ## 5C) IDENTIFY NUMERIC TRAITS FOR THIS LOCATION
  ## ------------------------------------------------------------
  ## ------------------------------------------------------------
  ## 5C) IDENTIFY NUMERIC TRAITS FOR THIS LOCATION (FINAL)
  ## ------------------------------------------------------------
  
  # all numeric columns in this location
  numeric_cols_all <- names(data_loc)[sapply(data_loc, is.numeric)]
  numeric_cols_all <- setdiff(
    numeric_cols_all,
    c("year", traits_to_drop, non_trait_numeric)
  )
  
  # keep traits that have at least ONE observed value
  numeric_present <- numeric_cols_all[
    sapply(data_loc[numeric_cols_all], function(x) any(!is.na(x)))
  ]
  
  # location-specific exclusions
  excluded_traits <- if (loc %in% names(traits_excluded_by_location)) {
    traits_excluded_by_location[[loc]]
  } else {
    character(0)
  }
  
  # FINAL trait list used everywhere downstream
  numeric_cols <- setdiff(numeric_present, excluded_traits)
  
  cat("FINAL numeric traits USED at", loc, ":\n")
  print(numeric_cols)
  
  
  ## ============================================================
  ## 7) OUTLIERS (UNIVARIATE, IQR) ------------------------------
  ## ============================================================
  outlier_list <- lapply(numeric_cols, function(col){
    x <- data_loc[[col]][!is.na(data_loc[[col]])]
    if (length(x) < 5) return(NULL)
    q1 <- quantile(x,.25);
    q3 <- quantile(x,.75)
    iqr <- q3 - q1
    tibble::tibble(
      variable = col,
      n= length(x),
      q1= q1,
      median = median(x),
      q3= q3,
      iqr= iqr,
      lower_fence = q1 - 1.5*iqr,
      upper_fence = q3 + 1.5*iqr,
      n_out_low= sum(x < (q1 - 1.5*iqr)),
      n_out_hig = sum(x > (q3 + 1.5*iqr))
    )
  })
  outlier_tbl <- dplyr::bind_rows(outlier_list)
  write.csv(
    outlier_tbl,
    file.path(outlier_dir, paste0("outlier_iqr_summary_", loc, ".csv")),
    row.names = FALSE
  )
  
  ## ============================================================
  ## 7B) MULTIVARIATE OUTLIERS — MAHALANOBIS (PCA-SAFE)
  ## ============================================================
  if (length(numeric_cols) > 1) {
    num_mat<- data_loc[, numeric_cols, drop = FALSE]
    num_complete <- num_mat[complete.cases(num_mat), , drop = FALSE]
    
    if (nrow(num_complete) > ncol(num_complete)) {
      pca<- prcomp(num_complete, scale. = TRUE)
      var_expl <- pca$sdev^2
      keep<- which(var_expl > 1e-8)
      
      if (length(keep) >= 2) {
        scores <- pca$x[, keep, drop = FALSE]
        center <- colMeans(scores)
        covmat <- cov(scores)
        
        md <- mahalanobis(scores, center, covmat)
        cutoff<- qchisq(0.999, df = length(keep))
        
        multivar_outliers <- data.frame(
          Row_Index= as.numeric(rownames(num_complete)),
          Mahalanobis_Distance = md,
          ChiSq_Cutoff_0.999= cutoff,
          Is_Outlier= md > cutoff
        )
        
        write.csv(
          multivar_outliers,
          file.path(outlier_dir, paste0("multivariate_outliers_mahalanobis_", loc, ".csv")),
          row.names = FALSE
        )
        
        cat("Multivariate outlier detection done for", loc, "\n")
      } else {
        cat("Not enough independent PCs for Mahalanobis at", loc, "\n")
      }
    } else {
      cat("Not enough rows for Mahalanobis at", loc, "\n")
    }
  } else {
    cat("Skipping Mahalanobis at", loc, " (<=1 numeric trait).\n")
  }
  
  ## ============================================================
  ## 8) DESCRIPTIVE STATS ---------------------------------------
  ## ============================================================
  desc <- lapply(numeric_cols, function(col){
    x <- data_loc[[col]][!is.na(data_loc[[col]])]
    if (length(x) == 0) return(NULL)
    tibble::tibble(
      variable = col,
      n= length(x),
      mean= mean(x),
      median= median(x),
      sd= sd(x),
      var = var(x),
      cv_perc= ifelse(mean(x)==0, NA, sd(x)/mean(x)*100),
      min= min(x),
      max = max(x),
      skewness = e1071::skewness(x),
      kurtosis = e1071::kurtosis(x)
    )
  })
  desc_tbl <- dplyr::bind_rows(desc)
  write.csv(
    desc_tbl,
    file.path(summary_dir, paste0("descriptive_stats_", loc, ".csv")),
    row.names = FALSE
  )
  
  ## ============================================================
  ## 9) NORMALITY (SHAPIRO) -------------------------------------
  ## ============================================================
  norm_test <- lapply(numeric_cols, function(col){
    x <- data_loc[[col]][!is.na(data_loc[[col]])]
    pval <- if (length(x) >= 3) {
      tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA)
    } else NA
    tibble::tibble(
      variable = col,
      n= length(x),
      shapiro_p = pval,
      normal= ifelse(!is.na(pval) & pval > 0.05, "YES", "NO")
    )
  })
  norm_tbl <- dplyr::bind_rows(norm_test)
  write.csv(
    norm_tbl,
    file.path(summary_dir, paste0("normality_test_", loc, ".csv")),
    row.names = FALSE
  )
  
  ## ============================================================
  ## 9B) VARIANCE HOMOGENEITY — LEVENE (BY BLOC)
  ## ============================================================
  levene_table <- tibble::tibble()
  
  if ("bloc" %in% names(data_loc)) {
    data_loc$bloc <- as.factor(data_loc$bloc)
    
    if (length(unique(data_loc$bloc)) >= 2 && !all(is.na(data_loc$bloc))) {
      
      levene_results <- lapply(numeric_cols, function(tr) {
        x <- data_loc[[tr]]
        if (sum(!is.na(x)) < 5) return(NULL)
        
        out <- tryCatch({
          lv <- car::leveneTest(x ~ data_loc$bloc)
          
          tibble::tibble(
            variable= tr,
            df= lv$Df[2],
            F_value= lv$`F value`[1],
            p_value= lv$`Pr(>F)`[1],
            equal_variance = ifelse(lv$`Pr(>F)`[1] > 0.05, "YES", "NO")
          )
        }, error = function(e) NULL)
        
        return(out)
      })
      
      levene_table <- dplyr::bind_rows(levene_results)
      
      write.csv(
        levene_table,
        file.path(summary_dir, paste0("variance_homogeneity_levene_", loc, ".csv")),
        row.names = FALSE
      )
      
      cat("Levene results saved for", loc, "\n")
      
    } else {
      cat("Levene test skipped at", loc, ": bloc has < 2 levels.\n")
    }
    
  } else {
    cat("Levene test skipped at", loc, ": no 'bloc' column.\n")
  }
  
  ## ============================================================
  ## 9C) LEVENE HEATMAP — BURGUNDY THEME
  ## ============================================================
  if (nrow(levene_table) > 0) {
    
    lev_plot <- levene_table %>%
      dplyr::mutate(equal_num = ifelse(equal_variance == "YES", 1, 0))
    
    lev_plot$variable <- factor(
      lev_plot$variable,
      levels = lev_plot$variable[order(lev_plot$equal_num)]
    )
    
    p_lev <- ggplot2::ggplot(
      lev_plot,
      aes(
        x = "Variance_Homogeneity",
        y = variable,
        fill = as.factor(equal_num)
      )
    ) +
      geom_tile(color = "gray60") +
      geom_text(aes(label = equal_variance),
                size = 3.3, fontface = "bold") +
      scale_fill_manual(
        values = c("0" = "#5a001c", "1" = "#ffb3c6"),
        labels = c("0" = "NO", "1" = "YES"),
        name= "Equal Variance?"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.title= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_text(size = 8),
        panel.grid= element_blank(),
        plot.title= element_text(size = 14, face = "bold"),
        legend.position = "right"
      ) +
      labs(title = paste0("Levene Variance Homogeneity (Bloc Groups) — ", loc))
    
    ggsave(
      file.path(corr_dir, paste0("Levene_VarianceHomogeneity_Heatmap_", loc, ".png")),
      p_lev,
      width = 6, height = 10, dpi = 320
    )
  }
  
  ## ============================================================
  ## 10) LONG FORMAT FOR PLOTS ----------------------------------
  ## ============================================================
  if (length(numeric_cols) == 0) {
    cat("No numeric traits at", loc, "— skipping plots and correlations.\n")
    next
  }
  
  long_data <- data_loc %>%
    tidyr::pivot_longer(all_of(numeric_cols), names_to = "trait", values_to = "value")
  
  ## ============================================================
  ## 11) PLOTS (TITLES PER LOCATION + YEAR)
  ## ============================================================
  pretty_titles <- list(
    h_date= "Heading_Date",
    moisture= "Grain_Moisture",
    yield_kgha = "Yield",
    twtgl= "Test_Weight",
    test_wt= "Test_Weight",
    test_weight = "Test_Weight"
  )
  
  for(tr in unique(long_data$trait)) {
    
    sub <- long_data %>% dplyr::filter(trait == tr, !is.na(value))
    if (nrow(sub) == 0) next
    
    pretty_raw <- pretty_names[[tr]] %||% tr
    pretty<- pretty_titles[[tr]] %||% pretty_raw
    unit<- pretty_units[[tr]] %||% ""
    ylab<- if(unit == "") pretty else paste0(pretty, " (", unit, ")")
    
    plot_title <- paste0(pretty, " — ", loc, " ", year_label)
    
    ## HISTOGRAM
    ggsave(
      file.path(hist_dir, paste0(tr, "_", loc, ".png")),
      ggplot(sub, aes(value, fill = year)) +
        geom_histogram(color = "black", bins = 30) +
        facet_wrap(~year, scales = "free_y") +
        theme_classic() +
        labs(title = plot_title, x = pretty, y = "Count"),
      width = 7, height = 5
    )
    
    ## DENSITY
    ggsave(
      file.path(dens_dir, paste0(tr, "_", loc, ".png")),
      ggplot(sub, aes(value, color = year)) +
        geom_density(alpha = 0.3) +
        theme_classic() +
        labs(title = plot_title, x = pretty, y = "Density"),
      width = 7, height = 5
    )
    
    ## BOX PLOT
    ggsave(
      file.path(box_dir, paste0(tr, "_", loc, ".png")),
      ggplot(sub, aes(year, value, fill = year)) +
        geom_boxplot() +
        theme_classic() +
        labs(title = plot_title, x = "Year", y = ylab),
      width = 6, height = 5
    )
    
    ## VIOLIN PLOT
    ggsave(
      file.path(violin_dir, paste0(tr, "_", loc, ".png")),
      ggplot(sub, aes(year, value, fill = year)) +
        geom_violin(trim = FALSE) +
        geom_boxplot(width = 0.12, outlier.shape = NA) +
        theme_classic() +
        labs(title = plot_title, x = "Year", y = ylab),
      width = 6, height = 5
    )
    
    ## QQ PLOT
    ggsave(
      file.path(qq_dir, paste0(tr, "_", loc, ".png")),
      ggplot(sub, aes(sample = value)) +
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~year) +
        theme_classic() +
        labs(
          title = plot_title,
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ),
      width = 7, height = 5
    )
  }
  
  ## ============================================================
  ## 12) CORRELATION HEATMAP (LOWER TRIANGLE, MAGMA)
  ## ============================================================
  drop_from_heatmap <- c("prot_12","prot_db","test_weight","test_wt")
  heatmap_cols <- setdiff(numeric_cols, drop_from_heatmap)
  
  if (length(heatmap_cols) >= 2) {
    heatmap_data <- data_loc[, heatmap_cols, drop = FALSE]
    
    cm <- Hmisc::rcorr(as.matrix(heatmap_data))
    r<- cm$r
    p<- cm$P
    traits <- colnames(r)
    
    df_cor <- reshape2::melt(r)
    names(df_cor) <- c("Var1","Var2","corr")
    df_cor$pval <- reshape2::melt(p)$value
    
    df_cor <- df_cor %>%
      dplyr::mutate(
        i = match(Var1, traits),
        j = match(Var2, traits),
        keep = i >= j
      ) %>%
      dplyr::filter(keep)
    
    df_cor$annot <- sprintf(
      "%.2f%s",
      df_cor$corr,
      ifelse(df_cor$pval < 0.001,"***",
             ifelse(df_cor$pval < 0.01,"**",
                    ifelse(df_cor$pval < 0.05,"*","")))
    )
    
    heatmap_rename <- list(
      "Yield_g_per_Plot"= "Yield_g_Plot",
      "Test_Weight_g_per_L"= "Test_Weight",
      "Protein_Concentration_AI"= "Protein_Concentration"
    )
    
    pretty_axis_heatmap <- function(vec) {
      sapply(vec, function(v) {
        new <- if (v %in% names(pretty_names)) pretty_names[[v]] else v
        if (new %in% names(heatmap_rename)) {
          new <- heatmap_rename[[new]]
        }
        new
      })
    }
    
    df_cor$Var1 <- pretty_axis_heatmap(as.character(df_cor$Var1))
    df_cor$Var2 <- pretty_axis_heatmap(as.character(df_cor$Var2))
    
    df_cor$Var1 <- factor(df_cor$Var1, levels = unique(df_cor$Var1))
    df_cor$Var2 <- factor(df_cor$Var2, levels = unique(df_cor$Var2))
    
    p_heat <- ggplot(df_cor, aes(Var2, Var1, fill = corr)) +
      geom_tile(color="gray70", linewidth=0.4) +
      geom_text(aes(label = annot), size = 2.8) +
      scale_fill_gradientn(
        colours = viridis::magma(200),
        limits= c(-1, 1),
        name = "Correlation"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 55, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        plot.title= element_text(size = 16, face="bold", hjust = 0.5),
        panel.grid= element_blank(),
        axis.title.x = element_text(size=10, face="bold"),
        axis.title.y = element_blank(),
        plot.margin = margin(5,5,5,5)
      ) +
      labs(
        title = paste0("Correlation Heatmap_", loc, "_", year_label),
        x= "Traits"
      )
    
    ggsave(
      file.path(corr_dir, paste0("CorrelationHeatmap_", loc, "_", year_label, "_magma.png")),
      p_heat,
      width = 12, height = 10, dpi = 320
    )
  } else {
    cat("Skipping correlation heatmap at", loc, " (too few traits).\n")
  }
  ## ============================================================
  ## 12C) PAIRWISE REGRESSIONS — SAFE VERSION
  ## ============================================================
  reg_results <- list()
  num_traits<- numeric_cols
  
  for (i in seq_along(num_traits)) {
    for (j in seq_along(num_traits)) {
      
      trait_x <- num_traits[i]
      trait_y <- num_traits[j]
      if (trait_x == trait_y) next
      
      df_sub <- data_loc[, c(trait_x, trait_y)]
      df_sub <- df_sub[complete.cases(df_sub), ]
      
      # skip if not enough observations
      if (nrow(df_sub) < 5) next
      
      # Try fitting model safely
      fit <- tryCatch(lm(df_sub[[trait_y]] ~ df_sub[[trait_x]]), error = function(e) NULL)
      if (is.null(fit)) next
      
      summ <- summary(fit)
      
      # skip if regression didn't produce 2 coefficients
      if (length(coef(fit)) < 2) next
      
      intercept <- coef(fit)[1]
      slope<- coef(fit)[2]
      
      # handle NA slope / degenerate model
      if (is.na(slope)) next
      
      p_value <- tryCatch(summ$coefficients[2,4], error = function(e) NA)
      r2 <- summ$r.squared %||% NA
      adj_r2<- summ$adj.r.squared %||% NA
      rmse <- sqrt(mean(summ$residuals^2)) %||% NA
      
      sh_p <- tryCatch(
        shapiro.test(summ$residuals)$p.value,
        error = function(e) NA
      )
      
      # add row safely
      reg_results[[length(reg_results) + 1]] <- data.frame(
        trait_x = trait_x,
        trait_y = trait_y,
        intercept = intercept,
        slope= slope,
        r2= r2,
        adj_r2= adj_r2,
        rmse= rmse,
        p_value= p_value,
        residual_normality_p = sh_p
      )
    }
  }
  
  if (length(reg_results) > 0) {
    reg_table <- dplyr::bind_rows(reg_results)
    write.csv(
      reg_table,
      file.path(reg_dir, paste0("pairwise_regressions_", loc, ".csv")),
      row.names = FALSE
    )
  }
  
  
  ## ============================================================
  ## 13) DATA ISSUES --------------------------------------------
  ## ============================================================
  issues <- lapply(numeric_cols, function(col){
    x <- data_loc[[col]]
    tibble::tibble(
      variable= col,
      n= sum(!is.na(x)),
      na_percent= mean(is.na(x))*100,
      constant = length(unique(x[!is.na(x)])) <= 1,
      negative= sum(x < 0, na.rm = TRUE),
      zero= sum(x == 0, na.rm = TRUE)
    )
  })
  issues_tbl <- dplyr::bind_rows(issues)
  write.csv(
    issues_tbl,
    file.path(issues_dir, paste0("data_issues_", loc, ".csv")),
    row.names = FALSE
  )
  
  ## ============================================================
  ## 14) GROUP COUNTS (BLOC, ENTRY, YEAR) -----------------------
  ## ============================================================
  for(fc in factor_cols){
    if (!fc %in% names(data_loc)) next
    tab <- data_loc %>% dplyr::count(.data[[fc]])
    write.csv(
      tab,
      file.path(group_dir, paste0("group_counts_", fc, "_", loc, ".csv")),
      row.names = FALSE
    )
  }
  
} # end location loop

## ============================================================
## SAVE GLOBAL CLEAN DATA (ALL LOCATIONS TOGETHER) -------------
## ============================================================
write.csv(raw_data, file.path(out_dir_base, "raw_clean_global.csv"), row.names = FALSE)
saveRDS(raw_data, file.path(out_dir_base, "raw_clean_global.rds"))

cat("\n===== PRE-ANALYSIS COMPLETED SUCCESSFULLY FOR ALL LOCATIONS =====\n")

sapply(raw_data, class)


sapply(raw_data[, c("bloc", "location", "recurrent_parent", "donor_parent")], class)
