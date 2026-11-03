suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(tibble))

cache_dir <- rappdirs::user_cache_dir("textdata")
lm_folder <- file.path(cache_dir, "loughran")
lm_rds    <- file.path(lm_folder, "LoughranMcDonald.rds")
lm_csv    <- file.path(lm_folder,
  "LoughranMcDonald_MasterDictionary_2018 - LoughranMcDonald_MasterDictionary_2018.csv")

dir.create(lm_folder, recursive = TRUE, showWarnings = FALSE)
cat("Cache folder:", lm_folder, "\n")

if (!file.exists(lm_csv)) {
  cat("Baixando LM do Google Drive...\n")
  tryCatch(
    download.file(
      "https://drive.google.com/uc?id=12ECPJMxV2wSalXG8ykMmkpa1fq_ur0Rf&export=download",
      lm_csv, mode = "wb", quiet = FALSE
    ),
    error = function(e) cat("ERRO download:", conditionMessage(e), "\n")
  )
  if (file.exists(lm_csv))
    cat("Download OK. Tamanho:", file.info(lm_csv)$size, "bytes\n")
}

if (file.exists(lm_csv) && !file.exists(lm_rds)) {
  cat("Processando CSV -> RDS...\n")
  data <- read_csv(lm_csv, col_types = cols_only(
    Word = col_character(), Negative = col_double(), Positive = col_double(),
    Uncertainty = col_double(), Litigious = col_double(),
    Constraining = col_double(), Superfluous = col_double()
  ), show_col_types = FALSE)
  cat("CSV lido:", nrow(data), "palavras\n")
  types <- c("Negative", "Positive", "Uncertainty",
             "Litigious", "Constraining", "Superfluous")
  out <- list()
  for (t in types)
    out[[t]] <- tibble(word = tolower(data$Word[data[[t]] != 0]),
                       sentiment = tolower(t))
  lm_tb <- Reduce(rbind, out)
  saveRDS(lm_tb, lm_rds)
  cat("RDS salvo:", nrow(lm_tb), "entradas, categorias:",
      paste(unique(lm_tb$sentiment), collapse = ", "), "\n")
} else if (file.exists(lm_rds)) {
  lm_tb <- readRDS(lm_rds)
  cat("Ja em cache:", nrow(lm_tb), "entradas\n")
} else {
  cat("ERRO: arquivo CSV nao encontrado apos download\n")
}
