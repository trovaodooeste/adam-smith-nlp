# Script 02: Pipeline NLP para textos clássicos de economia
#
# Adaptado do script 08_nlp_variaveis.R do projeto artigo-wgmgap.
# Diferenças principais:
#   - Input: arquivos .txt de textos clássicos (por livro ou por capítulo)
#   - Unidade de análise: livro OU capítulo/seção (configurável abaixo)
#   - Sem variáveis de crowdfunding (equity, target, teamnum, setores)
#   - LDA com K configurável (sugestão: K=5 para ~5 textos, K=10 para corpus maior)
#
# Output: resultados/nlp_textos.csv

pkgs <- c("tidyverse", "tidytext", "textdata", "topicmodels",
          "quanteda", "quanteda.textstats", "slam", "stringr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(tidyverse)
library(tidytext)
library(textdata)
library(topicmodels)
library(quanteda)
library(quanteda.textstats)
library(slam)

set.seed(42)

dir.create("resultados", showWarnings = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURAÇÃO — ajustar conforme a opção de artigo escolhida
# ─────────────────────────────────────────────────────────────────────────────

# UNIDADE DE ANÁLISE:
# "livro"    = uma linha por livro (compara livros inteiros)
# "capitulo" = divide cada texto em capítulos (requer parse_capitulos = TRUE)
UNIT <- "livro"

# K de tópicos para LDA
K_LDA <- 8

# ─────────────────────────────────────────────────────────────────────────────
# 1. Carrega textos
# ─────────────────────────────────────────────────────────────────────────────

txt_files <- list.files("textos", pattern = "\\.txt$", full.names = TRUE)
cat("Arquivos encontrados:", length(txt_files), "\n")
cat(paste(" ", basename(txt_files)), sep = "\n")

if (length(txt_files) == 0) {
  stop("Nenhum arquivo .txt em textos/. Execute script 01 primeiro.")
}

df_raw <- map_dfr(txt_files, function(f) {
  linhas <- readLines(f, encoding = "UTF-8", warn = FALSE)
  tibble(
    doc_id = tools::file_path_sans_ext(basename(f)),
    texto  = paste(linhas, collapse = " ")
  )
})

cat("\nTextos carregados:\n")
df_raw |>
  mutate(nchars = nchar(texto), nwords = str_count(texto, "\\S+")) |>
  select(doc_id, nchars, nwords) |>
  print()

# Metadados: autor, obra, ano — AJUSTAR conforme os arquivos carregados
metadata <- tribble(
  ~doc_id,                      ~autor,    ~obra_curta,              ~ano,
  "smith_tms",                  "Smith",   "Moral Sentiments",       1759,
  "smith_won",                  "Smith",   "Wealth of Nations",      1776,
  "keynes_general_theory",      "Keynes",  "Econ. Consequences",     1919,
  "keynes_treatise_prob",       "Keynes",  "Treatise on Prob.",      1921,
  "knight_risk",                "Knight",  "Risk & Uncertainty",     1921,
  "ricardo_principles",         "Ricardo", "Principles",             1817,
  "mill_principles",            "Mill",    "Principles Pol. Econ",   1848
  # adicionar outros conforme necessário
)

df <- df_raw |>
  left_join(metadata, by = "doc_id") |>
  mutate(
    autor     = coalesce(autor, doc_id),
    obra_curta = coalesce(obra_curta, doc_id),
    ano       = coalesce(ano, NA_real_)
  )

cat("\nDataset final:", nrow(df), "documentos\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. Tokenização
# ─────────────────────────────────────────────────────────────────────────────

tokens_raw <- df |>
  unnest_tokens(word, texto)

data("stop_words")
tokens_clean <- tokens_raw |>
  anti_join(stop_words, by = "word") |>
  filter(!str_detect(word, "^\\d+$"), nchar(word) >= 3)

cat("\nTokens (limpos):", nrow(tokens_clean), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3. Variáveis estruturais
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- Variáveis estruturais ---\n")

pitch_length <- df |>
  mutate(
    nchars = nchar(texto),
    nwords = str_count(texto, "\\S+")
  ) |>
  select(doc_id, nchars, nwords)

# Primeira pessoa (coletiva vs. individual)
# pct_we: relevante para testar "voz autoral" vs. "voz de escola/comunidade"
pronoun_counts <- tokens_raw |>
  mutate(
    first_pl = word %in% c("we", "our", "us", "ourselves"),
    first_sg = word %in% c("i", "my", "me", "myself", "mine")
  ) |>
  group_by(doc_id) |>
  summarise(
    n_words_raw = n(),
    n_first_pl  = sum(first_pl),
    n_first_sg  = sum(first_sg),
    .groups = "drop"
  ) |>
  mutate(
    pct_we     = n_first_pl / pmax(n_words_raw, 1),
    pct_i      = n_first_sg / pmax(n_words_raw, 1),
    ratio_we_i = (n_first_pl + 0.5) / (n_first_sg + 0.5)
  )

# Legibilidade — compara complexidade sintática entre textos/épocas
corp <- corpus(df, docid_field = "doc_id", text_field = "texto")
readability_scores <- textstat_readability(
  corp,
  measure = c("Flesch", "Flesch.Kincaid", "ARI", "Coleman.Liau.grade")
) |>
  rename(doc_id = document) |>
  select(doc_id, Flesch, Flesch.Kincaid, ARI, Coleman.Liau.grade)

cat("Legibilidade calculada para", nrow(readability_scores), "textos\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4. Loughran-McDonald — sentimento financeiro
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- Loughran-McDonald ---\n")
cat("(Execute scripts/precache_lm.R uma vez se este passo falhar)\n\n")

lm_dict <- tryCatch(
  get_sentiments("loughran"),
  error = function(e) {
    cat("Cache LM ausente — rode precache_lm.R primeiro.\n")
    stop(e)
  }
)

cat("Palavras LM:", nrow(lm_dict), "| Categorias:",
    paste(unique(lm_dict$sentiment), collapse = ", "), "\n")

lm_scores <- tokens_clean |>
  inner_join(lm_dict, by = "word") |>
  count(doc_id, sentiment) |>
  left_join(tokens_clean |> count(doc_id, name = "n_tokens"), by = "doc_id") |>
  mutate(score = n / n_tokens) |>
  select(doc_id, sentiment, score) |>
  pivot_wider(
    names_from  = sentiment,
    values_from = score,
    values_fill = 0,
    names_prefix = "lm_"
  )

lm_cats <- paste0("lm_", c("positive", "negative", "uncertainty",
                             "litigious", "constraining", "superfluous"))
for (col in lm_cats) {
  if (!col %in% names(lm_scores)) lm_scores[[col]] <- 0
}

lm_scores <- df |>
  select(doc_id) |>
  left_join(lm_scores, by = "doc_id") |>
  mutate(across(starts_with("lm_"), ~ replace_na(.x, 0)))

cat("\nScores LM por texto:\n")
lm_scores |>
  left_join(metadata |> select(doc_id, autor, obra_curta, ano), by = "doc_id") |>
  arrange(ano) |>
  select(autor, obra_curta, ano, lm_positive, lm_negative, lm_uncertainty,
         lm_constraining) |>
  print(n = 20)

# ─────────────────────────────────────────────────────────────────────────────
# 5. LDA — Tópicos latentes
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- LDA (K =", K_LDA, ") ---\n")

dtm <- tokens_clean |>
  count(doc_id, word) |>
  cast_dtm(doc_id, word, n)

dtm <- dtm[slam::row_sums(dtm) > 0, ]
cat("DTM:", nrow(dtm), "docs x", ncol(dtm), "termos\n")

lda_model <- LDA(dtm, k = K_LDA, method = "Gibbs",
                 control = list(seed = 42, iter = 2000, burnin = 200,
                                thin = 10, verbose = 0))
saveRDS(lda_model, "resultados/lda_model.rds")

# Top palavras por tópico
cat("\nInterpretação dos tópicos (top 8 palavras):\n")
tidy(lda_model, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 8) |>
  summarise(words = paste(term, collapse = ", "), .groups = "drop") |>
  mutate(label = paste0("T", topic, ": ", words)) |>
  pull(label) |>
  cat(sep = "\n")

lda_gamma <- tidy(lda_model, matrix = "gamma") |>
  rename(doc_id = document) |>
  pivot_wider(names_from = topic, values_from = gamma, names_prefix = "lda_")

# ─────────────────────────────────────────────────────────────────────────────
# 6. Consolida e salva
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- Consolidando ---\n")

nlp_features <- df |>
  select(doc_id, autor, obra_curta, ano) |>
  left_join(pitch_length,    by = "doc_id") |>
  left_join(pronoun_counts |>
              select(doc_id, pct_we, pct_i, ratio_we_i, n_words_raw),
            by = "doc_id") |>
  left_join(readability_scores, by = "doc_id") |>
  left_join(lm_scores,          by = "doc_id") |>
  left_join(lda_gamma,          by = "doc_id")

cat("Dimensão final:", nrow(nlp_features), "x", ncol(nlp_features), "\n")

write_csv(nlp_features, "resultados/nlp_textos.csv")
cat("Salvo: resultados/nlp_textos.csv\n")

# Resumo comparativo
cat("\n=== Resumo comparativo ===\n")
nlp_features |>
  arrange(ano) |>
  select(autor, obra_curta, ano, Flesch, pct_we, pct_i,
         lm_positive, lm_negative, lm_uncertainty) |>
  mutate(across(where(is.numeric), ~ round(.x, 4))) |>
  print(n = 30)

cat("\n=== Script 02 concluído ===\n")
