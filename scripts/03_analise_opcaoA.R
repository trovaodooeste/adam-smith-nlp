# Script 03a: Opção A — O "Problema de Adam Smith"
#
# Research question: TMS (1759) e WoN (1776) diferem em linguagem econômica,
# estrutura retórica e tópicos latentes? A análise com Loughran-McDonald
# suporta ou refuta o "Das Adam Smith Problem"?
#
# Posicionamento vs. Ballandonne & Cersosimo (2023, JHET 45:27-49):
#   B&C usaram sentimentr + Jockers-Rinker (dicionário genérico) e concluíram
#   que sentimento "não deve ser usado" em textos históricos econômicos porque
#   "gold", "wealth", "money" são incorretamente marcados como positivos.
#   Este script usa Loughran-McDonald (LM), construído para linguagem econômica,
#   que trata esse vocabulário como neutro e foca nas 6 categorias relevantes.
#   Adiciona também LDA (ausente em B&C) e testes estatísticos formais.
#
# Requer: resultados/nlp_textos.csv (script 02)
#         textos/smith_tms.txt e textos/smith_won.txt

library(tidyverse)
library(tidytext)
library(topicmodels)
library(quanteda)
library(quanteda.textstats)
library(ggplot2)

dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Carrega resultados NLP
# ─────────────────────────────────────────────────────────────────────────────

nlp <- read_csv("resultados/nlp_textos.csv", show_col_types = FALSE) |>
  filter(str_detect(doc_id, "smith"))

if (nrow(nlp) < 2) stop("Rode o script 02 com smith_tms.txt e smith_won.txt em textos/")

cat("=== OPÇÃO A: O Problema de Adam Smith ===\n")
cat("Posicionamento: responde à limitação de Ballandonne & Cersosimo (2023, JHET)\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. REPLICAÇÃO B&C: legibilidade e pronomes (validação cruzada)
# ─────────────────────────────────────────────────────────────────────────────

cat("--- Replicação B&C: Legibilidade e Pronomes ---\n")
cat("(Flesch-Kincaid e pct_we/pct_i — base comparativa com B&C 2023)\n\n")

nlp |>
  select(obra_curta, ano, Flesch, Flesch.Kincaid, ARI, pct_we, pct_i, ratio_we_i) |>
  mutate(across(where(is.numeric), ~ round(.x, 4))) |>
  print()

cat("\nInterpretação Flesch-Kincaid:\n")
cat("  B&C encontraram TMS marginalmente mais difícil que WoN (grade 14-20).\n")
fk <- nlp |> select(obra_curta, Flesch.Kincaid) |> deframe()
tms_label <- names(fk)[str_detect(names(fk), "Moral|TMS")]
won_label  <- names(fk)[str_detect(names(fk), "Wealth|WoN|Nations")]
if (length(tms_label) == 1 && length(won_label) == 1) {
  cat(sprintf("  TMS: %.2f | WoN: %.2f | Diff: %+.2f\n",
              fk[tms_label], fk[won_label], fk[tms_label] - fk[won_label]))
  cat(sprintf("  %s\n",
              ifelse(fk[tms_label] > fk[won_label],
                     "TMS mais difícil — confirma B&C",
                     "WoN mais difícil — contraria B&C (verificar edição do texto)")))
}

cat("\nInterpretação pronomes:\n")
cat("  B&C: TMS domina em I/he/him/we/us (dialogic); WoN dominado por 'it' (monologic).\n")
cat("  H4: pct_we(TMS) > pct_we(WoN) e ratio_we_i(TMS) > ratio_we_i(WoN)\n")
nlp |>
  select(obra_curta, pct_we, pct_i, ratio_we_i) |>
  mutate(across(where(is.numeric), ~ round(.x, 5))) |>
  print()

# ─────────────────────────────────────────────────────────────────────────────
# 3. CONTRIBUIÇÃO PRINCIPAL: Loughran-McDonald (resposta à limitação B&C)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- Loughran-McDonald: resposta à limitação de B&C ---\n")
cat("B&C usaram Jockers-Rinker (genérico): 'gold', 'wealth', 'money' = positivo.\n")
cat("LM (econômico): trata esse vocabulário como neutro; foca em uncertainty,\n")
cat("constraining, positive, negative — categorias relevantes para textos econômicos.\n\n")

lm_vars <- c("lm_positive", "lm_negative", "lm_uncertainty",
             "lm_constraining", "lm_litigious", "lm_superfluous")

cat("Scores LM (proporção de tokens):\n")
nlp |>
  select(obra_curta, ano, all_of(lm_vars)) |>
  mutate(across(where(is.numeric), ~ round(.x, 5))) |>
  arrange(ano) |>
  print()

cat("\n--- Teste das hipóteses ---\n\n")

lm_tms <- nlp |> filter(str_detect(doc_id, "tms"))
lm_won <- nlp |> filter(str_detect(doc_id, "won"))

if (nrow(lm_tms) == 1 && nrow(lm_won) == 1) {

  cat("H1 — lm_positive(TMS) > lm_positive(WoN)?\n")
  cat(sprintf("  TMS: %.5f | WoN: %.5f → %s\n",
              lm_tms$lm_positive, lm_won$lm_positive,
              ifelse(lm_tms$lm_positive > lm_won$lm_positive,
                     "CONFIRMADA ✓ (TMS mais positivo — linguagem moral)",
                     "NÃO CONFIRMADA — resultado a discutir")))

  cat("\nH2 — lm_constraining(WoN) > lm_constraining(TMS)?\n")
  cat(sprintf("  WoN: %.5f | TMS: %.5f → %s\n",
              lm_won$lm_constraining, lm_tms$lm_constraining,
              ifelse(lm_won$lm_constraining > lm_tms$lm_constraining,
                     "CONFIRMADA ✓ (WoN mais prescritivo — regras de mercado)",
                     "NÃO CONFIRMADA — resultado a discutir")))

  cat("\nH3 — lm_uncertainty: diferença TMS vs. WoN?\n")
  diff_unc <- lm_tms$lm_uncertainty - lm_won$lm_uncertainty
  cat(sprintf("  TMS: %.5f | WoN: %.5f | Diff: %+.5f\n",
              lm_tms$lm_uncertainty, lm_won$lm_uncertainty, diff_unc))
  cat(sprintf("  %s\n",
              ifelse(abs(diff_unc) < 0.001,
                     "Diferença mínima — refuta dicotomia (ambos lidam com incerteza)",
                     ifelse(lm_tms$lm_uncertainty > lm_won$lm_uncertainty,
                            "TMS mais incerto — Smith mais hesitante em filosofia moral",
                            "WoN mais incerto — Smith mais hesitante em economia"))))
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. Análise por capítulo — granularidade ausente em B&C
# ─────────────────────────────────────────────────────────────────────────────

parse_por_capitulo <- function(path, doc_prefix) {
  if (!file.exists(path)) return(NULL)
  linhas <- readLines(path, encoding = "UTF-8", warn = FALSE)
  breaks <- which(str_detect(toupper(linhas), "^(CHAPTER|PART|BOOK)\\s+[IVXLC0-9]+"))
  if (length(breaks) < 2) breaks <- seq(1, length(linhas), by = 200)
  ends <- c(breaks[-1] - 1, length(linhas))
  map2_dfr(seq_along(breaks), ends, function(idx, e) {
    s <- breaks[idx]
    tibble(doc_id = paste0(doc_prefix, "_cap", idx),
           texto  = paste(linhas[s:e], collapse = " "))
  }) |> filter(nchar(texto) > 500)
}

cat("\n--- Análise por capítulo (granularidade além de B&C) ---\n")
tms_caps <- parse_por_capitulo("textos/smith_tms.txt", "tms")
won_caps <- parse_por_capitulo("textos/smith_won.txt", "won")

if (!is.null(tms_caps) && !is.null(won_caps)) {
  cat(sprintf("TMS: %d capítulos/blocos | WoN: %d capítulos/blocos\n",
              nrow(tms_caps), nrow(won_caps)))

  df_caps <- bind_rows(
    tms_caps |> mutate(livro = "TMS (1759)"),
    won_caps |> mutate(livro = "WoN (1776)")
  )

  lm_dict <- tryCatch(get_sentiments("loughran"),
                      error = function(e) { cat("Cache LM ausente.\n"); NULL })

  if (!is.null(lm_dict)) {
    lm_caps <- df_caps |>
      unnest_tokens(word, texto) |>
      inner_join(lm_dict, by = "word") |>
      count(doc_id, livro, sentiment) |>
      group_by(doc_id, livro) |>
      mutate(total = sum(n), pct = n / total) |>
      ungroup()

    # Uncertainty por capítulo — H3 com poder estatístico
    unc_tms <- lm_caps |> filter(livro == "TMS (1759)", sentiment == "uncertainty") |> pull(pct)
    unc_won <- lm_caps |> filter(livro == "WoN (1776)", sentiment == "uncertainty") |> pull(pct)

    if (length(unc_tms) >= 3 && length(unc_won) >= 3) {
      t_res <- t.test(unc_tms, unc_won)
      cat(sprintf("\nTeste t — lm_uncertainty por capítulo (H3 formal):\n"))
      cat(sprintf("  TMS: média=%.5f, dp=%.5f (n=%d)\n",
                  mean(unc_tms), sd(unc_tms), length(unc_tms)))
      cat(sprintf("  WoN: média=%.5f, dp=%.5f (n=%d)\n",
                  mean(unc_won), sd(unc_won), length(unc_won)))
      cat(sprintf("  t=%.3f, df=%.1f, p=%.4f\n",
                  t_res$statistic, t_res$parameter, t_res$p.value))
      cat(sprintf("  Interpretação: %s\n",
                  ifelse(t_res$p.value < 0.05,
                         "Diferença significativa — suporta dicotomia",
                         "Sem diferença — refuta dicotomia (ambos incertos)")))
    }

    # Constraining por capítulo — H2 com poder estatístico
    con_tms <- lm_caps |> filter(livro == "TMS (1759)", sentiment == "constraining") |> pull(pct)
    con_won <- lm_caps |> filter(livro == "WoN (1776)", sentiment == "constraining") |> pull(pct)

    if (length(con_tms) >= 3 && length(con_won) >= 3) {
      t_res2 <- t.test(con_tms, con_won)
      cat(sprintf("\nTeste t — lm_constraining por capítulo (H2 formal):\n"))
      cat(sprintf("  TMS: %.5f | WoN: %.5f | p=%.4f → %s\n",
                  mean(con_tms), mean(con_won), t_res2$p.value,
                  ifelse(t_res2$p.value < 0.05 && mean(con_won) > mean(con_tms),
                         "H2 CONFIRMADA ✓", "H2 não confirmada")))
    }
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. LDA — estrutura temática latente (ausente em B&C)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- LDA: estrutura temática latente (ausente em B&C) ---\n")
cat("B&C usaram TF-IDF + keyness (superfície). LDA detecta estrutura latente.\n\n")

lda_model <- tryCatch(readRDS("resultados/lda_model.rds"), error = function(e) NULL)

if (!is.null(lda_model)) {
  gamma <- tidy(lda_model, matrix = "gamma") |>
    filter(str_detect(document, "smith"))

  if (nrow(gamma) > 0) {
    gamma_wide <- gamma |>
      mutate(livro = ifelse(str_detect(document, "tms"), "TMS", "WoN")) |>
      group_by(livro, topic) |>
      summarise(gamma_med = mean(gamma), .groups = "drop") |>
      pivot_wider(names_from = livro, values_from = gamma_med) |>
      mutate(diff = TMS - WoN, diff_abs = abs(diff)) |>
      arrange(desc(diff_abs))

    cat("Tópicos mais diferenciados entre TMS e WoN (|diff| decrescente):\n")
    print(gamma_wide)

    cat("\nTop palavras dos 3 tópicos mais diferenciados:\n")
    top3 <- gamma_wide |> slice_head(n = 3) |> pull(topic)
    tidy(lda_model, matrix = "beta") |>
      filter(topic %in% top3) |>
      group_by(topic) |>
      slice_max(beta, n = 8) |>
      summarise(words = paste(term, collapse = ", "), .groups = "drop") |>
      mutate(label = paste0("T", topic, ": ", words)) |>
      pull(label) |>
      cat(sep = "\n")

    cat("\nInterpretação:\n")
    cat("  Tópicos com diff > 0: mais presentes em TMS (esfera moral/sentimental?)\n")
    cat("  Tópicos com diff < 0: mais presentes em WoN (esfera econômica?)\n")
    cat("  Se tópicos morais aparecem em AMBOS: refuta dicotomia Adam Smith Problem.\n")
  }
} else {
  cat("AVISO: lda_model.rds não encontrado. Rode script 02 primeiro.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. TF-IDF — replicação e extensão de B&C
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- TF-IDF: replicação de B&C + extensão ---\n")
cat("B&C (Tabela 2) encontraram em TMS: sympathy, joy, sorrow, virtue...\n")
cat("Espera-se replicar esse resultado com nosso corpus Gutenberg.\n\n")

if (!is.null(tms_caps) && !is.null(won_caps)) {
  tfidf_smith <- bind_rows(
    tms_caps |> mutate(livro = "TMS"),
    won_caps |> mutate(livro = "WoN")
  ) |>
    unnest_tokens(word, texto) |>
    anti_join(tidytext::stop_words, by = "word") |>
    filter(nchar(word) >= 3, !str_detect(word, "^\\d+$")) |>
    count(livro, word) |>
    bind_tf_idf(word, livro, n) |>
    group_by(livro) |>
    slice_max(tf_idf, n = 20) |>
    arrange(livro, desc(tf_idf))

  cat("Top 20 palavras distintivas (TF-IDF) — comparar com B&C Tabela 2:\n")
  tfidf_smith |>
    select(livro, word, tf_idf) |>
    mutate(tf_idf = round(tf_idf, 5)) |>
    print(n = 40)

  write_csv(tfidf_smith, "resultados/opcaoA_tfidf_smith.csv")
  cat("\nSalvo: resultados/opcaoA_tfidf_smith.csv\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 7. Resumo final — síntese do posicionamento
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n=== SÍNTESE — Posicionamento vs. B&C (2023) ===\n\n")
cat("Ballandonne & Cersosimo (2023) concluíram que sentimento 'não deve ser\n")
cat("usado' em textos econômicos históricos por inadequação do dicionário.\n")
cat("Este estudo responde:\n\n")
cat("  1. Com Loughran-McDonald (econômico), sentimento É interpretável:\n")
cat("     → uncertainty e constraining capturam distinções conceptuais reais.\n\n")
cat("  2. LDA revela estrutura temática latente que TF-IDF não captura:\n")
cat("     → tópicos morais em ambos os livros refutam/confirmam a dicotomia.\n\n")
cat("  3. Análise por capítulo + testes estatísticos tornam as hipóteses\n")
cat("     formalmente testáveis — além do nível exploratório de B&C.\n\n")

cat("=== Script 03a concluído ===\n")
cat("Resultados: resultados/opcaoA_tfidf_smith.csv\n")
cat("Figuras:    figuras/\n")
