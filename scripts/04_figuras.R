# Script 04: Figuras para o artigo — Opção A (Adam Smith Problem)
#
# Gera 6 figuras academicamente formatadas (PNG 300 dpi + PDF):
#   Fig 1 — Perfil LM: 6 categorias TMS vs. WoN
#   Fig 2 — Pronomes: pct_we, pct_i, ratio_we_i
#   Fig 3 — Uncertainty por capítulo (boxplot + t-test)
#   Fig 4 — LDA: proporção de tópicos em TMS vs. WoN
#   Fig 5 — TF-IDF: palavras mais distintivas (lollipop)
#   Fig 6 — Legibilidade: Flesch, Flesch-Kincaid, ARI
#
# Requer: resultados/nlp_textos.csv, resultados/lda_model.rds,
#         textos/smith_tms.txt, textos/smith_won.txt

library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(scales)

dir.create("figuras", showWarnings = FALSE)

# ─── Paleta e tema ───────────────────────────────────────────────────────────

COL_TMS <- "#1a6eb5"   # azul — TMS
COL_WON <- "#c0392b"   # vermelho — WoN
COL_BOTH <- c("TMS (1759)" = COL_TMS, "WoN (1776)" = COL_WON)

tema_artigo <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 10, color = "gray40"),
    axis.title        = element_text(size = 11),
    strip.background  = element_rect(fill = "gray92", color = NA),
    strip.text        = element_text(face = "bold")
  )

salvar <- function(nome, largura = 7, altura = 4.5) {
  ggsave(paste0("figuras/", nome, ".png"),
         width = largura, height = altura, dpi = 300, bg = "white")
  ggsave(paste0("figuras/", nome, ".pdf"),
         width = largura, height = altura)
  cat("  Salvo: figuras/", nome, ".png + .pdf\n", sep = "")
}

# ─────────────────────────────────────────────────────────────────────────────
# Carrega dados NLP
# ─────────────────────────────────────────────────────────────────────────────

nlp <- read_csv("resultados/nlp_textos.csv", show_col_types = FALSE) |>
  filter(str_detect(doc_id, "smith")) |>
  mutate(livro = ifelse(str_detect(doc_id, "tms"), "TMS (1759)", "WoN (1776)"))

# ─────────────────────────────────────────────────────────────────────────────
# Fig 1 — Perfil Loughran-McDonald (6 categorias)
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 1 — Perfil LM...\n")

lm_long <- nlp |>
  select(livro, lm_positive, lm_negative, lm_uncertainty,
         lm_constraining, lm_litigious, lm_superfluous) |>
  pivot_longer(-livro, names_to = "categoria", values_to = "score") |>
  mutate(
    categoria = str_remove(categoria, "lm_"),
    categoria = str_to_title(categoria),
    categoria = factor(categoria,
      levels = c("Positive", "Negative", "Uncertainty",
                 "Constraining", "Litigious", "Superfluous"))
  )

ggplot(lm_long, aes(x = categoria, y = score, fill = livro)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.4f", score)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3, color = "gray30") +
  scale_fill_manual(values = COL_BOTH) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Figure 1 — Loughran-McDonald Sentiment Profile: TMS vs. Wealth of Nations",
    subtitle = "Proportion of clean tokens classified per category (LM economic dictionary)",
    x        = NULL,
    y        = "Proportion of tokens (%)",
    caption  = "Source: authors' calculations. Dictionary: Loughran and McDonald (2011)."
  ) +
  tema_artigo

salvar("fig1_lm_perfil", largura = 8, altura = 5)

# ─────────────────────────────────────────────────────────────────────────────
# Fig 2 — Pronomes (pct_we, pct_i, ratio)
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 2 — Pronomes...\n")

pron_long <- nlp |>
  select(livro, pct_we, pct_i) |>
  pivot_longer(-livro, names_to = "pronome", values_to = "pct") |>
  mutate(
    pronome = ifelse(pronome == "pct_we", "pct_we  (we/our/us)", "pct_i  (I/my/me)"),
    livro_pron = paste(livro, pronome, sep = "\n")
  )

p_pron <- ggplot(pron_long, aes(x = pronome, y = pct, fill = livro)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.2f%%", pct * 100)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.2, color = "gray30") +
  scale_fill_manual(values = COL_BOTH) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Figure 2 — First-Person Pronouns: TMS vs. Wealth of Nations",
    subtitle = "H4: TMS shows higher use of 'we/our/us' — dialogic voice (Brown 1994)",
    x        = NULL,
    y        = "Proportion of all tokens (%)",
    caption  = "Source: authors' calculations. Extended replication of Ballandonne and Cersosimo (2023)."
  ) +
  tema_artigo

# Adiciona ratio_we_i como anotação
ratio_data <- nlp |>
  select(livro, ratio_we_i) |>
  mutate(pronome = "pct_we  (we/our/us)",
         y_pos   = nlp$pct_we + 0.001)

p_pron <- p_pron +
  geom_text(
    data = ratio_data,
    aes(x = pronome, y = y_pos + 0.003, label = sprintf("ratio_we_i = %.2f", ratio_we_i),
        color = livro),
    position = position_dodge(width = 0.7),
    size = 3, fontface = "italic", inherit.aes = FALSE
  ) +
  scale_color_manual(values = COL_BOTH, guide = "none")

print(p_pron)
salvar("fig2_pronomes", largura = 7.5, altura = 5)

# ─────────────────────────────────────────────────────────────────────────────
# Recalcula scores LM por capítulo (para figs 3 e extra)
# ─────────────────────────────────────────────────────────────────────────────

cat("Recalculando LM por capítulo...\n")

parse_por_capitulo <- function(path, livro_label) {
  if (!file.exists(path)) return(NULL)
  linhas <- readLines(path, encoding = "UTF-8", warn = FALSE)
  breaks <- which(str_detect(toupper(linhas), "^(CHAPTER|PART|BOOK)\\s+[IVXLC0-9]+"))
  if (length(breaks) < 5) breaks <- seq(1, length(linhas), by = 200)
  ends <- c(breaks[-1] - 1, length(linhas))
  map2_dfr(seq_along(breaks), ends, function(idx, e) {
    s <- breaks[idx]
    tibble(cap_id = paste0(livro_label, "_cap", idx),
           livro  = livro_label,
           texto  = paste(linhas[s:e], collapse = " "))
  }) |> filter(nchar(texto) > 500)
}

tms_caps <- parse_por_capitulo("textos/smith_tms.txt", "TMS (1759)")
won_caps <- parse_por_capitulo("textos/smith_won.txt", "WoN (1776)")
df_caps  <- bind_rows(tms_caps, won_caps)

lm_dict <- get_sentiments("loughran")

lm_caps <- df_caps |>
  unnest_tokens(word, texto) |>
  inner_join(lm_dict, by = "word", relationship = "many-to-many") |>
  count(cap_id, livro, sentiment) |>
  group_by(cap_id, livro) |>
  mutate(total_sent = sum(n), pct = n / total_sent) |>
  ungroup()

# ─────────────────────────────────────────────────────────────────────────────
# Fig 3 — Uncertainty por capítulo (boxplot + t-test)
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 3 — Boxplot uncertainty por capítulo...\n")

unc_caps <- lm_caps |> filter(sentiment == "uncertainty")

t_res <- t.test(pct ~ livro, data = unc_caps)
p_label <- if (t_res$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", t_res$p.value)
t_label <- sprintf("t = %.2f, %s", abs(t_res$statistic), p_label)

ggplot(unc_caps, aes(x = livro, y = pct, fill = livro)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5,
               outlier.fill = "white", width = 0.5) +
  geom_jitter(aes(color = livro), width = 0.12, alpha = 0.35, size = 1.2) +
  annotate("text", x = 1.5, y = max(unc_caps$pct) * 0.97,
           label = t_label, size = 3.5, color = "gray20", fontface = "italic") +
  annotate("segment", x = 1, xend = 2,
           y = max(unc_caps$pct) * 0.92, yend = max(unc_caps$pct) * 0.92,
           color = "gray40", linewidth = 0.4) +
  scale_fill_manual(values  = COL_BOTH, guide = "none") +
  scale_color_manual(values = COL_BOTH, guide = "none") +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title    = "Figure 3 — Chapter-level LM Uncertainty Distribution",
    subtitle = "H3: WN shows significantly higher uncertainty proportion (Welch's t-test, chapter level)",
    x        = NULL,
    y        = "Uncertainty words as % of LM-matched words per chapter",
    caption  = "Source: authors' calculations. Loughran-McDonald dictionary (2011)."
  ) +
  tema_artigo

salvar("fig3_uncertainty_boxplot", largura = 6, altura = 5)

# ─────────────────────────────────────────────────────────────────────────────
# Fig 4 — LDA: proporção de tópicos
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 4 — LDA topic distribution...\n")

lda_model <- tryCatch(readRDS("resultados/lda_model.rds"), error = function(e) NULL)

if (!is.null(lda_model)) {
  beta_all <- tidy(lda_model, matrix = "beta")

  # Tópico boilerplate = maior beta para "gutenberg"
  boilerplate_topic <- beta_all |>
    filter(term == "gutenberg") |>
    slice_max(beta, n = 1) |>
    pull(topic)

  # Rótulo: 3 palavras substantivas por tópico (excluindo boilerplate terms)
  stop_lda <- c("gutenberg", "project", "electronic", "copyright",
                "license", "foundation", "terms", "literary", "etext")
  top_words <- beta_all |>
    filter(!term %in% stop_lda) |>
    group_by(topic) |>
    slice_max(beta, n = 3) |>
    summarise(label = paste(term, collapse = " / "), .groups = "drop") |>
    mutate(
      topic_label = ifelse(
        topic == boilerplate_topic,
        paste0("T", topic, "  [boilerplate]"),
        paste0("T", topic, "  ", label)
      )
    )

  gamma_smith <- tidy(lda_model, matrix = "gamma") |>
    filter(str_detect(document, "smith")) |>
    mutate(livro = ifelse(str_detect(document, "tms"), "TMS (1759)", "WoN (1776)")) |>
    left_join(top_words, by = "topic")

  # Ordena tópicos por diff TMS-WoN (mais TMS-dominante no topo)
  ordem <- gamma_smith |>
    group_by(topic_label) |>
    summarise(diff = gamma[livro == "TMS (1759)"] - gamma[livro == "WoN (1776)"],
              .groups = "drop") |>
    arrange(diff) |>
    pull(topic_label)

  gamma_smith <- gamma_smith |>
    mutate(topic_label = factor(topic_label, levels = ordem))

  ggplot(gamma_smith, aes(x = gamma, y = topic_label, fill = livro)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.92) +
    geom_text(aes(label = ifelse(gamma > 0.02, sprintf("%.0f%%", gamma * 100), "")),
              position = position_dodge(width = 0.75),
              hjust = -0.15, size = 2.9, color = "gray25") +
    scale_fill_manual(values = COL_BOTH) +
    scale_x_continuous(labels = label_percent(accuracy = 1),
                       expand = expansion(mult = c(0, 0.14))) +
    labs(
      title    = "Figure 4 — LDA Topic Proportions in TMS and WN",
      subtitle = sprintf("LDA, K = %d; topics ordered from TMS-dominant (top) to WN-dominant (bottom)",
                         lda_model@k),
      x        = "Topic proportion (γ)",
      y        = NULL,
      caption  = "Source: authors' calculations. LDA via topicmodels package (Gibbs, 2,000 iterations)."
    ) +
    tema_artigo +
    theme(axis.text.y = element_text(size = 9),
          legend.position = "right")

  salvar("fig4_lda_topicos", largura = 9, altura = 5)
} else {
  cat("  AVISO: lda_model.rds não encontrado, Fig 4 não gerada.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Fig 5 — TF-IDF (lollipop chart)
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 5 — TF-IDF lollipop...\n")

STOP_EXTRA <- c("_of", "prepositions", "footnote", "verbs", "verb",
                "adjective", "substantive", "footnotes", "edition",
                "gutenberg", "project", "etext", "chapter", "book",
                "vol", "page", "ibid", "viz", "iii", "viz", "hath")

tfidf_smith <- bind_rows(
  tms_caps |> mutate(livro = "TMS (1759)"),
  won_caps |> mutate(livro = "WoN (1776)")
) |>
  unnest_tokens(word, texto) |>
  anti_join(tidytext::stop_words, by = "word") |>
  filter(nchar(word) >= 3,
         !str_detect(word, "^\\d+$"),
         !word %in% STOP_EXTRA) |>
  count(livro, word) |>
  bind_tf_idf(word, livro, n) |>
  group_by(livro) |>
  slice_max(tf_idf, n = 15) |>
  arrange(livro, desc(tf_idf)) |>
  ungroup() |>
  mutate(
    word  = reorder_within(word, tf_idf, livro),
    color = ifelse(str_detect(livro, "TMS"), COL_TMS, COL_WON)
  )

ggplot(tfidf_smith, aes(x = tf_idf, y = word, color = livro)) +
  geom_segment(aes(x = 0, xend = tf_idf, yend = word), linewidth = 0.7, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.95) +
  scale_color_manual(values = COL_BOTH, guide = "none") +
  scale_y_reordered() +
  scale_x_continuous(labels = label_number(accuracy = 0.0001)) +
  facet_wrap(~ livro, scales = "free_y", ncol = 2) +
  labs(
    title    = "Figure 5 — Most Distinctive Words per Work (TF-IDF)",
    subtitle = "Top 15 words per work — replication and extension of B&C (2023, Table 2)",
    x        = "TF-IDF",
    y        = NULL,
    caption  = "Source: authors' calculations. Stop words removed (tidytext). Cf. B&C (2023): sympathy, joy, sorrow in TMS."
  ) +
  tema_artigo +
  theme(panel.grid.major.x = element_line(color = "gray92"),
        panel.grid.major.y = element_blank())

salvar("fig5_tfidf_lollipop", largura = 9, altura = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Fig 6 — Legibilidade (Flesch, FK, ARI)
# ─────────────────────────────────────────────────────────────────────────────

cat("Gerando Fig 6 — Legibilidade...\n")

read_long <- nlp |>
  select(livro, Flesch, Flesch.Kincaid, ARI) |>
  pivot_longer(-livro, names_to = "indice", values_to = "valor") |>
  mutate(
    indice = recode(indice,
                    "Flesch"         = "Flesch\n(higher = easier)",
                    "Flesch.Kincaid" = "Flesch-Kincaid\n(grade level)",
                    "ARI"            = "ARI\n(grade level)"),
    indice = factor(indice, levels = c(
      "Flesch\n(higher = easier)",
      "Flesch-Kincaid\n(grade level)",
      "ARI\n(grade level)"))
  )

ggplot(read_long, aes(x = indice, y = valor, fill = livro)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.92) +
  geom_text(aes(label = round(valor, 1)),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.5, color = "gray20") +
  scale_fill_manual(values = COL_BOTH) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Figure 6 — Readability Indices: TMS vs. Wealth of Nations",
    subtitle = "Replication of B&C (2023): both texts of similar difficulty (grade ~15)",
    x        = NULL,
    y        = "Score",
    caption  = "Source: authors' calculations via quanteda.textstats. Cf. B&C (2023, Table 3)."
  ) +
  tema_artigo

salvar("fig6_legibilidade", largura = 7, altura = 4.5)

# ─────────────────────────────────────────────────────────────────────────────
# Resumo
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== Script 04 concluído ===\n")
cat("Figuras geradas em figuras/:\n")
arqs <- list.files("figuras/", pattern = "\\.(png|pdf)$")
for (a in sort(arqs)) cat(" ", a, "\n")
