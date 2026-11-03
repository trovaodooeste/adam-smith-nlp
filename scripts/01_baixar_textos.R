# Script 01: Download de textos clássicos via Project Gutenberg
#
# Usa download.file() direto na CDN do Gutenberg — sem gutenbergr.
# Todos os textos estão em domínio público.
#
# Output: arquivos .txt na pasta textos/

dir.create("textos", showWarnings = FALSE)

baixar_gutenberg <- function(id, destino, nome_amigavel) {
  if (file.exists(destino)) {
    cat("Já existe:", destino, "\n")
    return(invisible(TRUE))
  }
  url <- sprintf("https://www.gutenberg.org/cache/epub/%d/pg%d.txt", id, id)
  cat("Baixando", nome_amigavel, "...\n  URL:", url, "\n")
  resultado <- tryCatch(
    download.file(url, destfile = destino, mode = "wb", quiet = FALSE),
    error = function(e) {
      cat("  ERRO:", conditionMessage(e), "\n")
      return(1L)
    }
  )
  if (resultado == 0 && file.exists(destino)) {
    n_linhas <- length(readLines(destino, warn = FALSE))
    cat("  Salvo:", destino, "(", n_linhas, "linhas )\n")
    return(invisible(TRUE))
  } else {
    cat("  Falha no download de", nome_amigavel, "\n")
    return(invisible(FALSE))
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO A (obrigatória): Adam Smith Problem
# ─────────────────────────────────────────────────────────────────────────────

baixar_gutenberg(67363, "textos/smith_tms.txt", "Theory of Moral Sentiments (TMS, 1759)")
baixar_gutenberg(3300, "textos/smith_won.txt", "Wealth of Nations (WoN, 1776)")

# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO C: Keynes, Knight (Hayek não está no Gutenberg)
# ─────────────────────────────────────────────────────────────────────────────

baixar_gutenberg(15776, "textos/keynes_general_theory.txt", "General Theory (Keynes, 1936)")
baixar_gutenberg(32625, "textos/keynes_treatise_prob.txt",  "Treatise on Probability (Keynes, 1921)")
baixar_gutenberg(11473, "textos/knight_risk.txt",            "Risk, Uncertainty and Profit (Knight, 1921)")

# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO D: Cientificação da economia
# ─────────────────────────────────────────────────────────────────────────────

baixar_gutenberg(33310, "textos/ricardo_principles.txt", "Principles of Political Economy (Ricardo, 1817)")
baixar_gutenberg(30107, "textos/mill_principles.txt",    "Principles of Political Economy (Mill, 1848)")

# ─────────────────────────────────────────────────────────────────────────────
# Resumo
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== Script 01 concluído ===\n")
cat("Textos disponíveis em textos/:\n")
arqs <- list.files("textos/", pattern = "\\.txt$")
if (length(arqs) == 0) {
  cat("  (nenhum arquivo encontrado)\n")
} else {
  for (a in arqs) {
    tam <- file.info(paste0("textos/", a))$size / 1024
    cat(sprintf("  %-45s %6.0f KB\n", a, tam))
  }
}
