########################################################################
## Description: Imputação de Dados Perdidos - Código Interativo
## 
## Maintainer: UNIVALI / EP / PPGCA
## Author: Rodrigo Sant'Ana
## Support: Claude AI
## Created: qua mar 18 17:51:54 2026 (-0300)
## Version: 0.0.1
## 
## URL: 
## Doc URL: 
## 
## Database info: 
## 
### Commentary: 
##
## COMO USAR ESTE SCRIPT:
##  → Rode cada seção passo a passo (Ctrl+Enter linha por linha, ou selecione
##    um bloco e execute)
##  → Leia os comentários ANTES de rodar cada bloco — eles explicam o que
##    acontecerá e o que você deve observar
##  → Nos blocos marcados com  ✏  EXERCÍCIO, tente resolver antes de ver
##    a solução no final do script
##  → Nos blocos marcados com 💬 DISCUSSÃO, reflita e anote suas respostas
## 
### Code:
########################################################################

## ─────────────────────────────────────────────────────────────────────
## MÓDULO 0 — INSTALAÇÃO E CARREGAMENTO DE PACOTES
## ─────────────────────────────────────────────────────────────────────
## Execute este bloco UMA VEZ antes de tudo. Pode demorar alguns minutos
## na primeira execução.
pacotes_necessarios <- c("mice", "VIM", "naniar", "tidyverse",
                         "ggplot2", "patchwork", "broom")

instalar_se_necessario <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(pacotes_necessarios, instalar_se_necessario))

## Carregar todos os pacotes
library(mice)
library(VIM)
library(naniar)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(patchwork)
library(tidyr)
library(broom)

cat("\n✅  Todos os pacotes carregados com sucesso!\n")
cat("   R versão:", R.version$version.string, "\n\n")

## =======================================================================
## MÓDULO 1 — O QUE SÃO DADOS AUSENTES?
## =======================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 1 — O QUE SÃO DADOS AUSENTES?\n")
cat("=" , strrep("=", 60), "\n\n")

## ───────────────────────────────────────────────────────────────────────
## 1.1  Criando e inspecionando NAs em R
## ───────────────────────────────────────────────────────────────────────
## NA é o símbolo que R usa para dados ausentes. Pode aparecer em qualquer
## tipo de dado: numérico, texto, lógico, data.

## Observe como R trata operações com NA:
x <- c(10, 20, NA, 40, 50)

cat("─── Operações com NA ───────────────────────────────────\n")
cat("Vetor:", x, "\n")
cat("Soma (sem na.rm):", sum(x), "     ← NA 'contamina' o resultado!\n")
cat("Soma (com na.rm):", sum(x, na.rm = TRUE), "   ← agora funciona\n")
cat("Média (com na.rm):", mean(x, na.rm = TRUE), "\n")
cat("É NA?", is.na(x), "\n\n")

## 💬  DISCUSSÃO:
## Por que NA "contamina" operações matemáticas?
## R interpreta: "não sei o valor, então não sei o resultado"
## Isso é matematicamente correto, mas exige atenção na prática!

## ───────────────────────────────────────────────────────────────────────
## 1.2  Dataset de exemplo — pacientes fictícios
## ───────────────────────────────────────────────────────────────────────
## Vamos criar um dataset clínico com ausências típicas da área de saúde.

set.seed(42)  # semente para reprodutibilidade — SEMPRE use em simulações!

n <- 120

pacientes <- tibble(
  id         = 1:n,
  idade      = sample(18:75, n, replace = TRUE),
  sexo       = sample(c("M", "F"), n, replace = TRUE),
  peso_kg    = round(rnorm(n, mean = 72, sd = 14), 1),
  altura_cm  = round(rnorm(n, mean = 168, sd = 10)),
  glicose    = round(rnorm(n, mean = 95, sd = 18)),
  pressao    = round(rnorm(n, mean = 120, sd = 15)),
  colesterol = round(rnorm(n, mean = 185, sd = 35)),
  grupo      = sample(c("Controle", "Tratamento"), n, replace = TRUE)
)

## Introduzindo ausências com diferentes mecanismos:
## — glicose: MCAR (aparelho com falha aleatória — 12% ausente)
## — peso_kg: MAR (pacientes obesos recusam mais — relacionado à altura)
## — colesterol: MNAR (pacientes com colesterol alto evitam o exame — 18%)

## MCAR: completamente aleatório
idx_mcar <- sample(1:n, size = round(0.12 * n))
pacientes$glicose[idx_mcar] <- NA

## MAR: probabilidade de ausência aumenta com altura (proxy de peso)
prob_ausencia_peso <- plogis((pacientes$altura_cm - 168) / 8)
ausente_peso <- rbinom(n, 1, prob = prob_ausencia_peso * 0.4) == 1
pacientes$peso_kg[ausente_peso] <- NA

## MNAR: quem tem colesterol mais alto é menos provável de reportar
prob_ausencia_col <- plogis((pacientes$colesterol - 185) / 20)
ausente_col <- rbinom(n, 1, prob = prob_ausencia_col * 0.5) == 1
pacientes$colesterol[ausente_col] <- NA

cat("─── Dataset criado com", n, "pacientes ───────────────────\n")
cat("Primeiras linhas:\n")
print(head(pacientes, 8))
cat("\nDimensões:", nrow(pacientes), "linhas ×", ncol(pacientes), "colunas\n\n")

## ✏  EXERCÍCIO 1.1
## Quantas células no total existem neste dataset?
## Quantas contêm NA?
## Qual a porcentagem geral de ausências?
## → Tente calcular antes de rodar a linha abaixo!

total_celulas  <- prod(dim(pacientes))
total_na       <- sum(is.na(pacientes))
pct_geral_na   <- round(total_na / total_celulas * 100, 1)

cat("─── Resumo de ausências ────────────────────────────────\n")
cat("Total de células:", total_celulas, "\n")
cat("Total de NAs:", total_na, "\n")
cat("Porcentagem geral de ausências:", pct_geral_na, "%\n\n")

## Ausências por variável:
na_por_var <- pacientes |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "n_ausente") |>
  mutate(pct_ausente = round(n_ausente / n * 100, 1)) |>
  arrange(desc(pct_ausente))

cat("─── Ausências por variável ─────────────────────────────\n")
print(na_por_var)
cat("\n")


## =============================================================================
## MÓDULO 2 — DIAGNÓSTICO VISUAL DE AUSÊNCIAS
## =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 2 — DIAGNÓSTICO VISUAL DE AUSÊNCIAS\n")
cat("=" , strrep("=", 60), "\n\n")

## ─────────────────────────────────────────────────────────────────────────────
## 2.1  Gráficos do pacote naniar
## ─────────────────────────────────────────────────────────────────────────────
## O pacote naniar oferece visualizações especializadas para ausências.
## Vamos explorar as principais.

cat("─── Gráfico 1: Percentual ausente por variável ─────────\n")
cat("Observe quais variáveis têm mais ausências.\n\n")

p1 <- gg_miss_var(pacientes, show_pct = TRUE) +
  labs(title = "% de Dados Ausentes por Variável",
       subtitle = "Dataset: pacientes clínicos (n = 120)",
       x = "% ausente", y = "Variável") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

### ─────────────────────────────────────────────────────────────────────────────
cat("\n─── Gráfico 2: Mapa de ausências (vis_miss) ────────────\n")
cat("Cada linha = 1 observação | Cinza = presente | Preto = ausente\n")
cat("Observe: há PADRÃO nas ausências ou parecem aleatórias?\n\n")

p2 <- vis_miss(pacientes, cluster = TRUE) +
  labs(title = "Mapa de Ausências — Padrão por Observação",
       subtitle = "Linhas agrupadas por padrão de ausência similar") +
  theme_minimal(base_size = 11)

print(p2)

## ─────────────────────────────────────────────────────────────────────────────
cat("\n─── Gráfico 3: Combinações de ausências ────────────────\n")
cat("Mostra quais variáveis tendem a estar ausentes JUNTAS.\n\n")

p3 <- gg_miss_upset(pacientes, nsets = 5) 
print(p3)

## ─────────────────────────────────────────────────────────────────────────────
cat("\n─── Gráfico 4: Padrão agregado com VIM::aggr ───────────\n")
cat("Lado esquerdo: % por variável | Lado direito: padrões combinados\n\n")

## Selecionamos apenas variáveis numéricas com ausência para o aggr
vars_com_na <- c("glicose", "peso_kg", "colesterol")

aggr(
  pacientes[, vars_com_na],
  col   = c("#12A5A5", "#D94F4F"),
  numbers = TRUE,
  sortVars = TRUE,
  labels  = vars_com_na,
  ylab    = c("Proporção de ausências", "Padrão"),
  main    = "Padrão Agregado de Ausências"
)

# ─────────────────────────────────────────────────────────────────────────────
# 2.2  Resumo tabular do naniar
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── Resumo tabular: miss_var_summary ───────────────────\n")
resumo_naniar <- miss_var_summary(pacientes)
print(resumo_naniar)

cat("\n─── Quantos casos têm PELO MENOS UM valor ausente? ─────\n")
n_casos_incompletos <- sum(!complete.cases(pacientes))
cat(n_casos_incompletos, "de", n, "casos têm ao menos 1 NA (", 
    round(n_casos_incompletos/n*100, 1), "%)\n\n")

# 💬  DISCUSSÃO:
# 1. Olhando vis_miss com cluster = TRUE, você consegue identificar padrões?
# 2. Alguma variável parece "sistematicamente" ausente junto com outra?
# 3. Isso sugere MCAR, MAR ou MNAR?


# =============================================================================
# ███  MÓDULO 3 — MECANISMOS DE AUSÊNCIA (TEORIA + TESTE)
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 3 — MECANISMOS: MCAR, MAR, MNAR\n")
cat("=" , strrep("=", 60), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3.1  Teste de Little (MCAR)
# ─────────────────────────────────────────────────────────────────────────────
# H0: os dados são MCAR
# p < 0.05 → rejeitamos MCAR → os dados provavelmente são MAR ou MNAR

cat("─── Teste de Little para MCAR ──────────────────────────\n")
cat("H0: os dados são MCAR\n")
cat("H1: os dados NÃO são MCAR\n\n")

# O teste usa apenas variáveis numéricas
dados_num <- pacientes |> select(where(is.numeric), -id)
resultado_little <- mcar_test(dados_num)

cat("Estatística de teste (χ²):", round(resultado_little$statistic, 3), "\n")
cat("Graus de liberdade:", resultado_little$df, "\n")
cat("p-valor:", format.pval(resultado_little$p.value, digits = 4), "\n\n")

if (resultado_little$p.value < 0.05) {
  cat("⚠  Conclusão: REJEITAMOS H0 (p < 0.05)\n")
  cat("   → Dados NÃO são MCAR → provavelmente MAR ou MNAR\n")
  cat("   → Imputação simples (média) pode introduzir viés!\n")
} else {
  cat("✅  Conclusão: Não rejeitamos H0 (p ≥ 0.05)\n")
  cat("   → Dados são consistentes com MCAR\n")
  cat("   → Imputação simples é razoável\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.2  Investigando MAR visualmente
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── Investigando MAR: peso_kg vs. altura_cm ────────────\n")
cat("Se peso_kg for MAR, sua ausência deve depender de outras variáveis.\n")
cat("Vamos verificar se a altura média difere entre quem tem/não tem peso.\n\n")

# Criar indicador de ausência
pacientes_diag <- pacientes |>
  mutate(
    peso_ausente    = is.na(peso_kg),
    glicose_ausente = is.na(glicose),
    col_ausente     = is.na(colesterol)
  )

# Comparar altura entre grupos com/sem peso
comparacao_mcar_mar <- pacientes_diag |>
  group_by(peso_ausente) |>
  summarise(
    n              = n(),
    altura_media   = round(mean(altura_cm), 1),
    altura_dp      = round(sd(altura_cm), 1),
    glicose_media  = round(mean(glicose, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(peso_ausente = ifelse(peso_ausente, "Peso AUSENTE", "Peso PRESENTE"))

cat("Se peso_kg for MAR (depende da altura), esperamos DIFERENÇA na altura:\n\n")
print(comparacao_mcar_mar)

p_mar <- ggplot(pacientes_diag, aes(x = peso_ausente, y = altura_cm, fill = peso_ausente)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.color = "#D94F4F") +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.2) +
  scale_fill_manual(values = c("FALSE" = "#0D7C7C", "TRUE" = "#D94F4F"),
                    labels = c("Peso presente", "Peso ausente")) +
  labs(title    = "Diagnóstico MAR: Altura por Status do Peso",
       subtitle = "Se alturas forem significativamente diferentes → peso é MAR",
       x        = "Peso kg está ausente?",
       y        = "Altura (cm)",
       fill     = NULL) +
  scale_x_discrete(labels = c("FALSE" = "Presente", "TRUE" = "Ausente")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(p_mar)

# Teste t para confirmar
t_test <- t.test(altura_cm ~ peso_ausente, data = pacientes_diag)
cat("\nTeste t (altura ~ ausência de peso):\n")
cat("p-valor:", format.pval(t_test$p.value, digits = 4), "\n")
cat("Diferença de médias:",
    round(diff(t_test$estimate), 2), "cm\n\n")

# 💬  DISCUSSÃO:
# A diferença de altura entre grupos é estatisticamente significativa?
# O que isso confirma sobre o mecanismo de ausência do peso_kg?

# =============================================================================
# ███  MÓDULO 4 — IMPUTAÇÃO SIMPLES (MÉDIA, MEDIANA, MODA)
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 4 — IMPUTAÇÃO SIMPLES\n")
cat("=" , strrep("=", 60), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.1  Implementando imputação simples do zero
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 4.1  Imputação manual pela média e mediana ─────────\n\n")

# Vamos trabalhar especificamente com a variável glicose (MCAR)
glicose_original <- pacientes$glicose  # salvar original (com NAs)
glicose_obs <- glicose_original[!is.na(glicose_original)]  # só observados

# Calcular estatísticas com dados observados
media_obs   <- mean(glicose_obs)
mediana_obs <- median(glicose_obs)

cat("Estatísticas de glicose (valores observados, n =", length(glicose_obs), "):\n")
cat("  Média:   ", round(media_obs, 2), "\n")
cat("  Mediana: ", round(mediana_obs, 2), "\n")
cat("  Desvio-padrão:", round(sd(glicose_obs), 2), "\n\n")

# Criar cópias para imputação
glicose_imp_media   <- glicose_original
glicose_imp_mediana <- glicose_original

# Imputar
glicose_imp_media[is.na(glicose_imp_media)]     <- media_obs
glicose_imp_mediana[is.na(glicose_imp_mediana)] <- mediana_obs

# Verificar que não há mais NAs
cat("NAs antes da imputação:", sum(is.na(glicose_original)), "\n")
cat("NAs após imputação pela média:", sum(is.na(glicose_imp_media)), "\n")
cat("NAs após imputação pela mediana:", sum(is.na(glicose_imp_mediana)), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.2  Efeito da imputação simples na distribuição
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 4.2  Visualizando o impacto na distribuição ────────\n")
cat("ATENÇÃO: observe o que acontece com a variância e a forma!\n\n")

# Comparar desvios-padrão
cat("Desvio-padrão antes (observados):", round(sd(glicose_obs), 2), "\n")
cat("Desvio-padrão após imput. média: ", round(sd(glicose_imp_media), 2), "\n")
cat("Desvio-padrão após imput. mediana:", round(sd(glicose_imp_mediana), 2), "\n")
cat("→ A variância DIMINUI após imputação simples!\n\n")

# Dataframe para visualização
df_viz <- bind_rows(
  tibble(glicose = glicose_obs,         tipo = "1. Original (sem NA)"),
  tibble(glicose = glicose_imp_media,   tipo = "2. Imputação pela Média"),
  tibble(glicose = glicose_imp_mediana, tipo = "3. Imputação pela Mediana")
)

p_dist <- ggplot(df_viz, aes(x = glicose, fill = tipo)) +
  geom_histogram(binwidth = 8, color = "white", alpha = 0.85) +
  geom_vline(data = df_viz |> group_by(tipo) |> summarise(m = mean(glicose)),
             aes(xintercept = m), color = "#1B2A4A", linewidth = 1, linetype = "dashed") +
  facet_wrap(~tipo, ncol = 1) +
  scale_fill_manual(values = c("#0D7C7C", "#E8A020", "#D94F4F")) +
  labs(title    = "Efeito da Imputação Simples na Distribuição da Glicose",
       subtitle = "Linha tracejada = média | Observe o pico artificial na média",
       x        = "Glicose (mg/dL)",
       y        = "Frequência") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

print(p_dist)

# ─────────────────────────────────────────────────────────────────────────────
# 4.3  Moda para variáveis categóricas
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── 4.3  Imputação pela moda (variável categórica) ─────\n\n")

# Criar ausências em sexo (MCAR simulado)
pacientes_cat <- pacientes
set.seed(7)
idx_sexo_na <- sample(1:n, 15)
pacientes_cat$sexo[idx_sexo_na] <- NA

cat("Tabela de sexo ANTES da imputação:\n")
print(table(pacientes_cat$sexo, useNA = "always"))

# Calcular moda
moda_sexo <- names(which.max(table(pacientes_cat$sexo, useNA = "no")))
cat("\nModa:", moda_sexo, "\n")

# Imputar pela moda
pacientes_cat$sexo[is.na(pacientes_cat$sexo)] <- moda_sexo

cat("\nTabela de sexo APÓS imputação pela moda:\n")
print(table(pacientes_cat$sexo, useNA = "always"))
cat("\n⚠  Atenção: a imputação pela moda inflaciona a categoria dominante!\n\n")

# =============================================================================
#  MÓDULO 5 — IMPUTAÇÃO POR kNN
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 5 — IMPUTAÇÃO POR kNN\n")
cat("=" , strrep("=", 60), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.1  Implementando kNN com VIM
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 5.1  kNN com VIM::kNN ──────────────────────────────\n")
cat("O kNN imputa com base nos k vizinhos mais próximos no espaço\n")
cat("de features — usa a distância de Gower para variáveis mistas.\n\n")

# Selecionar colunas relevantes para a imputação
# kNN usa as outras variáveis como 'vizinhos'
dados_knn <- pacientes |> select(-id)

# Rodar kNN com k = 5 vizinhos
# dist_var = variáveis usadas para calcular a distância
set.seed(123)
dados_knn_imp <- kNN(
  dados_knn,
  variable = c("glicose", "peso_kg", "colesterol"),  # o que imputar
  k        = 5,                                       # número de vizinhos
  dist_var = c("idade", "altura_cm", "pressao"),      # base para distância
  imp_var  = FALSE   # FALSE = não criar colunas indicadoras
)

cat("NAs antes do kNN:\n")
cat("  glicose:   ", sum(is.na(dados_knn$glicose)), "\n")
cat("  peso_kg:   ", sum(is.na(dados_knn$peso_kg)), "\n")
cat("  colesterol:", sum(is.na(dados_knn$colesterol)), "\n\n")

cat("NAs após kNN:\n")
cat("  glicose:   ", sum(is.na(dados_knn_imp$glicose)), "\n")
cat("  peso_kg:   ", sum(is.na(dados_knn_imp$peso_kg)), "\n")
cat("  colesterol:", sum(is.na(dados_knn_imp$colesterol)), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.2  Efeito do parâmetro k — sensibilidade
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 5.2  Sensibilidade ao parâmetro k ──────────────────\n")
cat("Como o valor de k afeta os resultados? Vamos testar k = 1, 3, 5, 10, 20\n\n")

ks <- c(1, 3, 5, 10, 20)

resultados_k <- map_dfr(ks, function(k_val) {
  set.seed(42)
  imp_k <- kNN(dados_knn, variable = "glicose", k = k_val, imp_var = FALSE)
  glicose_imp <- imp_k$glicose

  tibble(
    k            = k_val,
    media        = round(mean(glicose_imp), 2),
    desvio_padrao = round(sd(glicose_imp), 2),
    minimo       = round(min(glicose_imp), 1),
    maximo       = round(max(glicose_imp), 1)
  )
})

# Adicionar linha de referência (valores observados)
ref <- tibble(
  k             = 0,
  media         = round(mean(glicose_obs), 2),
  desvio_padrao = round(sd(glicose_obs), 2),
  minimo        = round(min(glicose_obs), 1),
  maximo        = round(max(glicose_obs), 1)
)

cat("k = 0: referência (apenas observados)\n")
print(bind_rows(ref, resultados_k))

cat("\n💡  Observe:\n")
cat("   k muito pequeno (k=1) → imputação instável, mais ruidosa\n")
cat("   k muito grande (k=20) → imputação suavizada (regressão à média)\n")
cat("   k = 5 a 10 costuma ser um bom equilíbrio\n\n")

# Visualização: distribuição com diferentes k
df_k_viz <- map_dfr(c(3, 5, 10), function(k_val) {
  set.seed(42)
  imp_k <- kNN(dados_knn, variable = "glicose", k = k_val, imp_var = FALSE)
  tibble(glicose = imp_k$glicose, k_label = paste0("kNN  k=", k_val))
})

df_k_viz <- bind_rows(
  tibble(glicose = glicose_obs, k_label = "Original (observados)"),
  df_k_viz
)

p_knn <- ggplot(df_k_viz, aes(x = glicose, fill = k_label)) +
  geom_density(alpha = 0.55, color = "white") +
  scale_fill_manual(values = c("#1B2A4A", "#0D7C7C", "#12A5A5", "#E8A020")) +
  labs(title    = "Distribuição da Glicose: kNN com Diferentes Valores de k",
       subtitle = "k maior → curva mais suave (regressão à média)",
       x        = "Glicose (mg/dL)", y = "Densidade", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(p_knn)


# =============================================================================
#  MÓDULO 6 — IMPUTAÇÃO MÚLTIPLA COM MICE
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 6 — IMPUTAÇÃO MÚLTIPLA COM MICE\n")
cat("=" , strrep("=", 60), "\n\n")

cat("O mice gera m datasets completos, analisa cada um e combina\n")
cat("os resultados usando as Regras de Rubin (1987).\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 6.1  Configuração e execução do mice
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 6.1  Rodando mice ──────────────────────────────────\n")
cat("Parâmetros: m = 10 datasets, método = 'pmm', 20 iterações\n\n")

# Preparar dados (remover id e variável categórica por simplicidade)
dados_mice <- pacientes |> select(-id, -sexo, -grupo)

# Inspecionar os métodos que o mice sugere automaticamente
metodos_sugeridos <- make.method(dados_mice)
cat("Métodos sugeridos pelo mice para cada variável:\n")
print(metodos_sugeridos)
cat("\n('pmm' = Predictive Mean Matching — padrão para variáveis contínuas)\n\n")

# Rodar a imputação múltipla
set.seed(2024)
imp_mice <- mice(
  dados_mice,
  m          = 10,      # número de datasets imputados
  method     = "pmm",   # método: pmm é robusto para variáveis contínuas
  maxit      = 20,      # número de iterações por dataset
  printFlag  = FALSE    # suprimir output detalhado (mude para TRUE para ver)
)

cat("✅  mice concluído!\n")
cat("Objeto mice:\n")
print(imp_mice)

# ─────────────────────────────────────────────────────────────────────────────
# 6.2  Diagnóstico de convergência
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── 6.2  Verificando convergência ──────────────────────\n")
cat("As linhas coloridas são os m datasets. Elas devem se misturar\n")
cat("(parecer 'ruído branco') sem tendências ou separações claras.\n\n")

# Trace plot — mostra convergência das cadeias de Markov
plot(imp_mice, 
     main = "Diagnóstico de Convergência do MICE\n(Trace Plot — deve parecer ruído branco)")

# ─────────────────────────────────────────────────────────────────────────────
# 6.3  Diagnóstico das distribuições imputadas
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── 6.3  Comparando distribuições observadas vs imputadas\n")
cat("Linha azul = observados | Linhas vermelhas = cada dataset imputado\n")
cat("As distribuições devem ser similares (se o mecanismo for MAR).\n\n")

densityplot(
  imp_mice,
  ~ glicose + peso_kg + colesterol,
  main = "Densidade: Observados (azul) vs Imputados (vermelho)"
)

# ─────────────────────────────────────────────────────────────────────────────
# 6.4  Extraindo um dataset completo
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── 6.4  Extraindo um dataset completo ─────────────────\n")

# complete() retorna um dos m datasets completos
# Use action = 1 a m para escolher, ou "long" para todos empilhados
dados_completo_1 <- complete(imp_mice, action = 1)
dados_completo_long <- complete(imp_mice, action = "long", include = TRUE)

cat("Dataset completo #1 — primeiras linhas:\n")
print(head(dados_completo_1, 5))
cat("\nNúmero de NAs no dataset completo:", sum(is.na(dados_completo_1)), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 6.5  Análise nos dados imputados + Regras de Rubin
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 6.5  Análise com pool() e Regras de Rubin ──────────\n")
cat("Ajustamos o modelo em CADA um dos m datasets e combinamos!\n\n")

# Modelo: glicose ~ idade + peso_kg + pressao
# with() ajusta o modelo em cada um dos m datasets
modelos <- with(imp_mice,
  lm(glicose ~ idade + peso_kg + pressao)
)

# pool() combina usando as Regras de Rubin
resultado_pool <- pool(modelos)
resumo_pool <- summary(resultado_pool)
resumo_pool$fmi <- resultado_pool$pooled$fmi

cat("Resultados combinados (Regras de Rubin):\n\n")
print(resumo_pool)

cat("\n─── Interpretando as colunas ───────────────────────────\n")
cat("estimate:  coeficiente combinado (média dos m modelos)\n")
cat("std.error: erro-padrão que incorpora incerteza DA imputação\n")
cat("statistic: estatística t\n")
cat("p.value:   p-valor para H0: coeficiente = 0\n")
cat("b:         variância ENTRE imputações\n")
cat("df:        graus de liberdade de Barnard-Rubin\n")
cat("fmi:       fração de informação ausente (quanto a ausência prejudicou)\n\n")

# Fração de informação ausente por variável
cat("─── Fração de Informação Ausente (FMI) ─────────────────\n")
cat("FMI > 0.5 indica que a ausência está prejudicando seriamente a estimativa\n\n")
fmi_df <- resumo_pool |>
  select(term, fmi = fmi) |>
  mutate(fmi = round(fmi, 3),
         avaliacao = case_when(
           fmi < 0.1  ~ "✅ Baixo impacto",
           fmi < 0.3  ~ "⚠  Impacto moderado",
           fmi < 0.5  ~ "⚠  Impacto significativo",
           TRUE       ~ "❌ Alto impacto"
         ))
print(fmi_df)
cat("\n")


# =============================================================================
#  MÓDULO 7 — COMPARAÇÃO DE MÉTODOS
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 7 — COMPARANDO OS MÉTODOS\n")
cat("=" , strrep("=", 60), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7.1  Comparação das distribuições de glicose após cada método
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 7.1  Distribuições comparadas lado a lado ──────────\n\n")

# Obter glicose imputada por kNN
set.seed(42)
knn_result <- kNN(dados_mice |> select(glicose, idade, altura_cm, pressao),
                  variable = "glicose", k = 5, imp_var = FALSE)

# Obter glicose imputada por mice (média dos m datasets)
idx_na <- is.na(pacientes$glicose)
glicose_mice_vals <- map_dbl(1:10, function(i) {
  d <- complete(imp_mice, i)
  mean(d$glicose[idx_na], na.rm = TRUE)
}) |> mean()

# Dataset para comparação
df_comp <- bind_rows(
  tibble(glicose = glicose_obs,       metodo = "1. Original"),
  tibble(glicose = glicose_imp_media, metodo = "2. Média"),
  tibble(glicose = knn_result$glicose, metodo = "3. kNN (k=5)"),
  tibble(glicose = complete(imp_mice, 1)$glicose, metodo = "4. MICE")
)

p_comp <- ggplot(df_comp, aes(x = glicose, fill = metodo, color = metodo)) +
  geom_density(alpha = 0.40, linewidth = 0.8) +
  scale_fill_manual(values  = c("#1B2A4A", "#E8A020", "#12A5A5", "#2D9E6A")) +
  scale_color_manual(values = c("#1B2A4A", "#E8A020", "#12A5A5", "#2D9E6A")) +
  labs(title    = "Comparação de Métodos: Distribuição da Glicose",
       subtitle = "Original (sem NA) × Média × kNN × MICE",
       x        = "Glicose (mg/dL)", y = "Densidade", fill = NULL, color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_comp)

# ─────────────────────────────────────────────────────────────────────────────
# 7.2  Tabela de estatísticas descritivas por método
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── 7.2  Estatísticas descritivas por método ───────────\n\n")

tab_comp <- df_comp |>
  group_by(metodo) |>
  summarise(
    n      = n(),
    media  = round(mean(glicose), 2),
    dp     = round(sd(glicose), 2),
    min    = round(min(glicose), 1),
    p25    = round(quantile(glicose, 0.25), 1),
    mediana= round(median(glicose), 1),
    p75    = round(quantile(glicose, 0.75), 1),
    max    = round(max(glicose), 1),
    .groups = "drop"
  )

print(tab_comp)

cat("\n💡  O que observar:\n")
cat("   → A MÉDIA deve ser similar entre os métodos\n")
cat("   → O DP (desvio-padrão) deve ser PRESERVADO — se cair muito, ruim!\n")
cat("   → A Média reduz DP mais que kNN e MICE\n")
cat("   → MICE tende a preservar melhor a distribuição original\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7.3  Boxplots comparativos
# ─────────────────────────────────────────────────────────────────────────────
p_box <- ggplot(df_comp, aes(x = metodo, y = glicose, fill = metodo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "#D94F4F") +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#1B2A4A", "#E8A020", "#12A5A5", "#2D9E6A")) +
  labs(title    = "Distribuição da Glicose por Método de Imputação",
       subtitle = "Boxplot + pontos individuais (jitter)",
       x        = NULL, y = "Glicose (mg/dL)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(p_box)


# =============================================================================
#  MÓDULO 8 — PRÁTICA GUIADA: DATASET AIRQUALITY
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 8 — PRÁTICA GUIADA COM AIRQUALITY\n")
cat("=" , strrep("=", 60), "\n\n")

cat("O dataset airquality tem dados de qualidade do ar em Nova Iorque (1973).\n")
cat("Vamos aplicar o fluxo COMPLETO de análise de ausências.\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 1: Conhecer o dataset
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 1: Conhecer o dataset ────────────────────────\n")

data("airquality")
cat("Dimensões:", nrow(airquality), "×", ncol(airquality), "\n")
cat("\nVariáveis:\n")
cat("  Ozone:   Concentração de ozônio (ppb)\n")
cat("  Solar.R: Radiação solar (Langleys)\n")
cat("  Wind:    Velocidade do vento (mph)\n")
cat("  Temp:    Temperatura (°F)\n")
cat("  Month:   Mês (5 a 9)\n")
cat("  Day:     Dia do mês\n\n")

print(summary(airquality))

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 2: Quantificar ausências
# ─────────────────────────────────────────────────────────────────────────────
cat("\n─── PASSO 2: Quantificar ausências ─────────────────────\n")

na_airq <- colSums(is.na(airquality))
pct_airq <- round(na_airq / nrow(airquality) * 100, 1)

cat("Ausências por variável:\n")
print(data.frame(n_ausente = na_airq, pct_ausente = pct_airq))

cat("\nTotal de casos incompletos:", sum(!complete.cases(airquality)),
    "de", nrow(airquality), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 3: Visualizar padrão
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 3: Visualizar padrão de ausências ────────────\n\n")

p_airq1 <- vis_miss(airquality) +
  labs(title    = "Mapa de Ausências — airquality",
       subtitle = "Ozone e Solar.R têm ausências. Há padrão temporal?") +
  theme_minimal(base_size = 12)

print(p_airq1)

# Existe padrão temporal? Vamos checar
airquality_diag <- airquality |>
  mutate(ozone_ausente = is.na(Ozone))

p_airq2 <- ggplot(airquality_diag, aes(x = Day, y = Ozone, color = ozone_ausente)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line(data = airquality_diag |> filter(!ozone_ausente),
            color = "#0D7C7C", alpha = 0.4) +
  facet_wrap(~Month, nrow = 1,
             labeller = labeller(Month = c("5"="Maio","6"="Junho","7"="Julho",
                                           "8"="Agosto","9"="Setembro"))) +
  scale_color_manual(values = c("FALSE" = "#0D7C7C", "TRUE" = "#D94F4F"),
                     labels = c("Presente", "Ausente")) +
  labs(title  = "Série Temporal de Ozônio por Mês",
       subtitle = "Pontos vermelhos = ausências | Há concentração em algum mês?",
       x = "Dia", y = "Ozônio (ppb)", color = "Ozone") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p_airq2)

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 4: Teste de Little
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 4: Teste de Little ───────────────────────────\n")

resultado_little_aq <- mcar_test(airquality)
cat("χ² =", round(resultado_little_aq$statistic, 3), "\n")
cat("df =", resultado_little_aq$df, "\n")
cat("p-valor =", format.pval(resultado_little_aq$p.value, digits = 4), "\n\n")

if (resultado_little_aq$p.value < 0.05) {
  cat("⚠  Não é MCAR → usar MICE!\n\n")
} else {
  cat("✅  Consistente com MCAR → qualquer método é adequado\n\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 5: Imputação com mice
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 5: Imputação com mice ────────────────────────\n\n")

set.seed(99)
imp_airq <- mice(
  airquality,
  m         = 10,
  method    = "pmm",
  maxit     = 15,
  printFlag = FALSE
)

cat("Método utilizado por variável:\n")
print(imp_airq$method)

airq_completo <- complete(imp_airq, action = 1)
cat("\nNAs após imputação:", sum(is.na(airq_completo)), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 6: Validar imputação
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 6: Validar imputação ─────────────────────────\n")
cat("Comparando distribuições antes × depois.\n\n")

densityplot(imp_airq, ~ Ozone + Solar.R,
            main = "airquality: Observados (azul) vs Imputados (vermelho)")

# Estatísticas descritivas de Ozone
cat("Estatísticas do Ozone:\n")
cat("  Antes (observados):",
    "média =", round(mean(airquality$Ozone, na.rm = TRUE), 1),
    "| DP =", round(sd(airquality$Ozone, na.rm = TRUE), 1), "\n")
cat("  Após MICE (dataset 1):",
    "média =", round(mean(airq_completo$Ozone), 1),
    "| DP =", round(sd(airq_completo$Ozone), 1), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASSO 7: Análise com dados imputados
# ─────────────────────────────────────────────────────────────────────────────
cat("─── PASSO 7: Modelo nos dados imputados ────────────────\n\n")

# Regressão: Ozone ~ Solar.R + Wind + Temp
modelos_airq <- with(imp_airq, lm(Ozone ~ Solar.R + Wind + Temp))
pool_airq    <- pool(modelos_airq)

cat("Modelo: Ozone ~ Solar.R + Wind + Temp\n")
cat("(combinado por Regras de Rubin — 10 datasets)\n\n")
print(summary(pool_airq))

# Comparar com casos completos
cat("\n─── Comparação: Casos Completos × MICE ─────────────────\n")
modelo_cc <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)

comp_cc_mice <- bind_rows(
  tidy(modelo_cc) |> mutate(metodo = "Casos Completos (n=111)"),
  summary(pool_airq) |>
    select(term, estimate, std.error, p.value) |>
    mutate(metodo = "MICE (n=153)")
) |>
  select(metodo, term, estimate, std.error, p.value) |>
  mutate(across(where(is.numeric), ~round(., 4)))

print(comp_cc_mice)

cat("\n💡  Interprete:\n")
cat("   → Os coeficientes mudam entre os métodos?\n")
cat("   → O erro-padrão do MICE é maior ou menor?\n")
cat("   → Qual modelo usa mais informação?\n\n")


# =============================================================================
#  MÓDULO 9 — SIMULAÇÃO: IMPACTO DO % DE AUSÊNCIA
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 9 — SIMULAÇÃO: IMPACTO DO % DE AUSÊNCIA\n")
cat("=" , strrep("=", 60), "\n\n")

cat("Vamos simular dados com coeficiente VERDADEIRO conhecido\n")
cat("e ver como cada % de ausência afeta a estimativa.\n\n")

set.seed(100)
N_sim     <- 500
x_sim     <- rnorm(N_sim, mean = 0, sd = 1)
y_sim     <- 2 + 1.5 * x_sim + rnorm(N_sim, sd = 0.8)
# Coeficiente verdadeiro de x = 1.5

proporcoes <- c(0.05, 0.10, 0.20, 0.30, 0.40, 0.50)

cat("Coeficiente verdadeiro de x:", 1.5, "\n\n")

resultados_sim <- map_dfr(proporcoes, function(pct) {
  set.seed(pct * 1000)

  # Introduzir ausências em y de forma MAR
  ausente <- rbinom(N_sim, 1, prob = plogis(x_sim * 0.5) * pct * 2) == 1
  pct_real <- mean(ausente)

  y_na <- y_sim
  y_na[ausente] <- NA

  df_sim <- data.frame(x = x_sim, y = y_na)

  # Casos completos
  coef_cc <- coef(lm(y ~ x, data = df_sim, na.action = na.omit))["x"]

  # Imputação pela média
  y_media <- y_na
  y_media[is.na(y_media)] <- mean(y_na, na.rm = TRUE)
  coef_media <- coef(lm(y ~ x, data = data.frame(x = x_sim, y = y_media)))["x"]

  # MICE
  set.seed(42)
  imp_sim <- tryCatch(
    mice(df_sim, m = 5, method = "pmm", maxit = 10, printFlag = FALSE),
    error = function(e) NULL
  )
  coef_mice <- if (!is.null(imp_sim)) {
    summary(pool(with(imp_sim, lm(y ~ x))))$estimate[2]
  } else NA

  tibble(
    pct_ausente    = round(pct_real * 100, 1),
    coef_verdadeiro = 1.5,
    coef_casos_completos = round(coef_cc, 3),
    coef_media     = round(coef_media, 3),
    coef_mice      = round(coef_mice, 3)
  )
})

cat("Coeficiente estimado vs verdadeiro (β = 1.5):\n\n")
print(resultados_sim)

# Visualização
df_sim_viz <- resultados_sim |>
  pivot_longer(cols = starts_with("coef_"),
               names_to = "metodo", values_to = "coeficiente") |>
  mutate(metodo = recode(metodo,
    "coef_verdadeiro"       = "Verdadeiro (β=1.5)",
    "coef_casos_completos"  = "Casos Completos",
    "coef_media"            = "Imput. Média",
    "coef_mice"             = "MICE"
  ))

p_sim <- ggplot(df_sim_viz |> filter(metodo != "Verdadeiro (β=1.5)"),
                aes(x = pct_ausente, y = coeficiente,
                    color = metodo, group = metodo)) +
  geom_hline(yintercept = 1.5, linetype = "dashed",
             color = "#1B2A4A", linewidth = 1) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  annotate("text", x = 7, y = 1.52, label = "β verdadeiro = 1.5",
           color = "#1B2A4A", size = 3.5, fontface = "italic") +
  scale_color_manual(values = c("#E8A020", "#D94F4F", "#2D9E6A")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title    = "Viés no Coeficiente por % de Ausência e Método",
       subtitle = "Quanto mais longe da linha tracejada, maior o viés",
       x        = "% de dados ausentes",
       y        = "Coeficiente estimado de X",
       color    = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_sim)

cat("\n💡  O que você observa?\n")
cat("   → A imputação pela média fica mais longe de β=1.5 conforme % aumenta?\n")
cat("   → O MICE permanece mais próximo do valor verdadeiro?\n")
cat("   → A partir de que % os métodos mais simples falham?\n\n")


# =============================================================================
#  MÓDULO 10 — BOAS PRÁTICAS E ANÁLISE DE SENSIBILIDADE
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  MÓDULO 10 — BOAS PRÁTICAS E ANÁLISE DE SENSIBILIDADE\n")
cat("=" , strrep("=", 60), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 10.1  Checklist de boas práticas
# ─────────────────────────────────────────────────────────────────────────────
cat("─── Checklist de Boas Práticas ─────────────────────────\n\n")

cat("✅  1. SEMPRE especificar set.seed() para reprodutibilidade\n")
cat("✅  2. Reportar % de ausências POR VARIÁVEL na seção de métodos\n")
cat("✅  3. Realizar o Teste de Little e reportar o resultado\n")
cat("✅  4. Usar m ≥ % de ausências (ex: 25% ausente → m ≥ 25)\n")
cat("✅  5. Verificar convergência com plot(imp)\n")
cat("✅  6. Comparar distribuições com densityplot(imp)\n")
cat("✅  7. Reportar a Fração de Informação Ausente (FMI)\n")
cat("✅  8. Realizar análise de sensibilidade (comparar métodos)\n")
cat("✅  9. Incluir variáveis auxiliares correlacionadas no modelo mice\n")
cat("✅ 10. Para MNAR: reportar limitações e realizar análise pior-caso\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 10.2  Análise de sensibilidade formal
# ─────────────────────────────────────────────────────────────────────────────
cat("─── 10.2  Análise de Sensibilidade: Ozone ~ Temp ───────\n")
cat("Comparando: Casos Completos × Média × kNN × MICE\n\n")

# Dataset: airquality, modelo: Ozone ~ Temp
aq_knn <- kNN(airquality |> select(Ozone, Solar.R, Wind, Temp),
              variable = "Ozone", k = 5, imp_var = FALSE)
aq_media <- airquality
aq_media$Ozone[is.na(aq_media$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)

modelos_sensib <- list(
  "Casos Completos" = lm(Ozone ~ Temp, data = airquality, na.action = na.omit),
  "Imput. Média"    = lm(Ozone ~ Temp, data = aq_media),
  "kNN (k=5)"       = lm(Ozone ~ Temp, data = aq_knn)
)

tab_sensib <- map_dfr(names(modelos_sensib), function(nome) {
  m <- modelos_sensib[[nome]]
  s <- summary(m)
  tibble(
    metodo          = nome,
    n               = nobs(m),
    coef_Temp       = round(coef(m)["Temp"], 3),
    se_Temp         = round(sqrt(vcov(m)["Temp","Temp"]), 3),
    R2              = round(s$r.squared, 3),
    p_valor         = format.pval(coef(summary(m))["Temp", "Pr(>|t|)"], digits = 3)
  )
})

# Adicionar MICE
mice_temp <- with(imp_airq, lm(Ozone ~ Temp))
pool_temp <- pool(mice_temp)
sr <- summary(pool_temp)
tab_sensib <- bind_rows(
  tab_sensib,
  tibble(metodo = "MICE (m=10)", n = 153,
         coef_Temp = round(sr$estimate[2], 3),
         se_Temp   = round(sr$std.error[2], 3),
         R2        = NA_real_,
         p_valor   = format.pval(sr$p.value[2], digits = 3))
)

cat("Análise de Sensibilidade — Coeficiente de Temp sobre Ozone:\n\n")
print(tab_sensib)

cat("\n💡  Se os coeficientes variam MUITO entre métodos → resultados frágeis!\n")
cat("   Se são estáveis → conclusões robustas ao método de imputação.\n\n")


# =============================================================================
#  EXERCÍCIOS PROPOSTOS (TENTE RESOLVER!)
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  EXERCÍCIOS PROPOSTOS\n")
cat("=" , strrep("=", 60), "\n\n")

cat("─── EXERCÍCIO 1 (Básico) ───────────────────────────────\n")
cat("Usando o dataset nhanes do pacote mice:\n")
cat("  a) Quantos NAs existem por variável?\n")
cat("  b) Qual a % de casos completos?\n")
cat("  c) Crie um vis_miss e um gg_miss_upset.\n\n")
cat("   data(nhanes, package = 'mice')\n\n")

cat("─── EXERCÍCIO 2 (Intermediário) ────────────────────────\n")
cat("Com o dataset pacientes criado neste script:\n")
cat("  a) Aplique kNN com k = 3, 7 e 15 para pressao (após introduzir NAs)\n")
cat("  b) Compare as distribuições em um único gráfico\n")
cat("  c) Qual k preserva melhor a distribuição original?\n\n")
cat("   Dica: primeiro crie NAs em pressao com:\n")
cat("   set.seed(1); pacientes$pressao[sample(1:120, 20)] <- NA\n\n")

cat("─── EXERCÍCIO 3 (Avançado) ─────────────────────────────\n")
cat("Use o dataset boys do VIM:\n")
cat("  a) Rode mice com m=15, método 'pmm'\n")
cat("  b) Ajuste lm(hgt ~ age + wgt) em cada dataset\n")
cat("  c) Compare com o modelo de casos completos\n")
cat("  d) Reporte a FMI e interprete o resultado\n\n")
cat("   data(boys, package = 'VIM')\n\n")

cat("─── EXERCÍCIO 4 (Desafio — Simulação) ──────────────────\n")
cat("  a) Crie um dataset com N=300, y = 3 + 0.8*x + epsilon\n")
cat("  b) Introduza 25% de ausências em y sob MCAR e MAR separadamente\n")
cat("  c) Aplique mice nos dois cenários\n")
cat("  d) Compare as estimativas de β com o valor verdadeiro (0.8)\n")
cat("  e) Em qual mecanismo o viés é maior? Por quê?\n\n")


# =============================================================================
#  SOLUÇÕES DOS EXERCÍCIOS
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  SOLUÇÕES DOS EXERCÍCIOS\n")
cat("  (Não espreite antes de tentar!)\n")
cat("=" , strrep("=", 60), "\n\n")

cat("─── SOLUÇÃO 1 ──────────────────────────────────────────\n")
cat("Execute o bloco abaixo:\n\n")

## ---- SOLUÇÃO 1 ----
# data(nhanes, package = "mice")
# cat("NAs por variável:\n"); print(colSums(is.na(nhanes)))
# cat("% casos completos:", mean(complete.cases(nhanes))*100, "%\n")
# vis_miss(nhanes)
# gg_miss_upset(nhanes)

cat("(descomente as linhas acima para rodar)\n\n")

cat("─── SOLUÇÃO 2 ──────────────────────────────────────────\n\n")

## ---- SOLUÇÃO 2 ----
set.seed(1)
pac2 <- pacientes
pac2$pressao[sample(1:120, 20)] <- NA
pressao_obs2 <- pac2$pressao[!is.na(pac2$pressao)]

res_ks_pressao <- map_dfr(c(3, 7, 15), function(k_val) {
  set.seed(42)
  imp_k <- kNN(pac2 |> select(pressao, idade, altura_cm, peso_kg) |> drop_na(-pressao),
               variable = "pressao", k = k_val, imp_var = FALSE)
  tibble(pressao = imp_k$pressao, k_label = paste0("kNN k=", k_val))
})

df_sol2 <- bind_rows(
  tibble(pressao = pressao_obs2, k_label = "Original"),
  res_ks_pressao
)

p_sol2 <- ggplot(df_sol2, aes(x = pressao, fill = k_label, color = k_label)) +
  geom_density(alpha = 0.3, linewidth = 0.9) +
  scale_fill_manual(values  = c("#1B2A4A","#0D7C7C","#12A5A5","#E8A020")) +
  scale_color_manual(values = c("#1B2A4A","#0D7C7C","#12A5A5","#E8A020")) +
  labs(title = "Solução Ex. 2 — Pressão por kNN com Diferentes k",
       x = "Pressão (mmHg)", y = "Densidade", fill = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_sol2)

cat("─── SOLUÇÃO 3 ──────────────────────────────────────────\n\n")

data(boys, package = "mice")

set.seed(77)
imp_boys <- mice(boys, m = 15, method = "pmm", maxit = 15, printFlag = FALSE)

mod_boys <- with(imp_boys, lm(hgt ~ age + wgt))
pool_boys <- pool(mod_boys)

cat("Modelo: hgt ~ age + wgt (dataset boys, mice m=15)\n\n")
print(summary(pool_boys))

cat("\nComparando com casos completos:\n")
mod_boys_cc <- lm(hgt ~ age + wgt, data = boys, na.action = na.omit)
print(tidy(mod_boys_cc))

cat("\nFração de Informação Ausente (FMI):\n")
print(summary(pool_boys) |>
  mutate(fmi = pool_boys$pooled$fmi) |>
  select(term, fmi) |>
  mutate(fmi = round(fmi, 3)))

cat("\n─── SOLUÇÃO 4 ──────────────────────────────────────────\n\n")

set.seed(200)
N4 <- 300
x4 <- rnorm(N4)
y4 <- 3 + 0.8 * x4 + rnorm(N4, sd = 1)

# MCAR: aleatório
ausente_mcar4 <- sample(1:N4, round(0.25 * N4))
y4_mcar <- y4; y4_mcar[ausente_mcar4] <- NA

# MAR: depende de x4
prob_mar4 <- plogis(x4 * 1.2)
ausente_mar4 <- rbinom(N4, 1, prob = prob_mar4 * 0.4) == 1
y4_mar <- y4; y4_mar[ausente_mar4] <- NA

cat("% ausente (MCAR):", round(mean(is.na(y4_mcar)) * 100, 1), "%\n")
cat("% ausente (MAR) :", round(mean(is.na(y4_mar))  * 100, 1), "%\n\n")

resultados4 <- map_dfr(list(
  list(mecan = "MCAR", y_na = y4_mcar),
  list(mecan = "MAR",  y_na = y4_mar)
), function(cfg) {
  df4 <- data.frame(x = x4, y = cfg$y_na)
  set.seed(42)
  imp4 <- mice(df4, m = 10, method = "pmm", maxit = 10, printFlag = FALSE)
  coef_pool <- summary(pool(with(imp4, lm(y ~ x))))$estimate[2]
  coef_cc4  <- coef(lm(y ~ x, data = df4, na.action = na.omit))["x"]
  tibble(
    mecanismo       = cfg$mecan,
    coef_verdadeiro = 0.8,
    coef_CC         = round(coef_cc4, 3),
    coef_MICE       = round(coef_pool, 3),
    vies_CC         = round(abs(coef_cc4 - 0.8), 3),
    vies_MICE       = round(abs(coef_pool - 0.8), 3)
  )
})

cat("Viés na estimativa de β (verdadeiro = 0.8):\n\n")
print(resultados4)

cat("\n💡  Conclusão da simulação:\n")
cat("   Sob MCAR: ambos têm viés baixo (imputação CC também é válida)\n")
cat("   Sob MAR:  MICE reduz o viés — casos completos são problemáticos\n\n")


# =============================================================================
#  RESUMO FINAL
# =============================================================================
cat("=" , strrep("=", 60), "\n")
cat("  RESUMO FINAL\n")
cat("=" , strrep("=", 60), "\n\n")

cat("📌  Pontos-chave desta aula:\n\n")

cat("1. Dados ausentes são inevitáveis — ignorá-los NÃO é neutro.\n\n")

cat("2. Identifique o mecanismo PRIMEIRO:\n")
cat("   MCAR → qualquer método | MAR → kNN/MICE | MNAR → modelos especiais\n\n")

cat("3. Regra prática de proporções:\n")
cat("   < 5%: média funciona | 5–20%: kNN | > 20%: MICE obrigatório\n\n")

cat("4. MICE é o gold standard:\n")
cat("   Use m ≥ % de ausências | Verifique convergência | Reporte FMI\n\n")

cat("5. Sempre faça análise de sensibilidade:\n")
cat("   Se os resultados mudam muito entre métodos → resultados frágeis!\n\n")

cat("6. Reprodutibilidade: sempre use set.seed() em imputações!\n\n")

cat("─── Pacotes utilizados neste script ────────────────────\n")
cat("mice   — imputação múltipla (van Buuren & Groothuis-Oudshoorn, 2011)\n")
cat("VIM    — visualização e kNN (Kowarik & Templ, 2016)\n")
cat("naniar — diagnóstico de ausências (Tierney & Cook, 2023)\n")
cat("tidyverse — manipulação e visualização\n\n")

cat("─── Referências ────────────────────────────────────────\n")
cat("van Buuren (2018). Flexible Imputation of Missing Data. CRC Press.\n")
cat("  → Disponível online: flexibleimputation.com\n")
cat("Rubin (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.\n")
cat("Little & Rubin (2002). Statistical Analysis with Missing Data. Wiley.\n\n")

cat("✅  Script concluído! Bons estudos!\n")

########################################################################
## 
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
## 
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
## 
##  You are free to:
## 
##  Share - copy and redistribute the material in any medium or format.
## 
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
## 
##  Under the following terms:
## 
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
## 
##  NonCommercial - You may not use the material for commercial
##  purposes.
## 
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
## 
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
## 
########################################################################