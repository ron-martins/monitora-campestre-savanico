### Proposta de guia prático para análise dos dados do Protocolo Campestre-Savânico do programa MONITORA

## Autor: Ronald Sodré Martins
## Co-autores: Celso Santos, Gislene Martins
## Instituto Chico Mendes de Conservação da Biodiversidade (ICMBio) / Núcleo de Gestão Integrado Cautário-Guaporé
## Data: 21-08-2024

### Instalação dos pacotes necessários
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(splitstackshape)) install.packages("splitstackshape")
if (!require(reshape2)) install.packages("reshape2")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stringr)) install.packages("stringr")
if (!require(patchwork)) install.packages("patchwork")
if (!require(forcats)) install.packages("forcats")
if (!require(gt)) install.packages("gt")
if (!require(gplots)) install.packages("gplots")
if (!require(readxl)) install.packages("readxl")
if (!require(xml2)) install.packages("xml2")
if (!require(purrr)) install.packages("purrr")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(viridis)) install.packages("viridis")
if (!require(flextable)) install.packages("flextable")

### Importação e tratamento dos dados

#### Arquivos do software ODK (XML)
process_xml_file <- function(file_path) {
  doc <- read_xml(file_path)
  
  registros <- xml_find_all(doc, ".//registro")
  
  registros_list <- vector("list", length(registros))
  
  for (i in seq_along(registros)) {
    ponto_amostral <- ifelse(length(xml_find_all(registros[[i]], ".//ponto_amostral")) > 0,
                             xml_text(xml_find_all(registros[[i]], ".//ponto_amostral")),
                             NA)
    ponto_metro <- ifelse(length(xml_find_all(registros[[i]], ".//ponto_metro")) > 0,
                          xml_text(xml_find_all(registros[[i]], ".//ponto_metro")),
                          NA)
    X <- ifelse(length(xml_find_all(registros[[i]], ".//tipo_forma_vida")) > 0,
                xml_text(xml_find_all(registros[[i]], ".//tipo_forma_vida")),
                NA)
    nativa <- ifelse(length(xml_find_all(registros[[i]], ".//forma_vida_nativa")) > 0,
                     xml_text(xml_find_all(registros[[i]], ".//forma_vida_nativa")),
                     NA)
    seca_morta <- ifelse(length(xml_find_all(registros[[i]], ".//forma_vida_seca_morta")) > 0,
                         xml_text(xml_find_all(registros[[i]], ".//forma_vida_seca_morta")),
                         NA)
    
    Ocorreram_impacto <- xml_text(xml_find_first(doc, "//impacto_manejo_uso"))
    
    Cobertura = xml_text(xml_find_first(doc, "//form_veg"))
    
    CICLO <- substr(xml_text(xml_find_first(doc, "//data")), 1, 4)
    
    Plaqueta = xml_text(xml_find_first(doc, "//num_placa"))
    
    registros_list[[i]] <- data.frame(
      CICLO = CICLO,
      Plaqueta = Plaqueta,
      Cobertura = Cobertura,
      Ocorreram_impacto = Ocorreram_impacto,
      ponto_amostral = as.numeric(ponto_amostral),
      ponto_metro = as.numeric(ponto_metro),
      X = X,
      nativa = nativa,
      seca_morta = seca_morta
    )
  }
  
  df <- bind_rows(registros_list)
  
  df <- df %>%
    mutate(
      Ocorreram_impacto = str_replace_all(Ocorreram_impacto, "não", "Não"),
      Ocorreram_impacto = str_replace_all(Ocorreram_impacto, "sim", "Sim"),
      Cobertura = str_replace_all(Cobertura, "campestre", "Campestre"),
      Cobertura = str_replace_all(Cobertura, "savanica", "Savânica")
    )
  
  df <- df %>%
    separate_rows(seca_morta, sep = " ") %>%
    separate_rows(nativa, sep = " ") %>%
    separate_rows(X, sep = " ") %>%
    mutate(nativa = ifelse(X == "nativa", nativa, ifelse(X == "seca_morta", NA, NA)),
           seca_morta = ifelse(X == "seca_morta", seca_morta, ifelse(X == "nativa", NA, NA))) %>%
    unique()
  
  return(df)
}
directory_path <- "dados/dados_odk/"
file_paths <- list.files(directory_path, pattern = "\\.xml$", full.names = TRUE, recursive = TRUE)
df.odk <- map_df(file_paths, process_xml_file)

#### Arquivos de planilhas (XLSX)
process_xlsx_file <- function(file_path) {
  
  dfs <- list()
  
  for (sheet_name in excel_sheets(file_path)) {
    df <- read_excel(file_path, sheet = sheet_name, skip = 4)
    df <- df[1:(nrow(df) -3), -((ncol(df) - 1):ncol(df))]
    
    df <- df %>%
      rename(ponto_metro = `TRENA (m)`,
             solo_nu = `Solo exposto / rochas`,
             serrapilheira = `Serapilheira / folhiço`,
             graminoide = Gramíneas,
             arbusto_acima = `Arbustos maior 0,5m`,
             erva_nao_graminoide = Ervas,
             arbusto_abaixo = `Arbustos menor 0,5m`,
             lianas = Cipós,
             arvore_acima = `Árvores c/ diâmetro maior que 5cm`,
             arvore_abaixo = `Árvores c/ diâmetro menor que 5cm`,
             seca_morta = `Plantas secas`,
             exotica = Exóticas
      )
    
    df$nativa <- apply(df, 1, function(row) {
      nativas <- c("graminoide", "arbusto_abaixo", "lianas", "erva_nao_graminoide", "arbusto_acima", "arvore_acima", "arvore_abaixo")
      valid_nativas <- nativas[!is.na(row[nativas]) & row[nativas] == "X"]
      paste(valid_nativas, collapse = " ")
    })
    
    df$ponto_metro <- as.numeric(df$ponto_metro)
    df <- df %>% arrange(as.numeric(ponto_metro))
    df$Plaqueta <- gsub("\\D", "", readxl::read_excel(file_path,sheet = sheet_name, range = "K2:U2", col_names = FALSE)[1,1])
    df$ponto_amostral <- as.integer(as.numeric(df$ponto_metro) / 0.5) + 1
    df$CICLO <- str_extract(gsub("\\D", "", readxl::read_excel(file_path, sheet = sheet_name, range = "A3:J3", col_names = FALSE)[1,1]), "\\d{4}$")
    
    df$X <- apply(df, 1, function(row) {
      columns_of_interest <- c("nativa", "solo_nu", "exotica", "seca_morta", "serrapilheira")
      paste(columns_of_interest[sapply(columns_of_interest, function(col) !is.na(row[col]) && row[col] != "")], collapse = " ")
    })
    
    dfs[[sheet_name]] <- df
  }
  
  df.merged <- bind_rows(dfs, .id = "aba") %>% select(-aba) %>% select(CICLO, Plaqueta, solo_nu, ponto_amostral, ponto_metro, X, nativa, seca_morta)
  
  df.final <- df.merged %>% separate_rows(X, sep = " ")
  
  df.final <- df.final %>% mutate(nativa = ifelse(X != "nativa", NA, nativa))
  
  df.final <- df.final %>% separate_rows(nativa, sep = " ")
  
  return(df.final)
}
file_path.2019 <- "dados/dados2019/MACS.Vegetação - REBIOGUAPORÉ.2019.xlsx"
file_path.2022 <- "dados/dados2022/MACS.Vegetação - REBIOGUAPORÉ.2022revisada.xlsx"
df.xlsx.2019 <- process_xlsx_file(file_path.2019)
df.xlsx.2022 <- process_xlsx_file(file_path.2022)
plaquetas_campestre <- c("1160", "1120", "1121", "1119", "1164", "1137", "1144", "1143", "1159", "1118", "1156", "1123", "1158", "1157", "1148", "1117", "1122", "1124", "1129", "1113", "1116", "1163", "1165", "1135", "1115", "1154", "1114", "1166")
plaquetas_savanica <- c("1128", "1126", "1105", "1101", "1125", "1104", "1127", "1132", "1136", "1103", "1133", "1134", "1130", "1131", "1151", "1153", "1140", "1107", "1141", "1112", "1108", "1142", "1111", "1109", "1110", "1155", "1152", "1102", "1106")
df.xlsx.2019 <- df.xlsx.2019 %>%
  mutate(Cobertura = case_when(
    Plaqueta %in% plaquetas_campestre ~ "Campestre",
    Plaqueta %in% plaquetas_savanica ~ "Savânica"))
df.xlsx.2022 <- df.xlsx.2022 %>%
  mutate(Cobertura = case_when(
    Plaqueta %in% plaquetas_campestre ~ "Campestre",
    Plaqueta %in% plaquetas_savanica ~ "Savânica"))
df.xlsx.2019 <- df.xlsx.2019 %>% 
  select(CICLO, Plaqueta, Cobertura, solo_nu, ponto_amostral, ponto_metro, X, nativa, seca_morta)
df.xlsx.2022 <- df.xlsx.2022 %>% 
  select(CICLO, Plaqueta, Cobertura, solo_nu, ponto_amostral, ponto_metro, X, nativa, seca_morta)

### Salvando os arquivos
write.csv(df.odk, "resultado_2023_REBIO_Guapore_ODK.csv", row.names = FALSE, quote = F)
write.csv(df.xlsx.2022, "resultado_2022_REBIO_Guapore_xlsx.csv", row.names = FALSE, quote = F)
write.csv(df.xlsx.2019, "resultado_2019_REBIO_Guapore_xlsx.csv", row.names = FALSE, quote = F)

### Visualização dos dados
#### Perfil da cobertura da vegetação para uma única expedição
df.xlsx.2019.nativas <- df.xlsx.2019 %>%
  filter(X == "nativa")
df.xlsx.2019.nativas$Cobertura <- factor(df.xlsx.2019.nativas$Cobertura, levels = c("Campestre", "Savânica")) 
df.xlsx.2019.nativas$nativa <- factor(df.xlsx.2019.nativas$nativa, 
                                      levels = names(sort(table(df.xlsx.2019.nativas$nativa), decreasing = TRUE)))

df.xlsx.2019.categoria <- df.xlsx.2019 %>%
  filter(X == "nativa") 
df.xlsx.2019.categoria$Cobertura <- factor(df.xlsx.2019.categoria$Cobertura, levels = c("Campestre", "Savânica"))
df.xlsx.2019.categoria <- df.xlsx.2019.categoria %>%
  mutate(Tipo = if_else(nativa %in% c("arbusto_acima", "arbusto_abaixo", "arvore_acima", "arvore_abaixo"),"Lenhosa", "Herbácea")) %>%
  group_by(CICLO, Cobertura, Tipo) %>%
  summarise(Contagem = n()) %>%
  ungroup()
df.xlsx.2019.categoria$Tipo <- factor(df.xlsx.2019.categoria$Tipo, levels = c("Lenhosa", "Herbácea")) 

df.xlsx.2019.prop <- df.xlsx.2019.categoria %>%
  group_by(Cobertura) %>%
  summarize(prop_lenhosas = sum(Contagem[Tipo == "Lenhosa"]) / sum(Contagem) * 100,
            prop_herbaceas = sum(Contagem[Tipo == "Herbácea"]) / sum(Contagem) * 100)
df.xlsx.2019.categoria <- left_join(df.xlsx.2019.categoria, df.xlsx.2019.prop, by = c("Cobertura"))
df.xlsx.2019.prop <- pivot_longer(df.xlsx.2019.prop, cols = c(prop_lenhosas, prop_herbaceas),
                                  names_to = "Tipo_Vegetacao", values_to = "Proporcao")

## Gráfico 1
ggplot(df.xlsx.2019 %>% 
         group_by(Cobertura, X) %>%
         summarise(count = n(), .groups = 'drop'), 
       aes(x = X, y = count, fill = Cobertura)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "",
       title = "Perfil da REBIO do Guaporé 2019", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gráfico 2
ggplot(merge(df.xlsx.2019 %>%
               mutate(X = factor(X, levels = df.xlsx.2019 %>% filter(Cobertura == "Savânica") %>% count(X) %>% arrange(desc(n)) %>% pull(X))) %>%
               group_by(Cobertura, X) %>%
               summarise(n = n_distinct(Plaqueta, ponto_amostral, X)),
             df.xlsx.2019 %>%
               mutate(X = factor(X, levels = df.xlsx.2019 %>% filter(Cobertura == "Savânica") %>% count(X) %>% arrange(desc(n)) %>% pull(X))) %>%
               group_by(Cobertura, Plaqueta) %>%
               summarise(pontos = n_distinct(ponto_amostral)) %>%
               group_by(Cobertura) %>%
               summarise(total = sum(pontos)),
             by = "Cobertura") %>%
         mutate(proporcao = n / total * 100), 
       aes(x = X, y = n, fill = X)) +
  geom_bar(stat = "identity", position = "dodge", linewidth = 1, size = 1, color = "black") +
  facet_wrap(~ Cobertura, scales = "free") +
  geom_text(aes(label = paste0(round(proporcao, 2), "%"),
                vjust = -0.5), size = 3.5) +
  scale_fill_viridis_d(option = "viridis",
                       labels = c("seca_morta" = "seca ou morta",
                                  "solo_nu" = "solo exposto",
                                  "exotica" = "exótica")) +
  labs(fill = "",
       title = "Perfil da REBIO do Guaporé 2019", x = "", y = "",
       caption = "proporção de pontos amostrais") +
  theme_void()

## Gráfico 3
ggplot(df.xlsx.2019.nativas, 
       aes(x = nativa, y = ..count.., fill = nativa)) +
  geom_bar(linewidth = 1, size = 1, color = "black") +
  scale_fill_viridis_d(option = "H",
                       labels = c("erva_nao_graminoide" = "erva não graminoide", "arbusto_acima" = "arbusto acima de 50 cm", 
                                  "arbusto_abaixo" = "arbusto abaixo de 50 cm","arvore_acima" = "árvore > 5 cm de diâmetro",
                                  "arvore_abaixo" = "árvore < 5 cm de diâmetro")) +
  labs(title = "Perfil da REBIO do Guaporé 2019", fill = "Forma de vida nativa" ,x = "", y = "") +
  theme_void() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3.5) +
  facet_wrap(~ Cobertura, drop = TRUE, ncol = 2)

## Gráfico 4
ggplot(df.xlsx.2019.prop, aes(x = Cobertura, y = Proporcao, fill = Tipo_Vegetacao)) +
  geom_bar(stat = "identity", position = "dodge", linewidth = 1, size = 1, color = "black") +
  scale_fill_manual(values = c("prop_lenhosas" = "#996600", "prop_herbaceas" = "#00CC66"),
                    name = "Proporção de formas de vida",
                    labels = c("prop_lenhosas" = "Lenhosas",
                               "prop_herbaceas" = "Herbáceas")) +
  geom_text(data = df.xlsx.2019.prop[df.xlsx.2019.prop$Tipo_Vegetacao == "prop_lenhosas", ],
            aes(label = paste0(round(Proporcao, digits = 2), "%"), vjust = -0.25, hjust = -0.5)) +
  geom_text(data = df.xlsx.2019.prop[df.xlsx.2019.prop$Tipo_Vegetacao == "prop_herbaceas", ],
            aes(label = paste0(round(Proporcao, digits = 2), "%"), vjust = 1.15, hjust = 1.5)) +
  theme_void() +
  labs(title = "Perfil da REBIO do Guaporé 2019") +
  facet_wrap(~Cobertura, scales = "free", ncol = 2)

#### Perfil da cobertura da vegetação para mais de uma expedição
df.UC <- bind_rows(df.odk %>% select(c(CICLO, Plaqueta, Cobertura, ponto_amostral, ponto_metro, X, nativa, seca_morta)),
                   df.xlsx.2022 %>% select(c(CICLO, Plaqueta, Cobertura, ponto_amostral, ponto_metro, X, nativa, seca_morta)))

df.UC.cober <- merge(df.UC %>%
                       mutate(X = factor(X, levels = df.UC %>% count(X) %>% arrange(desc(n)) %>% pull(X))) %>%
                       group_by(CICLO, Cobertura, X) %>%
                       summarise(n = n_distinct(Plaqueta, ponto_amostral, X)),
                     df.UC %>%
                       mutate(X = factor(X, levels = df.UC %>% count(X) %>% arrange(desc(n)) %>% pull(X))) %>%
                       group_by(CICLO, Cobertura, Plaqueta) %>%
                       summarise(pontos = n_distinct(ponto_amostral)) %>%
                       group_by(CICLO, Cobertura) %>%
                       summarise(total = sum(pontos)),
                     by = c("CICLO","Cobertura")) %>%
  mutate(proporcao = n / total * 100)

df.UC.nativas <- df.UC %>%
  filter(X == "nativa") %>%
  group_by(CICLO, Cobertura, nativa) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(CICLO, Cobertura) %>%
  mutate(Proporcao = Contagem / sum(Contagem) * 100) %>%
  ungroup()
df.UC.nativas$Cobertura <- factor(df.UC.nativas$Cobertura, levels = c("Campestre", "Savânica"))
df.UC.nativas$nativa <- factor(df.UC.nativas$nativa, 
                               levels = unique(df.UC.nativas$nativa[order(df.UC.nativas$Proporcao, decreasing = TRUE)]))

df.UC.categoria <- df.UC %>%
  filter(X == "nativa") 
df.UC.categoria$Cobertura <- factor(df.UC.categoria$Cobertura, levels = c("Campestre", "Savânica"))
df.UC.categoria <- df.UC.categoria %>%
  mutate(Tipo = if_else(nativa %in% c("arbusto_acima", "arbusto_abaixo", "arvore_acima", "arvore_abaixo"),"Lenhosa", "Herbácea")) %>%
  group_by(CICLO, Cobertura, Tipo) %>%
  summarise(Contagem = n()) %>%
  ungroup()
df.UC.categoria$Tipo <- factor(df.UC.categoria$Tipo, levels = c("Lenhosa", "Herbácea")) 

df.UC.prop <- df.UC.categoria %>%
  group_by(CICLO, Cobertura) %>%
  summarize(prop_lenhosas = sum(Contagem[Tipo == "Lenhosa"]) / sum(Contagem) * 100,
            prop_herbaceas = sum(Contagem[Tipo == "Herbácea"]) / sum(Contagem) * 100)
df.UC.categoria <- left_join(df.UC.categoria, df.UC.prop, by = c("CICLO", "Cobertura"))


df.UC.prop <- pivot_longer(df.UC.prop, cols = c(prop_lenhosas, prop_herbaceas),
                           names_to = "Tipo_Vegetacao", values_to = "Proporcao")

## Gráfico 1
ggplot(df.UC.cober, 
       aes(x = X, y = n, fill = X)) +
  geom_bar(stat = "identity", position = "dodge", linewidth = 1, size = 1, color = "black") +
  facet_wrap(~ CICLO + Cobertura, ncol = 2, scales = "free") +
  geom_text(data = df.UC.cober[df.UC.cober$proporcao >= 80, ],
            aes(label = paste0(round(proporcao, 2), "%"),
                vjust = 1.15), size = 2.5) +
  geom_text(data = df.UC.cober[df.UC.cober$proporcao < 80, ],
            aes(label = paste0(round(proporcao, 2), "%"),
                vjust = -0.5), size = 2.5) +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("seca_morta" = "seca ou morta",
                               "solo_nu" = "solo exposto",
                               "exotica" = "exótica")) +
  labs(fill = "",
       title = "Perfil da REBIO do Guaporé", x = "", y = "",
       caption = "proporção de pontos amostrais") +
  theme_void()

## Gráfico 2
ggplot(df.UC.nativas, aes(x = nativa, y = Proporcao, fill = nativa)) +
  geom_bar(linewidth = 1, stat = "identity", position = "dodge", size = 1, color = "black") +
  facet_wrap(~ CICLO + Cobertura, ncol = 2, scales = "free") +
  geom_text(data = df.UC.nativas[df.UC.nativas$Proporcao >= 50, ],
            aes(label = paste0(round(Proporcao, 0), "%"),
                vjust = 1.15), size = 3.5) +
  geom_text(data = df.UC.nativas[df.UC.nativas$Proporcao < 50, ],
            aes(label = paste0(round(Proporcao, 1), "%"),
                vjust = -0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("erva_nao_graminoide" = "erva não graminoide", "arbusto_acima" = "arbusto acima de 50 cm", 
                               "arbusto_abaixo" = "arbusto abaixo de 50 cm","arvore_acima" = "árvore > 5 cm de diâmetro",
                               "arvore_abaixo" = "árvore < 5 cm de diâmetro")) +
  labs(title = "Perfil da REBIO do Guaporé", fill = "Forma de vida nativa", x = "", y = "") +
  theme_void()

## Gráfico 3
ggplot(df.UC.prop, aes(x = Cobertura, y = Proporcao, fill = Tipo_Vegetacao)) +
  geom_bar(stat = "identity", position = "dodge", linewidth = 1, size = 1, color = "black") +
  scale_fill_manual(values = c("prop_lenhosas" = "#996600", "prop_herbaceas" = "#00CC66"),
                    name = "Proporção de formas de vida",
                    labels = c("prop_lenhosas" = "Lenhosas",
                               "prop_herbaceas" = "Herbáceas")) +
  geom_text(data = df.UC.prop[df.UC.prop$Tipo_Vegetacao == "prop_lenhosas", ],
            aes(label = paste0(round(Proporcao, digits = 2), "%"), vjust = -0.25, hjust = -0.1)) +
  geom_text(data = df.UC.prop[df.UC.prop$Tipo_Vegetacao == "prop_herbaceas", ],
            aes(label = paste0(round(Proporcao, digits = 2), "%"), vjust = 1.15, hjust = 1)) +
  theme_void() +
  labs(title = "Perfil da REBIO do Guaporé") +
  facet_wrap(~ CICLO + Cobertura, scales = "free", ncol = 2)














