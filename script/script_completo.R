library(tidyverse)
library(scales)
library(cowplot)
library(ggthemes)
library(ggrepel)

#---------------------------------------------------------------------------------------------
carregar <- function(file, extensao = "csv") {
  if(extensao == "csv") {
    read.csv(paste0("C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/data/", file))
  } else if(extensao == "csv2") {
    read.csv2(paste0("C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/data/", file))
  }
}

save_pdf_png <- function(plot, nome, diretorio, width, height) {
  for(device in c("png", "pdf")){
    ggsave(plot = plot,
           path = diretorio,
           width = width,
           height = height,
           dpi = 300,
           filename = paste0(nome, ".", device),
           device = device)
  }
}

translate <- function(nome) {
  if(nome == "Lower middle income") {
    nome = "Renda média baixa"
  } else if(nome == "Upper middle income") {
    nome = "Renda média alta"
  } else if(nome == "High income") {
    nome = "Alta renda"
  }
}

#--------------------------------------------------------------------------------------

continents <- carregar("continents.txt", "csv2") %>% rename(country = Country)

gdp <- carregar("GDP.csv")

suicide <- carregar("master.csv") %>% rename(country = ï..country) %>%
  inner_join(continents) %>%
  rename(continent = Continent) %>%
  inner_join(gdp, by = c("country" = "TableName")) %>%
  mutate(IncomeGroup = as.factor(IncomeGroup))

worldmap <- jpeg::readJPEG("C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/data/world_map.jpg")


traducao <- carregar("continents_pt.txt", "csv2") %>%
  rename(country_pt = PaÃ.s) %>%
  filter(row_number() < 194)


traducao$Continente <- iconv(traducao$Continente, from="UTF-8", to="LATIN1")

traducao$country_pt <- iconv(traducao$country_pt, from="UTF-8", to="LATIN1")

traducao <- cbind(traducao, continents) %>% select(country_pt, country)

suicide <- suicide %>%
  inner_join(traducao)


### GRÁFICO I

suicide %>%
  group_by(country, sex, year, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  filter(!suicide.rate < 0, !country == ifelse(suicide.rate == 0, FALSE, TRUE)) %>%
  ungroup() %>%
  mutate(country = reorder(country, -suicide.rate, FUN = mean)) %>%
  group_by(country) %>%
  summarise(suicide = mean(suicide.rate)) %>%
  top_n(44)  -> selected_countries


suicide %>%
  group_by(country, sex, year, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  filter(!suicide.rate < 0, year %in% seq(1986, 2016, by = 2)) %>%
  ungroup() %>%
  mutate(country = reorder(country, suicide.rate, FUN = mean)) %>%
  filter(country %in% selected_countries$country) %>%
  ggplot() +
  geom_point(aes(x = country, y = suicide.rate, color = sex, alpha = year), size = 3.5) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,4,0,0), "cm"),
        legend.position = c(1.18, .8)) +
  scale_alpha_continuous(range = c(0, .9),
                         name = "Ano") +
  scale_color_manual(values = c("#D147A1", "#0470C2"),
                     labels = c("Feminino", "Maculino"),
                     name = "Sexo") +
  scale_y_sqrt(name = "Taxa de suicídio (por cem mil habitantes)",
               labels = function(b){paste0(b*10^5)}) -> suicide_plot




suicide %>% 
  group_by(country, sex, year, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  filter(!suicide.rate < 0, year %in% seq(1986, 2016, by = 2)) %>%
  ungroup() %>%
  mutate(country = reorder(country, suicide.rate, FUN = mean)) %>%
  filter(country %in% selected_countries$country) %>%
  ggplot() +
  geom_point(aes(x = country, y = .9, color = continent), size = 2.5, shape = 15) +
  coord_flip() +
  theme_bw() +
  guides(color = FALSE, alpha = FALSE) +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "white"),
        plot.margin = unit(c(0,.10, 0,0), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = c("#FFB400",
                                "#B20A01",
                                "#008FA0",
                                "#007701",
                                "#003AB5",
                                "#890068")) +
  scale_y_continuous(name = " ") -> continent_label




ggdraw() +
  draw_image(worldmap) -> map


ggdraw() +
  draw_label("As taxas de suicídio por gênero, por ano e por região",
             size = 18/1.3,
             fontface = "bold",
             fontfamily = "mono") -> title

plot_grid(continent_label, suicide_plot, nrow = 1, rel_widths = c(.2, 5)) -> main_plot


plot_grid(map, title, nrow = 1, rel_widths = c(0.8, 3)) -> map_title


plot_grid(map_title, main_plot, ncol = 1, rel_heights = c(.35, 4)) -> output_suicide

save_pdf_png(plot = output_suicide, nome = "suicide_44", 
             diretorio = "C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/output",
             width = 8, height = 9.5)

output_suicide

#-----------------------------------------------------------------------------------------------------------------------------------

### GRÁFICO II (e II.V)

suicide %>%
  filter(year == 2010) %>%
  group_by(country, continent, HDI.for.year) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  ungroup() %>%
  ggplot(aes(x = HDI.for.year, y = suicide.rate)) +
  geom_point() +
  labs(x = "Índice de desenvolvimento humano", y = "Taxa de suicídios (por cem mil habitantes)") +
  scale_y_continuous(labels = function(b){paste0(b*10^5)}) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))-> hdi_suicide

save_pdf_png(plot = hdi_suicide, width = 8, height = 8, diretorio = "C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/output",
             nome = "hdi_suicide")

suicide %>%
  mutate(IncomeGroup = sapply(IncomeGroup, translate)) %>%
  mutate(IncomeGroup = factor(IncomeGroup, levels = c("Renda média baixa", "Renda média alta", "Alta renda"))) %>%
  group_by(country, year, HDI.for.year, IncomeGroup, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  ungroup() %>%
  filter(year == 2010,
         !suicide.rate == 0) %>%
  ggplot() +
  geom_boxplot(aes(x = IncomeGroup, y = suicide.rate, fill = IncomeGroup)) +
  scale_y_sqrt(name = "Taxa de suicídios (por cem mil habitantes)",
               labels = function(b){paste0(b * 10^5)}) +
  theme_bw() +
  scale_fill_manual(values = c("#f7fcb9",
                               "#addd8e",
                               "#31a354")
  ) +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13.5, face = "italic"),
        plot.title = element_text(hjust = .5, size = 16, family = "mono", face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 2),
        plot.caption = element_text(hjust = 0, size = 9.5)) +
  ggtitle("Taxa de suicídios por grupo de renda") +
  labs(captions = "Dados do Banco Mundial e da Organização Mundial da Saúde.") -> boxplot_incomegroup



boxplot_incomegroup

save_pdf_png(plot = boxplot_incomegroup,
             nome = "boxplot_suicidio",
             diretorio = "C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/output",
             width = 11, height = 8)

#-----------------------------------------------------------------------------------------------------------------------------------

### GRÁFICO III



suicide %>%
  group_by(sex, age) %>%
  summarise(suicides = sum(suicides_no)/sum(population)) %>%
  ungroup() %>%
  mutate(suicides = ifelse(sex == "female", -suicides, suicides),
         age = factor(age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))) %>%
  ggplot(aes(y = age)) +
  geom_col(aes(x = suicides, fill = sex)) +
  theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_text(size = 13.5, face = "italic"),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 16, family = "mono", face = "bold", hjust = .5),
        axis.title.x = element_text(size = 13.5, face = "italic"),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 13),
        plot.caption = element_text(size = 9.5, hjust = 0)) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(labels = c("20", "0", "20", "40"),
                     name = "Taxa de suicídios (por cem mil habitantes)",
                     limits = c(-25e-05, 45e-05)) +
  scale_y_discrete(name = "Faixa etária (em anos)",
                   labels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+")) +
  scale_fill_manual(labels = c("Feminino", "Masculino"),
                    values = c("#D147A1","#0470C2")) +
  ggtitle("Taxa de suicídios por faixa etária") +
  labs(captions = "Dados do Banco Mundial e da Organização Mundial da Saúde.") -> age_suicide

age_suicide



save_pdf_png(nome = "age_suicide", width = 9, height = 9, plot = age_suicide,
             diretorio = "C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/output")


# GRÁFICO I, VERSÃO II

suicide %>%
  group_by(country_pt, country, sex, year, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  filter(!suicide.rate < 0, !country == ifelse(suicide.rate == 0, FALSE, TRUE)) %>%
  ungroup() %>%
  mutate(country_pt = reorder(country_pt, -suicide.rate, FUN = mean)) %>%
  group_by(country_pt) %>%
  summarise(suicide = mean(suicide.rate)) %>%
  top_n(45) -> selected_countries


suicide %>%
  group_by(country_pt, country, sex, year, continent) %>%
  summarise(suicide.rate = sum(suicides_no)/sum(population)) %>%
  filter(!suicide.rate <= 0, year %in% seq(1986, 2016, by = 1)) %>%
  ungroup() %>%
  mutate(country_pt = reorder(country_pt, suicide.rate, FUN = mean)) %>%
  filter(country_pt %in% selected_countries$country_pt) -> suicide_gender

suicide_gender %>%
  count(country_pt) %>%
  filter(n > 30) -> selected_countriesII

suicide_gender %>%
  filter(country_pt %in% selected_countriesII$country_pt) -> suicide_gender


suicide_gender %>%
  ggplot() +
  geom_point(data = suicide_gender %>% filter(sex == "female"), 
             aes(x = country_pt, y = suicide.rate, color = year), size = 3.5) +
  geom_point(data = suicide_gender %>% filter(sex == "male"),
             shape = 21,
             aes(x = country_pt, y = suicide.rate, fill = year), size = 3.5, color = "transparent") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,4.5,0,0), "cm"),
        legend.position = c(1.18, .69),
        legend.title = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "grey", linetype = 3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_fill_gradient(low = "#deebf7", high = "#084081",
                      "Ano (masculino)") + 
  scale_color_gradient(low = "#fcc5c0", high = "#ae017e",
                       name = "Ano (feminino)") +
  scale_y_log10(name = "Taxa de suicídio (por cem mil habitantes)",
                labels = function(b){paste0(b*10^5)}) +
  guides(fill = guide_colourbar(ticks.colour = "transparent", 
                                barheight = unit(5, "cm"),
                                frame.colour = "black"),
         color = guide_colourbar(ticks.colour = "transparent", 
                                 barheight = unit(5, "cm"),
                                 frame.colour = "black")) -> suicide_plotII




suicide_gender %>%
  ggplot() +
  geom_point(aes(x = country_pt, y = .9, color = continent), size = 2.5, shape = 15) +
  coord_flip() +
  theme_bw() +
  guides(color = FALSE, alpha = FALSE) +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "white"),
        plot.margin = unit(c(0,.10, 0,0), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = c("#FFB400",
                                "#B20A01",
                                "#008FA0",
                                "#007701",
                                "#003AB5",
                                "#890068")) +
  scale_y_continuous(name = " ") -> continent_labelII




ggdraw() +
  draw_image(worldmap) -> mapII


ggdraw() +
  draw_label("As taxas de suicídio por gênero, por ano e por região",
             size = 18/1.3,
             fontface = "bold",
             fontfamily = "mono") -> titleII

plot_grid(continent_labelII, suicide_plotII, nrow = 1, rel_widths = c(.2, 5)) -> main_plotII


plot_grid(mapII, titleII, nrow = 1, rel_widths = c(0.8, 3)) -> map_titleII


plot_grid(map_titleII, main_plotII, ncol = 1, rel_heights = c(.35, 4)) -> output_suicideII

output_suicideII

save_pdf_png(plot = output_suicideII, nome = "suicide_II", 
             diretorio = "C:/Users/tiago/OneDrive/Documentos/aed-projeto.suicidio/output",
             width = 9, height = 9.5)

