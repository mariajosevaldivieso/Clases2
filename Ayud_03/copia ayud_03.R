library(data.table)    ## Este debiese de tenerlo instalado desde la ayudantía 01
library(ggplot2)       ## Este debiese de tenerlo instalado desde la clase 02
library(highcharter)
library(scales)
params<-NULL
params$month<-1
month <- params$month

Tabla_19_19 <- data.table(readRDS("Tabla_19_19.rds"))


cat_ocup <- Tabla_19_19[mes_central == params$month,.(sum(V1)),by= "cae_general_red"]

ggplot(data = cat_ocup, aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity")

ggplot(data = Tabla_19_19[mes_central == params$month,], aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity") +
  facet_wrap(~sexo,ncol = 2,scales = 'free_y',as.table = T)

Tabla_19_19$sexo <- factor(Tabla_19_19$sexo, levels = c(1,2), labels = c("Hombres","Mujeres"))


ggplot(data = Tabla_19_19[mes_central == params$month,], aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = number_format(scale = 0.000001, suffix = "M", big.mark = ".", decimal.mark = ",")) + 
  labs(x = "Categoría Ocupacional", y = "N° de Personas", fill = NULL) + 
  facet_wrap(~sexo,ncol = 2,scales = 'free_y',as.table = T)

ggplot(data = Tabla_19_19[cae_general_red == "Ocupados",], aes(x = mes_central, y = V1, fill = sexo)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = number_format(scale = 0.000001, suffix = "M", big.mark = ".", decimal.mark = ",")) + 
  labs(x = "Categoría Ocupacional", y = "N° de Personas", fill = NULL)

var_sexo <- Tabla_19_19[cae_general_red == "Ocupados", sum(V1), by = .(ano_trimestre,mes_central,sexo)]
var_sexo[,sexo_anterior := shift(V1, n = 1, type = 'lag'), by = .(sexo)]

var_sexo[,sexo_absoluto := round(V1 - sexo_anterior)]
var_sexo[,sexo_porcentual := round((V1 - sexo_anterior)/sexo_anterior,4)*100]

var_sexo$mes_nombre <- factor(var_sexo$mes_central, levels = c(2:12,1), labels = c("FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC","ENE"))

ggplot(data = var_sexo, aes(x = mes_nombre, y = sexo_absoluto, group = sexo, color = sexo)) + 
  geom_line(position = position_identity()) +
  scale_y_continuous(labels = number_format(scale = 0.001, suffix = "K", big.mark = ".", decimal.mark = ",")) + 
  labs(x = "Categoría Ocupacional", y = "N° de Personas", fill = NULL)

ggplot(data = var_sexo, aes(x = mes_nombre, y = sexo_porcentual, group = sexo, color = sexo)) + 
  geom_line(position = position_identity()) +
  scale_y_continuous(labels = number_format(scale = 1, suffix = "%", big.mark = ".", decimal.mark = ",")) + 
  labs(x = "Categoría Ocupacional", y = "N° de Personas", fill = NULL)

var_sexo[, fecha := ifelse(mes_central > 9, var_sexo$mes_central, paste0("0",var_sexo$mes_central))]
var_sexo[,fecha := as.Date(paste0(ano_trimestre,fecha,"01"),format = "%Y%m%d")]
var_sexo <- var_sexo[c(-1,-2)]

ggplot(data = var_sexo, aes(x = fecha, y = sexo_absoluto, group = sexo, color = sexo)) + 
  geom_hline(yintercept = 0) + 
  geom_line(position = position_identity()) + 
  geom_point(position = position_identity()) +
  geom_text(aes(x = unique(var_sexo$fecha)[9], y = 0, label = "aumento\n disminución"), colour="gray60") +
  geom_text(aes(label = sexo_absoluto), color = "black", nudge_y = 2500, size = 3) + 
  scale_y_continuous(labels = number_format(scale = 0.001, suffix = "K", big.mark = ".", decimal.mark = ",")) + 
  scale_x_date(date_labels = "%Y-%b", breaks = unique(var_sexo$fecha)[c(seq(1,12,2))]) +
  labs(x = "Categoría Ocupacional", y = "Variación de Personas", fill = NULL)

ggplot(data = Tabla_19_19[mes_central == params$month,round(sum(V1)),by = c("sexo","cae_general_red")], 
       aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity") + 
  geom_text(mapping = aes(label = V1),position = position_stack(0.5), size = 4, color = "white") + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = number_format(scale = 0.000001, suffix = "M", big.mark = ".", decimal.mark = ",")) + 
  labs(title = "CATEOGRÍAS OCUPACIONALES POR SEXO",subtitle = "Enero de 2020", x = NULL, y = NULL, fill = NULL, caption = "Obtenido de Nueva Encuesta Nacional de Empleo - INE") + 
  facet_grid(~sexo,scales = 'free_x',as.table = T) +
  theme_bw()

ggplot(data = Tabla_19_19[mes_central == params$month,round(sum(V1)),by = c("sexo","cae_general_red")], 
       aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity") + 
  geom_text(mapping = aes(label = V1),position = position_stack(0.5), size = 4, color = "white") + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = number_format(scale = 0.000001, suffix = "M", big.mark = ".", decimal.mark = ",")) + 
  labs(title = "CATEOGRÍAS OCUPACIONALES POR SEXO",subtitle = "Enero de 2020", x = NULL, y = NULL, fill = NULL, caption = "Obtenido de Nueva Encuesta Nacional de Empleo - INE") + 
  facet_grid(~sexo,scales = 'free_x',as.table = T) +
  theme_linedraw()

ggplot(data = Tabla_19_19[mes_central == params$month,round(sum(V1)),by = c("sexo","cae_general_red")], 
       aes(x = cae_general_red, y = V1, fill = cae_general_red)) + 
  geom_bar(stat="identity") + 
  geom_text(mapping = aes(label = V1),position = position_stack(0.5), size = 4, color = "white") + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(labels = number_format(scale = 0.000001, suffix = "M", big.mark = ".", decimal.mark = ",")) + 
  labs(title = "CATEOGRÍAS OCUPACIONALES POR SEXO",subtitle = "Enero de 2020", x = NULL, y = NULL, fill = NULL, caption = "Obtenido de Nueva Encuesta Nacional de Empleo - INE") + 
  facet_grid(~sexo,scales = 'free_x',as.table = T) +
  theme_bw() + 
  theme(text = element_text(size = 10),
        plot.caption = element_text(color = "#545453"),
        plot.background = element_blank(),
        panel.border = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid"), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'top', 
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.margin = margin(0, 0, 4, 0), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.box.margin = margin(0,0,0,0),
        legend.box.spacing = unit(0,'cm'), 
        strip.text.x = element_text(face = "italic", hjust = 0, color = "#00A297", size = 10), 
        strip.text.y = element_text(face = "italic", color = "#00A297"), 
        strip.background = element_rect(linetype = "blank"), 
        strip.background.x = element_blank(), 
        strip.background.y = element_rect(fill = "grey"),
        strip.placement = 'inside')

hchart(var_sexo, "line", hcaes(x = fecha, y = sexo_absoluto, group = sexo, color = sexo))

var_sexo_h <- var_sexo[sexo == "Hombres", .(fecha, Hombres = sexo_absoluto)]
var_sexo_m <- var_sexo[sexo == "Mujeres", .(fecha, Mujeres = sexo_absoluto)]
var_sexo_2 <- merge(var_sexo_h,var_sexo_m, by = "fecha", all = T)
var_sexo_2 <- ts(var_sexo_2[, .(Hombres,Mujeres)],start = c(2019,3), end =c(2020,1),frequency = 12)

hchart(var_sexo_2)

hchart(cat_ocup, "column", hcaes(x = cae_general_red, y = round(V1), color =cae_general_red))

#Opción 1
col_sexo_h <- Tabla_19_19[mes_central == params$month & sexo == "Hombres", .(Hombres = round(sum(V1))), by = .(cae_general_red)]
col_sexo_m <- Tabla_19_19[mes_central == params$month & sexo == "Mujeres", .(Mujeres = round(sum(V1))), by = .(cae_general_red)]
col_sexo <- merge(col_sexo_h,col_sexo_m, by = "cae_general_red", all = T)

# Opción 2
col_sexo_c <- Tabla_19_19[mes_central == params$month & cae_general_red == "Cesantes", .(Cesantes = round(sum(V1))), by = .(sexo)]
col_sexo_i <- Tabla_19_19[mes_central == params$month & cae_general_red == "Inactivos", .(Inactivos = round(sum(V1))), by = .(sexo)]
col_sexo_o <- Tabla_19_19[mes_central == params$month & cae_general_red == "Ocupados", .(Ocupados = round(sum(V1))), by = .(sexo)]
col_sexo_2 <- merge(col_sexo_c,col_sexo_i, by = "sexo", all = T)
col_sexo_2 <- merge(col_sexo_2,col_sexo_o, by = "sexo", all = T)

#Ahora presentamos los dos gráficos para las dos bases de datos antes colapsadas:

# Opción 1
purrr::map(c("Hombres","Mujeres"), function(z){
  highchart() %>%
    hc_xAxis(categories = c("Cesantes", "Inactivos", "Ocupados")) %>%
    hc_yAxis(labels = list(format = "{value}")) %>%
    hc_add_series(name = z, 
                  data = col_sexo[, z, by = "cae_general_red", with = F][[1]], 
                  type = "column", 
                  color = if(z == "Hombres"){"blue"}else{"red"}) %>%
    hc_title(text = paste("Categoría laboral:",z))
}) %>%
  hw_grid(rowheight = 525,ncol = 2) %>%
  htmltools::browsable()

# Opción 2
highchart() %>%
  hc_xAxis(categories = c("Hombres", "Mujeres")) %>%
  hc_add_series(name = "Cesantes", data = col_sexo_2$Cesantes, type = "column") %>%
  hc_add_series(name = "Inactivos", data = col_sexo_2$Inactivos, type = "column") %>%
  hc_add_series(name = "Ocupados", data = col_sexo_2$Ocupados, type = "column") %>%
  hc_title(text = paste("Categoría laboral")) %>%
  hc_yAxis(title = "none")
  
  