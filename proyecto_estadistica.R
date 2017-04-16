# Javier Veredas Morente - IES Emilio Prados

############### DATOS DE 400 METROS LISOS #####################

# Lectura de los datos desde fichero
marcas <- read.csv("marca_400.csv")

# Frecuencias absolutas
freq <- table(marcas$marca_400)

# Frecuencias relativas
freq_rel <- freq/sum(freq) * 100

# Frecuencias acumuladas
freq_acu <- cumsum(freq)

# Diagrama de barras
# png("barras_400m.png")
barplot(table(marcas$marca_400), xlab = "Marcas 400 m lisos", ylab = "tiempo (seg)", col = "deepskyblue")
# dev.off()

# Histograma
# png("histograma_400m.png")
h <- hist(marcas$marca_400, breaks = 3, right = FALSE, col = "deepskyblue", xlab = "Marca 400 m lisos (seg)", ylab = "frecuencia (num atletas)", main = "")
points(h$mids, h$counts, col = "red", lwd = 2)
lines(h$mids, h$counts, col = "red", lwd = 2)
# dev.off()


# Diagrama de sectores
# png("sectores_400m.png")
pie(table(cut(marcas$marca_400, breaks =c(50, 55, 60, 65, 70), include.lowest = T, right = F)), col = c("cornflowerblue", "coral1", "darkolivegreen3","gold"))
# dev.off()

# Princpales estadísticos de centralización, dispersión y localización

# Media
media <- mean(marcas$marca_400)

# Desviación típica:
dt <- sd(marcas$marca_400)

# Coeficinente de variación
cv <- dt/media

# Moda
calcular_moda <- function(x) {
	t <- table(x)
	return(as.numeric(names(t)[t == max(t)]))
}
moda <- calcular_moda(marcas$marca_400)

# Cuartiles, mínimo, máximo y mediana
summary(marcas$marca_400)



######################### DATOS DE ESPERANZA DE VIDA #####################
### Datos obtenidos de: http://apps.who.int/gho/data/view.main.HALEXv?lang=en
# Lectura de los datos
esp.vida <- read.csv("esp_vida.csv", row.names = 1)

# Nombre de las variables
names(esp.vida) <- c("2000", "2015")

# Histogramas
png("histograma_esp_vida.png")
h1 <- hist(esp.vida$`2000`, breaks = 20)
h2 <- hist(esp.vida$`2015`, breaks = 20)
plot(h1, col = rgb(0,0,1, alpha = 0.3), xlab = "Esperanza de vida", ylab = "Frecuencia", main = "", border = F, ylim = c(0,35))
plot(h2, col = rgb(1,0,0, alpha = 0.3), add = T, border = F)
points(h1$mids, h1$counts, col = rgb(0,0,1, alpha = 0.5), lwd = 2)
lines(h1$mids, h1$counts, col = rgb(0,0,1, alpha = 0.5), lwd = 2)
points(h2$mids, h2$counts, col = rgb(1,0,0, alpha = 0.5), lwd = 2)
lines(h2$mids, h2$counts, col = rgb(1,0,0, alpha = 0.5), lwd = 2)
arrows(mean(esp.vida$`2000`), max(h1$counts)/5, mean(esp.vida$`2000`), 0.5, length = 0.1, col = rgb(0,0,1, alpha = 0.5), lwd = 4)
arrows(mean(esp.vida$`2015`), max(h1$counts)/5, mean(esp.vida$`2015`), 0.5, length = 0.1, col = rgb(1,0,0, alpha = 0.5), lwd = 4)
legend("topleft", c("2000", "2015"), fill = c(rgb(0,0,1, alpha = 0.3), rgb(1,0,0, alpha = 0.3)))
dev.off()


# Diagramas de cajas y bigotes
# png("boxplot_esp_vida.png")
boxplot(esp.vida, ylab = "Esperanza de vida", xlab = "Año")
# dev.off()
