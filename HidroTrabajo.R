inp <- read.csv("FDC.csv",na.strings = "")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

windows(width = 5.5, height = 5)

par(mar = c(6, 5, 4, 6.5))

plot(inp[,2],type= "l",
     main = "Serie de cadual Rio Estrella y Banano",
     cex.main = "1.3",
     col.main = "#006633",
     xlab= "Tiempo", 
     ylab = "Caudal (mm por dia",
     col = "#339966"
)
lines(inp[ ,3], col = "#99FF33")


legend(x = "bottomright",
       inset = c(-0.31, 0),    
       legend = c("Banano", "Estrella"), 
       lty = c(1, 1),         
       col = c("#339966", "#99FF33"),         
       lwd = 2,
       cex = 0.8,
       bty = "n",
       xpd = (TRUE)
)

summary(inp[,2:3])
hist(inp[,2], 
     main =("Rio Estrella"),
     col.main = "#333333",
     xlab = "Agua por dia",
     ylab = "Caudal",
     col = "#999966"
)


hist(inp[,3],
     main = "Rio Banano",
     col.main = "#333333",
     xlab = "Agua por dia",
     ylab = "Caudal",
     col = "#999966"
)

names(inp) <- c("fecha", "estrella","banano")
attach(inp)

Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")
sum

MAQ_Estrella <- tapply(estrella, format(Tempdate, format = "%Y"), FUN = sum)   
MAQ_Banano <- tapply(banano, format(Tempdate, format = "%Y"), FUN = sum)   

write.csv(rbind(MAQ_Estrella, MAQ_Banano),file= "MAQ.csv")

on.exit(par(opar))

plot(MAQ_Banano,ylim = c(100,3000),
     main = "Caudales por y mes de Rio Banano y Estrella",
     col.main = "#333333",
     xlab = "Meses",
     ylab = "Años",
     col = "#CC9900"
)
lines(MAQ_Estrella, col = "red")

MAQ_Estrella <- tapply(estrella, format(Tempdate, format = "%m"), FUN = sum)   
MAQ_Banano <- tapply(banano, format(Tempdate, format = "%m"), FUN = sum)   

#Analisis de correlacion

inp.lm <- lm(inp[,2] ~ inp[,3], data = inp)
summary(inp.lm)

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3],
     main = "Relacion caudales",
     ylab = "Rio banano",
     xlab = "Rio Estrella"
)

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)

summary(inp.lm)

plot(inp.lm)