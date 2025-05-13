
unit.multipliers <- list(
  Mc = 1024 * 1024,
  Gn = 1024 * 1024 * 1024,
  Mn = 1024 * 1024,
  Gc = 1024 * 1024 * 1024
)

function1 <- function(memory){
  match <- regexpr("^(\\d+)(\\w+)$", memory)
  
  if (match!=-1) {
    memory.num <- as.numeric(sub("^(\\d+)(\\w+)$", "\\1", memory))
    unit <- sub("^(\\d+)(\\w+)$", "\\2", memory)
    
    multiplier <- unit.multipliers[[unit]]
    
    if (!is.null(multiplier)) {
      return(memory.num * multiplier)
    } else {
      return(NA)  
    }
  } else {
    return(NA)  
  }
}

function2 <- function(df, energy, time) {
  if (!(energy %in% colnames(df))) {
    stop(paste("Columna '", energy_col, "no encontrada en el DataFrame.", sep = ""))
  }
  if (!(time %in% colnames(df))) {
    stop(paste("Columna '", time, "' no encontrada en el DataFrame.", sep = ""))
  }
  
  df <- df %>% mutate(CPUPower = .[[energy]] / .[[time]]) %>% mutate(CPUPower = ifelse(is.finite(CPUPower), CPUPower, NA))
  
  return(df)
}

RSS <- function(Pred, Real){
  ss <- sum((Real-Pred)^2)
  return(ss)}

MSE <- function(Pred, Real){
  N <- length(Real)
  ss <- (1/N)*RSS(Pred,Real)
  return(ss)}

RMSE <- function(Pred, Real){
  ss <- sqrt(MSE(Pred, Real))
  return(ss)}

RSE <- function(Pred, Real, NumPred){
  N <- length(Real)-(NumPred+1)
  ss <- sqrt((1/N)*RSS(Pred,Real))
  return(ss)}

MAE <- function(Pred, Real){
  ss <- sum(abs(Real-Pred))/length(Real)
  return(ss)}

RE <- function(Pred, Real){
  ss <- sum(abs(Real-Pred))/sum(abs(Real))
  return(ss)}

indices.precision <- function(prediccion, real, cantidad.variables.predictoras){
  return(list(raiz.error.cuadratico = RMSE(prediccion, real), error.estandar.residuos = RSE(prediccion, real, cantidad.variables.predictoras),
              error.absoluto.medio = MAE(prediccion, real),
              error.relativo=RE(prediccion, real),
              correlacion = as.numeric(cor(prediccion, real))))}

plot.real.prediccion <- function(prediccion, real, modelo = "") {
  df <- data.frame(Real = real, Prediccion = prediccion)
  df$Distancia <- abs(df$Real - df$Prediccion)  # Distancia a la diagonal
  
  g <- ggplot(df, aes(x = Real, y = Prediccion)) +
    geom_point(color = "steelblue", size = 2, alpha = 0.8) +  # color fijo
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black", size = 1) +  # línea continua
    labs(
      title = paste0(ifelse(modelo == "", "", paste("with", modelo))),
      x = "Actual",
      y = "Prediction"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"  # ya no hay color por "Distancia"
    )
  
  
  return(g)
}

# Funciones

EDA <- function(set, index) {
  
  na.data <- set %>% filter(if_any(everything(), is.na)) #trabajos con NAs
  num.na <- nrow(na.data) #cantidad de NAs
  
  set <- na.omit(set) #se eliminan los NAs
  
  cero.data <- colSums(set==0) #Variables con valores cero
  
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  f4 <- set %>% filter(TimelimitRaw=="Partition_Limit")
  g0 <- ggplot(f4, aes(x=State))+geom_bar()
  g00 <- ggplot(f4, aes(x=CPUTimeRAW))+geom_bar()
  set <- set %>% filter(CPUTimeRAW!=0)
  
  g1 <- ggplot(set, aes(x=ConsumedEnergyRaw)) + geom_bar(color="black")
  g2 <- ggplot(set, aes(x=CPUTimeRAW)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ReqMem)) + geom_histogram(color="black")
  g4 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=ReqNodes)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g8 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g9 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  g10 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g11 <- ggplot(set, aes(x=Partition)) + geom_bar(color="black")
  g12 <- ggplot(set, aes(x=QOS)) + geom_bar(color="black")
  g13 <- ggplot(set, aes(x=State)) + geom_bar(color="black")
  
  g14 <- ggplot(set, aes(x = State, y = ConsumedEnergyRaw, fill=State)) + geom_boxplot()
  g15 <- ggplot(set, aes(x = State, y = CPUTimeRAW, fill=State)) + geom_boxplot()
  g16 <- ggplot(set, aes(x = State, y = ReqMem, fill=State)) + geom_boxplot() 
  g17 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_count()
  g18 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot()
  g19 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_count()
  g20 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_boxplot()
  
  contingencia_timelimit <- set %>% count(TimelimitRaw, State)
  
  g21 <- ggplot(contingencia_timelimit, aes(x = State, y = TimelimitRaw, fill = n)) + geom_tile() 
  g22 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_count()
  g23 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g24 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_count()
  g25 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g26 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_count()
  g27 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  contingencia_priority <- set %>% count(Priority, State)
  
  g28 <- ggplot(contingencia_priority, aes(x = State, y = Priority, fill = n)) + geom_tile() 
  g29 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g30 <- ggplot(set, aes(x = State, y = QOS, fill=State)) + geom_count()
  
  resumen <- summary(set %>% select(-Partition,-QOS,-State)) #resumen de variables numC)ricas.
  
  archivo.nombre <- paste0("edaset", index, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(Conteo.de.NAs = num.na, 
       Datos.cero = cero.data,
       g0, g00, g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30)
}

EDAc <- function(set, index){
  na.data <- set %>% filter(if_any(everything(), is.na)) #trabajos con NAs
  num.na <- nrow(na.data) #cantidad de NAs
  
  set <- na.omit(set) #se eliminan los NAs
  
  cero.data <- colSums(set==0) #Variables con valores cero
  
  g1 <- ggplot(set, aes(x=ReqMem)) + geom_histogram(binwidth = (2 * IQR(set$ReqMem) / length(set$ReqMem)^(1/3)), color="black")
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ReqNodes)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  g8 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g9 <- ggplot(set, aes(x=Partition)) + geom_bar(color="black")
  g10 <- ggplot(set, aes(x=QOS)) + geom_bar(color="black")
  g11 <- ggplot(set, aes(x=State)) + geom_bar(color="black")
  
  g12 <- ggplot(set, aes(x = State, y = ReqMem, fill=State)) + geom_boxplot() 
  g13 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_count()
  g14 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot()
  g15 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_count()
  g16 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_boxplot()
  
  contingencia_timelimit <- set %>% count(TimelimitRaw, State)
  
  g17 <- ggplot(contingencia_timelimit, aes(x = State, y = TimelimitRaw, fill = n)) + geom_tile() 
  g18 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_count()
  g19 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g20 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_count()
  g21 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g22 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_count()
  g23 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  contingencia_priority <- set %>% count(Priority, State)
  
  g24 <- ggplot(contingencia_priority, aes(x = State, y = Priority, fill = n)) + geom_tile() 
  g25 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g26 <- ggplot(set, aes(x = State, y = QOS, fill=State)) + geom_count()
  
  resumen <- summary(set %>% select(-Partition,-QOS,-State)) #resumen de variables numC)ricas.
  
  archivo.nombre <- paste0("edacset", index, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(Resumen = resumen,
       Conteo.de.NAs = num.na, 
       Datos.cero = cero.data,
       g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26)
  
}

DataMining <- function(set, index) {
  
  set$ConsumedEnergyRaw <- Winsorize(set$ConsumedEnergyRaw)
  set$CPUTimeRAW <- Winsorize(set$CPUTimeRAW)
  set$Priority <- Winsorize(set$Priority)
  set$ReqMem <- Winsorize(set$ReqMem)
  set$ReqCPUS <- Winsorize(set$ReqCPUS)
  set$TimelimitRaw <- Winsorize(set$TimelimitRaw)
  
  g31 <- ggplot(set, aes(x=ConsumedEnergyRaw)) + geom_bar(color="black")
  g32 <- ggplot(set, aes(x=CPUTimeRAW)) + geom_bar(color="black")
  g33 <- ggplot(set, aes(x=ReqMem)) + geom_histogram(color="black")
  g34 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g35 <- ggplot(set, aes(x=ReqNodes)) + geom_bar(color="black")
  g36 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g37 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g38 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g39 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  g40 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g41 <- ggplot(set, aes(x=Partition)) + geom_bar(color="black")
  g42 <- ggplot(set, aes(x=QOS)) + geom_bar(color="black")
  g43 <- ggplot(set, aes(x=State)) + geom_bar(color="black")
  
  g44 <- ggplot(set, aes(x = State, y = ConsumedEnergyRaw, fill=State)) + geom_boxplot()
  g45 <- ggplot(set, aes(x = State, y = CPUTimeRAW, fill=State)) + geom_boxplot()
  g46 <- ggplot(set, aes(x = State, y = ReqMem, fill=State)) + geom_boxplot() 
  g47 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_count()
  g48 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot()
  g49 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_count()
  g50 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_boxplot()
  
  contingencia_timelimit <- set %>% count(TimelimitRaw, State)
  
  g51 <- ggplot(contingencia_timelimit, aes(x = State, y = TimelimitRaw, fill = n)) + geom_tile() 
  g52 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_count()
  g53 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g54 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_count()
  g55 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g56 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_count()
  g57 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  contingencia_priority <- set %>% count(Priority, State)
  
  g58 <- ggplot(contingencia_priority, aes(x = State, y = Priority, fill = n)) + geom_tile() 
  g59 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g60 <- ggplot(set, aes(x = State, y = QOS, fill=State)) + geom_count()

  contingencia_priority <- set %>% count(Partition, State)
  
  g61 <- ggplot(contingencia_priority, aes(x = State, y = Partition, fill = n)) + geom_tile() 
  

  archivo.nombre <- paste0("miningset", index, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(g31,g32,g33,g34,g35,g36,g37,g38,g39,g40,g41,g42,g43,g44,g45,g46,g47,g48,g49,g50,g51,g52,g53,g54,g55,g56,g57,g58,g59,g60,g61)
}

DataMiningc <- function(set, index) {
  
  set$ResvCPURAW <- Winsorize(set$ResvCPURAW)
  set$Priority <- Winsorize(set$Priority)
  set$ReqMem <- Winsorize(set$ReqMem)
  set$ReqCPUS <- Winsorize(set$ReqCPUS)
  #set$ReqNodes <- Winsorize(set$ReqNodes)
  set$TimelimitRaw <- Winsorize(set$TimelimitRaw)
  
  g27 <- ggplot(set, aes(x=ReqMem)) + geom_histogram(binwidth = (2 * IQR(set$ReqMem) / length(set$ReqMem)^(1/3)), color="black")
  g28 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g29 <- ggplot(set, aes(x=ReqNodes)) + geom_bar(color="black")
  g30 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g31 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g32 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g33 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  g34 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g35 <- ggplot(set, aes(x=Partition)) + geom_bar(color="black")
  g36 <- ggplot(set, aes(x=QOS)) + geom_bar(color="black")
  g37 <- ggplot(set, aes(x=State)) + geom_bar(color="black")
  
  g38 <- ggplot(set, aes(x = State, y = ReqMem, fill=State)) + geom_boxplot() 
  g39 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_count()
  g40 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot()
  g41 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_count()
  g42 <- ggplot(set, aes(x = State, y = ReqNodes, fill=State)) + geom_boxplot()
  
  contingencia_timelimit <- set %>% count(TimelimitRaw, State)
  
  g43 <- ggplot(contingencia_timelimit, aes(x = State, y = TimelimitRaw, fill = n)) + geom_tile() 
  g44 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_count()
  g45 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g46 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_count()
  g47 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g48 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_count()
  g49 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  contingencia_priority <- set %>% count(Priority, State)
  
  g50 <- ggplot(contingencia_priority, aes(x = State, y = Priority, fill = n)) + geom_tile() 
  g51 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g52 <- ggplot(set, aes(x = State, y = QOS, fill=State)) + geom_count()
  
  contingencia_priority <- set %>% count(Partition, State)
  
  gp <- ggplot(contingencia_priority, aes(x = State, y = Partition, fill = n)) + geom_tile() 
  

  archivo.nombre <- paste0("miningsetc", index, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(g27,g28,g29,g30,g31,g32,g33,g34,g35,g36,g37,g38,g39,g40,g41,g42,g43,g44,g45,g46,g47,g48,g49,g50,g51,g52,gp)
}

Corrc <- function(set){
ttest1 <- t.test(ReqCPUS ~ State, data = set)
ttest2 <- t.test(ReqMem ~ State, data = set)
ttest3 <- t.test(ReqNodes ~ State, data = set)
ttest4 <- t.test(ResvCPURAW ~ State, data = set)
ttest5 <- t.test(TimelimitRaw ~ State, data = set)
ttest6 <- t.test(Priority ~ State, data = set)
ttest7 <- t.test(SubmitHour ~ State, data = set)
ttest8 <- t.test(SubmitWeekday ~ State, data = set)

g53 <- ggplot(set, aes(x = State, y = ReqCPUS, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g54 <- ggplot(set, aes(x = State, y = ReqMem, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g55 <- ggplot(set, aes(x = State, y = ReqNodes, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g56 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g57 <- ggplot(set, aes(x = State, y = TimelimitRaw, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g58 <- ggplot(set, aes(x = State, y = Priority, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g59 <- ggplot(set, aes(x = State, y = SubmitHour, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g60 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white")

g61 <- ggplot(set, aes(x = ReqCPUS, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g62 <- ggplot(set, aes(x = ReqMem, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g63 <- ggplot(set, aes(x = ReqNodes, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g64 <- ggplot(set, aes(x = ResvCPURAW, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g65 <- ggplot(set, aes(x = TimelimitRaw, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g66 <- ggplot(set, aes(x = Priority, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g67 <- ggplot(set, aes(x = SubmitHour, color = State, fill = State)) +
  geom_density(alpha = 0.3)

g68 <- ggplot(set, aes(x = SubmitWeekday, color = State, fill = State)) +
  geom_density(alpha = 0.3)

#chi1 <- chisq.test(set$State, set$Partition)

chi2 <- chisq.test(set$State, set$QOS)

#VC1 <- assocstats(table(set$State, set$Partition))

VC2 <- assocstats(table(set$State, set$QOS))

list(ttest1,ttest2,ttest3,ttest4,ttest5,ttest6,ttest7,ttest8, chi2, VC2,g53, g54, g55, g56, g57, g58, g59, g60, g61, g62, g63, g64, g65, g66, g67, g68)
}

Corrnum <- function(set) {
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g62 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Pearson')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g63 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  m.correlacion.kendall <- cor(set, method = "kendall")
  g64 <- ggcorrplot(m.correlacion.kendall,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Kendall')

  list(pairs, m.correlacion.pearson, m.correlacion.spearman, m.correlacion.kendall,g62,g63,g64)
  
  }
 
 #Análisis de varianza.

Corrfac <- function(set) {  
  mod1 <- lm(CPUPower ~., set.factor)
  mod1.resumen <- summary(mod1)
  anova.mod1 <- anova(mod1)
  g28 <- plot(anova.mod1)
  
  #archivo.nombre <- paste0("corrset", index, ".csv")
  #write.csv(set, archivo.nombre, row.names = FALSE)
  
  list( 
       mod1.resumen, 
       anova.mod1, 
       g28)
}

Modelsc <-  function(datos) {
  numero.filas          <- nrow(datos)
  cantidad.experimentos <- 5
  
  # Acumuladores de la PrecisiC3n Global para el gráfico de barras
  pg.svm <- 0
  pg.knn <- 0
  pg.bayes <- 0
  pg.arbol <- 0
  pg.bosque <- 0    
  pg.potenciacion <- 0
  pg.red <- 0    
  pg.xgboost <- 0  
  pg.glm <- 0   
  pg.glmnet <- 0 
  
  # Listas para guardar las Precisiones Globales en cada experimento para el gráfico de líneas
  
  lista.pg.svm <- list()
  lista.pg.knn <- list()
  lista.pg.bayes <- list()
  lista.pg.arbol <- list()
  lista.pg.bosque <- list()   
  lista.pg.potenciacion <- list()
  lista.pg.red <- list()   
  lista.pg.xgboost <- list()
  lista.pg.glm <- list() 
  lista.pg.glmnet <- list()
  
  # Acumuladores del NO FAILED para el gráfico de barras
  no.svm <- 0
  no.knn <- 0
  no.bayes <- 0
  no.arbol <- 0
  no.bosque <- 0    
  no.potenciacion <- 0
  no.red <- 0    
  no.xgboost <- 0  
  no.glm <- 0   
  no.glmnet <- 0 
  
  # Listas para guardar lel No FAILED en cada experimento para el gráfico de líneas
  lista.no.svm <- list()
  lista.no.knn <- list()
  lista.no.bayes <- list()
  lista.no.arbol <- list()
  lista.no.bosque <- list()   
  lista.no.potenciacion <- list()
  lista.no.red <- list()   
  lista.no.xgboost <- list()
  lista.no.glm <- list() 
  lista.no.glmnet <- list()
  
  # Acumuladores del NO FAILED para el gráfico de barras
  nf.svm <- 0
  nf.knn <- 0
  nf.bayes <- 0
  nf.arbol <- 0
  nf.bosque <- 0    
  nf.potenciacion <- 0
  nf.red <- 0    
  nf.xgboost <- 0  
  nf.glm <- 0   
  nf.glmnet <- 0 
  
  # Listas para guardar lel No FAILED en cada experimento para el gráfico de líneas
  lista.nf.svm <- list()
  lista.nf.knn <- list()
  lista.nf.bayes <- list()
  lista.nf.arbol <- list()
  lista.nf.bosque <- list()   
  lista.nf.potenciacion <- list()
  lista.nf.red <- list()   
  lista.nf.xgboost <- list()
  lista.nf.glm <- list() 
  lista.nf.glmnet <- list()
  
  for (i in 1:cantidad.experimentos) {
    muestra      <- createDataPartition(y = datos$State, p = 0.85, list = F)
    taprendizaje <- datos[muestra, ]
    ttesting     <- datos[-muestra, ]  
    
    modelo          <- train.svm(State ~ ., data = taprendizaje, kernel = "linear", probability = FALSE)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.svm <- append(lista.pg.svm,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.svm <- pg.svm + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.svm <- append(lista.no.svm,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.svm <- no.svm + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.svm <- append(lista.nf.svm,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.svm <- nf.svm + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    
    modelo          <- train.knn(State ~ ., data = taprendizaje, kmax = 37)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.knn <- append(lista.pg.knn,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.knn <- pg.knn + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.knn <- append(lista.no.knn,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.knn <- no.knn + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.knn <- append(lista.nf.knn,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.knn <- nf.knn + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    
    modelo          <- train.bayes(State ~ ., data = taprendizaje)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.bayes <- append(lista.pg.bayes,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.bayes <- pg.bayes +general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.bayes <- append(lista.no.bayes,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.bayes <- no.bayes + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.bayes <- append(lista.nf.bayes,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.bayes <- nf.bayes + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    
    modelo          <- train.rpart(State ~ ., data = taprendizaje)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.arbol <- append(lista.pg.arbol,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.arbol <- pg.arbol + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.arbol <- append(lista.no.arbol,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.arbol <- no.arbol + general.indexes(ttesting,prediccion)$category.accuracy["Fa"]  
    lista.nf.arbol <- append(lista.nf.arbol,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.arbol <- nf.arbol + general.indexes(ttesting,prediccion)$category.accuracy["NF"]  
    
    modelo          <- train.randomForest(State ~ ., data = taprendizaje)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.bosque <- append(lista.pg.bosque,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.bosque <- pg.bosque + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.bosque <- append(lista.no.bosque,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.bosque <- no.bosque + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.bosque <- append(lista.nf.bosque,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.bosque <- nf.bosque + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    modelo          <- train.ada(State ~ ., data = taprendizaje, iter = 20, nu = 1, type = "discrete")
    prediccion      <- predict(modelo, ttesting)
    lista.pg.potenciacion <- append(lista.pg.potenciacion,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.potenciacion <- pg.potenciacion +general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.potenciacion <- append(lista.no.potenciacion,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.potenciacion <- no.potenciacion + general.indexes(ttesting,prediccion)$category.accuracy["Fa"]
    lista.nf.potenciacion <- append(lista.nf.potenciacion,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.potenciacion <- nf.potenciacion + general.indexes(ttesting,prediccion)$category.accuracy["NF"]
    
    modelo          <- train.nnet(State ~ ., data = taprendizaje, size = 100, MaxNWts = 5000, 
                                  rang = 0.01, decay = 5e-4, maxit = 45, trace = TRUE)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.red    <- append(lista.pg.red,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.red    <- pg.red + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.red    <- append(lista.no.red,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.red    <- no.red + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.red    <- append(lista.nf.red,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.red    <- nf.red + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    modelo          <- train.xgboost(State ~ ., data = taprendizaje, nrounds = 79,
                                     print_every_n = 10, maximize = F , eval_metric = "error",verbose = 0)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.xgboost <- append(lista.pg.xgboost,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.xgboost <- pg.xgboost + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.xgboost <- append(lista.no.xgboost,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.xgboost <- no.xgboost +general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.xgboost <- append(lista.nf.xgboost,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.xgboost <- nf.xgboost +general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    modelo          <- train.glm(State ~ ., data = taprendizaje)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.glm    <- append(lista.pg.glm,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.glm          <- pg.glm + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.glm    <- append(lista.no.glm,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.glm          <- no.glm + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.glm    <- append(lista.nf.glm,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.glm          <- nf.glm + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
    
    modelo          <- train.glmnet(State ~ ., data = taprendizaje, standardize = TRUE)
    prediccion      <- predict(modelo, ttesting)
    lista.pg.glmnet <- append(lista.pg.glmnet,general.indexes(ttesting,prediccion)$overall.accuracy)
    pg.glmnet       <- pg.glmnet + general.indexes(ttesting,prediccion)$overall.accuracy
    lista.no.glmnet <- append(lista.no.glmnet,general.indexes(ttesting,prediccion)$category.accuracy["Fa"])
    no.glmnet       <- no.glmnet + general.indexes(ttesting,prediccion)$category.accuracy["Fa"] 
    lista.nf.glmnet <- append(lista.nf.glmnet,general.indexes(ttesting,prediccion)$category.accuracy["NF"])
    nf.glmnet       <- nf.glmnet + general.indexes(ttesting,prediccion)$category.accuracy["NF"] 
}
  pg.svm <- pg.svm/cantidad.experimentos
  pg.knn <- pg.knn/cantidad.experimentos
  pg.bayes <- pg.bayes/cantidad.experimentos
  pg.arbol <- pg.arbol/cantidad.experimentos
  pg.bosque <- pg.bosque/cantidad.experimentos
  pg.potenciacion <- pg.potenciacion/cantidad.experimentos
  pg.red <- pg.red/cantidad.experimentos
  pg.xgboost <- pg.xgboost/cantidad.experimentos
  pg.glm <- pg.glm/cantidad.experimentos
  pg.glmnet <- pg.glmnet/cantidad.experimentos
  
  no.svm <- no.svm/cantidad.experimentos
  no.knn <- no.knn/cantidad.experimentos
  no.bayes <- no.bayes/cantidad.experimentos
  no.arbol <- no.arbol/cantidad.experimentos
  no.bosque <- no.bosque/cantidad.experimentos
  no.potenciacion <- no.potenciacion/cantidad.experimentos
  no.red <- no.red/cantidad.experimentos
  no.xgboost <- no.xgboost/cantidad.experimentos
  no.glm <- no.glm/cantidad.experimentos
  no.glmnet <- no.glmnet/cantidad.experimentos
 
  nf.svm <- nf.svm/cantidad.experimentos
  nf.knn <- nf.knn/cantidad.experimentos
  nf.bayes <- nf.bayes/cantidad.experimentos
  nf.arbol <- nf.arbol/cantidad.experimentos
  nf.bosque <- nf.bosque/cantidad.experimentos
  nf.potenciacion <- nf.potenciacion/cantidad.experimentos
  nf.red <- nf.red/cantidad.experimentos
  nf.xgboost <- nf.xgboost/cantidad.experimentos
  nf.glm <- nf.glm/cantidad.experimentos
  nf.glmnet <- nf.glmnet/cantidad.experimentos
  
  
  lista.pg <- list(
    SVM = pg.svm, 
    KNN = pg.knn,
    BAYES = pg.bayes,
    ARBOL = pg.arbol,
    BOSQUES = pg.bosque,
    POTENCIACICON = pg.potenciacion,  
    RED = pg.red,
    XGBOOSTING = pg.xgboost,
    GLM = pg.glm,
    GLMNET = pg.glmnet
  )
  
  # Convierte la lista a un data frame
  df.pg <- data.frame(
    metodo = names(lista.pg),
    precision = unlist(lista.pg)
  )
  
  # Define el orden deseado de las barras en el eje x
  df.pg$metodo <- factor(df.pg$metodo, levels = names(lista.pg))
  
  # Asigna colores distintos a cada barra y para cada línea
  colores <- rainbow(length(df.pg$metodo))
  
  # Crear una matriz con las listas para el gráfico de líneas 
  matriz.pg <- cbind(lista.pg.svm,
                     lista.pg.knn,
                     lista.pg.bayes,
                     lista.pg.arbol,
                     lista.pg.bosque,
                     lista.pg.potenciacion,
                     lista.pg.red,
                     lista.pg.xgboost,
                     lista.pg.glm,
                     lista.pg.glmnet)
  
  # Crea el gráfico de líneas
  im1 <- matplot(matriz.pg, type = "l", col = colores, lty = 1, lwd = 2,
          xlab = "IteraciC3n", ylab = "PrecisiC3n Global",
          main = "Gráfico de Líneas para la PrecisiC3n Global")
  
  # Agregar una leyenda al gráfico de líneas
  legend("topright", legend = c("SVM", "KNN", "BAYES", "CRBOL", "BOSQUES", "POTENCIACICN", "RED", "XGBOOSTING", "GLM", "GLMNET"),
         col = colores, lty = 1, lwd = 2, cex = 0.8)
  
  # Crea un gráfico de barras con ggplot2
  im2 <- ggplot(df.pg, aes(x = metodo, y = precision, fill = metodo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(precision)), vjust = -0.5, size = 4) +  
    scale_fill_manual(values = colores) +
    labs(title = "PrecisiC3n Global",
         x = "MC)todo",
         y = "PrecisiC3n Global") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Lista del NO para el gráfico de barras
  lista.no <- list(
    SVM = no.svm, 
    KNN = no.knn,
    BAYES = no.bayes,
    ARBOL = no.arbol,
    BOSQUES = no.bosque,
    POTENCIACICON = no.potenciacion,  
    RED = no.red,
    XGBOOSTING = no.xgboost,
    GLM = no.glm,
    GLMNET = no.glmnet
  )
  
  # Convierte la lista a un data frame
  df.no <- data.frame(
    metodo = names(lista.no),
    no = unlist(lista.no)
  )
  
  # Define el orden deseado de las barras en el eje x
  df.no$metodo <- factor(df.no$metodo, levels = names(lista.no))
  
  # Asigna colores distintos a cada barra y para cada línea
  colores <- rainbow(length(df.no$metodo))
  
  # Crear una matriz con las listas para el gráfico de líneas 
  matriz.no <- cbind(lista.no.svm,
                     lista.no.knn,
                     lista.no.bayes,
                     lista.no.arbol,
                     lista.no.bosque,
                     lista.no.potenciacion,
                     lista.no.red,
                     lista.no.xgboost,
                     lista.no.glm,
                     lista.no.glmnet)
  
  # Crea el gráfico de líneas
  im3 <- matplot(matriz.no, type = "l", col = colores, lty = 1, lwd = 2,
          xlab = "IteraciC3n", ylab = "DetecciC3n del NO",
          main = "Gráfico de Líneas para la detecciC3n del NO")
  
  # Agregar una leyenda al gráfico de líneas
  legend("topright", legend = c("SVM", "KNN", "BAYES", "CRBOL", "BOSQUES", "POTENCIACICN", "RED", "XGBOOSTING", "GLM", "GLMNET"),
         col = colores, lty = 1, lwd = 2, cex = 0.8)
  
  # Crea un gráfico de barras con ggplot2
  im4 <- ggplot(df.no, aes(x = metodo, y = no, fill = metodo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(no)), vjust = -0.5, size = 4) +  
    scale_fill_manual(values = colores) +
    labs(title = "DetecciC3n del NO",
         x = "MC)todo",
         y = "DetecciC3n del NO") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Lista del NF para el gráfico de barras
  lista.nf <- list(
    SVM = nf.svm, 
    KNN = nf.knn,
    BAYES = nf.bayes,
    ARBOL = nf.arbol,
    BOSQUES = nf.bosque,
    POTENCIACICON = nf.potenciacion,  
    RED = nf.red,
    XGBOOSTING = nf.xgboost,
    GLM = nf.glm,
    GLMNET = nf.glmnet
  )
  
  # Convierte la lista a un data frame
  df.nf <- data.frame(
    metodo = names(lista.nf),
    nf = unlist(lista.nf)
  )
  
  # Define el orden deseado de las barras en el eje x
  df.nf$metodo <- factor(df.nf$metodo, levels = names(lista.nf))
  
  # Asigna colores distintos a cada barra y para cada línea
  colores <- rainbow(length(df.nf$metodo))
  
  # Crear una matriz con las listas para el gráfico de líneas 
  matriz.nf <- cbind(lista.nf.svm,
                     lista.nf.knn,
                     lista.nf.bayes,
                     lista.nf.arbol,
                     lista.nf.bosque,
                     lista.nf.potenciacion,
                     lista.nf.red,
                     lista.nf.xgboost,
                     lista.nf.glm,
                     lista.nf.glmnet)
  
  # Crea el gráfico de líneas
  im5 <- matplot(matriz.nf, type = "l", col = colores, lty = 1, lwd = 2,
          xlab = "IteraciC3n", ylab = "DetecciC3n del NF",
          main = "Gráfico de Líneas para la detecciC3n del NF")
  
  # Agregar una leyenda al gráfico de líneas
  legend("topright", legend = c("SVM", "KNN", "BAYES", "CRBOL", "BOSQUES", "POTENCIACICN", "RED", "XGBOOSTING", "GLM", "GLMNET"),
         col = colores, lty = 1, lwd = 2, cex = 0.8)
  
  # Crea un gráfico de barras con ggplot2
  im6 <- ggplot(df.nf, aes(x = metodo, y = nf, fill = metodo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(nf)), vjust = -0.5, size = 4) +  
    scale_fill_manual(values = colores) +
    labs(title = "DetecciC3n del NF",
         x = "MC)todo",
         y = "DetecciC3n del NF") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
list(im1,im2,im3,im4,im5,im6)
}
  
Models <- function(set) {
  
  sample <- createDataPartition(set$CPUPower, p=0.8, list=F)
  ttraining <- set[sample,]
  ttesting <- set[-sample,]
  
  linear.model <- lm(CPUPower~., ttraining)
  time.lm <- system.time({linear.model})
  linear.prediction <- predict(linear.model, ttesting)
  errores1 <- indices.precision(linear.prediction, ttesting$CPUPower, cantidad.variables.predictoras = (ncol(ttesting)-1))
  graf18 <- plot.real.prediccion(linear.prediction, ttesting$CPUPower)
  errores1
  
  knn.model <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "optimal")
  time.knn <- system.time({knn.model})
  knn.prediction <- predict(knn.model, ttesting)
  errores2 <- indices.precision(knn.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf19 <- plot.real.prediccion(knn.prediction$prediction, ttesting$CPUPower)
  errores2
  
  bayes.model <- train.bayes(CPUPower~., ttraining)
  time.bayes <- system.time({bayes.model})
  bayes.prediction <- predict(bayes.model, ttesting)
  errores3 <- indices.precision(bayes.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf20 <- plot.real.prediccion(bayes.prediction$prediction, ttesting$CPUPower)
  errores3
  
  tree.model <- train.rpart(CPUPower~.,ttraining, minsplit = 2)
  time.tree <- system.time({tree.model})
  tree.prediction <- predict(tree.model, ttesting)
  errores4 <- indices.precision(tree.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf21 <- plot.real.prediccion(tree.prediction$prediction, ttesting$CPUPower)
  errores4
  
  forest.model <- train.randomForest(CPUPower~., ttraining, importance = T)
  time.forest <- system.time({forest.model})
  forest.prediction <- predict(forest.model, ttesting)
  errores5 <- indices.precision(forest.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf22 <- plot.real.prediccion(forest.prediction$prediction, ttesting$CPUPower)
  errores5
  
  xgb.model <- train.xgboost(CPUPower~.,ttraining, nrounds = 25, eval_metric = "mlogloss", verbose = F)
  time.xgb <- system.time({xgb.model})
  xgb.prediction <- predict(xgb.model, ttesting)
  errores6 <- indices.precision(xgb.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf23 <- plot.real.prediccion(xgb.prediction$prediction, ttesting$CPUPower)
  errores6
  
  nnet.model <- train.nnet(CPUPower~.,ttraining, size = 5)
  time.nnet <- system.time({nnet.model})
  nnet.prediction <- predict(nnet.model, ttesting)
  errores7 <- indices.precision(nnet.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf24 <- plot.real.prediccion(nnet.prediction$prediction, ttesting$CPUPower)
  errores7
  
  lasso.model <- train.glmnet(CPUPower~.,ttraining, alpha = 1, family = "gaussian", standardize = F)
  time.lasso <- system.time({lasso.model})
  graf25 <- plot(lasso.model,"lambda", label=TRUE)
  x<-model.matrix(CPUPower~.,ttraining)[,-1]
  y<-ttraining$CPUPower
  sal.cv<-cv.glmnet(x,y,alpha=1) 
  mejor.lambda<-log(sal.cv$lambda.min)
  abline(v = mejor.lambda, col="blue", lwd=4, lty=3)
  lasso.prediction <- predict(lasso.model, ttesting)
  errores8 <- indices.precision(lasso.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf26 <- plot.real.prediccion(lasso.prediction$prediction, ttesting$CPUPower)
  errores8
  
  ridge.model <- train.glmnet(CPUPower~.,ttraining, alpha = 0, family = "gaussian", standardize = F)
  time.ridge <- system.time({ridge.model})
  graf27 <- plot(ridge.model,"lambda", label=TRUE)
  ridge.prediction <- predict(ridge.model, ttesting)
  errores9 <- indices.precision(ridge.prediction$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
  graf28 <- plot.real.prediccion(ridge.prediction$prediction, ttesting$CPUPower)
  errores9
  
  lista.error <- list(
    LM = errores1$raiz.error.cuadratico, 
    KNN = errores2$raiz.error.cuadratico,
    Bayes = errores3$raiz.error.cuadratico,
    Arbol = errores4$raiz.error.cuadratico,
    Bosques = errores5$raiz.error.cuadratico,
    XGBoosting = errores6$raiz.error.cuadratico,
    NNET = errores7$raiz.error.cuadratico,
    LASSO = errores8$raiz.error.cuadratico,
    Ridge = errores9$raiz.error.cuadratico 
  )
  
  df.error1 <- data.frame(
    Metodo = names(lista.error),
    Error = unlist(lista.error)
  )
  
  colores <- rainbow(length(df.error1$Metodo))
  
  graf29 <- ggplot(df.error1, aes(x = Metodo, y = Error, fill = Metodo)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colores) +
    labs(title = "Raíz del Error Cuadrático Medio",
         x = "MC)todo",
         y = "Error") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  lista.error1 <- list(
    LM = errores1$correlacion, 
    KNN = errores2$correlacion,
    Bayes = errores3$correlacion,
    Arbol = errores4$correlacion,
    Bosques = errores5$correlacion,
    XGBoosting = errores6$correlacion,
    NNET = errores7$correlacion,
    LASSO = errores8$correlacion,
    Ridge = errores9$correlacion 
  )
  
  df.error2 <- data.frame(
    Metodo = names(lista.error1),
    Error = unlist(lista.error1)
  )
  
  graf30 <- ggplot(df.error2, aes(x = Metodo, y = Error, fill = Metodo)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colores) +
    labs(title = "CorrelaciC3n",
         x = "MC)todo",
         y = "CorrelaciC3n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  list(plots = list(graf18,graf19,graf20,graf21,graf22,graf23,graf24,graf25,graf26,graf27,graf28,graf29,graf30), times = list(time.lm, time.knn, time.bayes, time.tree, time.forest, time.xgb, time.nnet,time.lasso, time.ridge), df.error1, df.error2, errores1, errores2, errores3, errores4, errores5, errores6, errores7, errores8, errores9, knn.model, forest.model, nnet.model)}

Calibrationknn <- function(set){
  numero.filas <- nrow(set)
  cantidad.experimentos <- 10
  error.knn.rectangular <- 0
  error.knn.triangular <- 0
  error.knn.epanechnikov <- 0
  error.knn.biweight <- 0
  error.knn.triweight <- 0
  error.knn.cos <- 0
  error.knn.inv <- 0
  error.knn.gaussian <- 0
  error.knn.optimal<- 0
  
  lista.error.knn.rectangular <- list()
  lista.error.knn.triangular <- list()
  lista.error.knn.epanechnikov <- list()
  lista.error.knn.biweight <- list()
  lista.error.knn.triweight <- list()
  lista.error.knn.cos <- list()
  lista.error.knn.inv <- list()
  lista.error.knn.gaussian <- list()
  lista.error.knn.optimal <- list()
  
  for(i in 1:cantidad.experimentos) {
    
    muestra <- createDataPartition(y = set$CPUPower, p = 0.80, list = F)
    ttraining <- set[muestra, ]
    ttesting <- set[-muestra, ]  
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "rectangular")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.rectangular <- append(lista.error.knn.rectangular,errores$raiz.error.cuadratico)
    error.knn.rectangular <- error.knn.rectangular + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "triangular")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.triangular <- append(lista.error.knn.triangular,errores$raiz.error.cuadratico)
    error.knn.triangular <- error.knn.triangular + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "epanechnikov")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.epanechnikov <- append(lista.error.knn.epanechnikov,errores$raiz.error.cuadratico)
    error.knn.epanechnikov <- error.knn.epanechnikov + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "biweight")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.biweight <- append(lista.error.knn.biweight,errores$raiz.error.cuadratico)
    error.knn.biweight <- error.knn.biweight + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "triweight")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.triweight <- append(lista.error.knn.triweight,errores$raiz.error.cuadratico)
    error.knn.triweight <- error.knn.triweight + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "cos")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.cos <- append(lista.error.knn.cos,errores$raiz.error.cuadratico)
    error.knn.cos <- error.knn.cos + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "inv")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.inv <- append(lista.error.knn.inv,errores$raiz.error.cuadratico)
    error.knn.inv <- error.knn.inv + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "gaussian")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.gaussian <- append(lista.error.knn.gaussian,errores$raiz.error.cuadratico)
    error.knn.gaussian <- error.knn.gaussian + errores$raiz.error.cuadratico
    
    
    modelo <- train.knn(CPUPower~.,ttraining, kmax = floor(sqrt(nrow(ttraining))),kernel = "optimal")
    prediccion <- predict(modelo, ttesting)
    errores <- indices.precision(prediccion$prediction,ttesting$CPUPower, cantidad.variables.predictoras=(ncol(ttesting)-1))
    
    lista.error.knn.optimal <- append(lista.error.knn.optimal,errores$raiz.error.cuadratico)
    error.knn.optimal <- error.knn.optimal + errores$raiz.error.cuadratico
  }
  
  error.knn.rectangular <- error.knn.rectangular/cantidad.experimentos
  
  error.knn.triangular <- error.knn.triangular/cantidad.experimentos
  
  error.knn.epanechnikov <- error.knn.epanechnikov/cantidad.experimentos
  
  error.knn.biweight <- error.knn.biweight/cantidad.experimentos
  
  error.knn.triweight  <- error.knn.triweight /cantidad.experimentos
  
  error.knn.cos <- error.knn.cos/cantidad.experimentos
  
  error.knn.inv <- error.knn.inv/cantidad.experimentos
  
  error.knn.gaussian <- error.knn.gaussian/cantidad.experimentos
  
  error.knn.optimal <- error.knn.optimal/cantidad.experimentos
  
  lista.error <- list(
    RECTANGULAR = error.knn.rectangular, 
    TRIANGULAR = error.knn.triangular,
    EPANECHNIKOV = error.knn.epanechnikov,
    BIWEIGHT = error.knn.biweight,
    TRIWEIGHT = error.knn.triweight,
    COS = error.knn.cos,
    INV = error.knn.inv,
    GAUSSIAN = error.knn.gaussian,
    OPTIMAL = error.knn.optimal
  )
  
  df.error <- data.frame(
    metodo = names(lista.error),
    error = unlist(lista.error)
  )
  
  df.error$metodo <- factor(df.error$metodo, levels = names(lista.error))
  
  colores <- rainbow(length(df.error$metodo))  # escala de grises
  
  matriz.errores <- cbind(
    lista.error.knn.rectangular,
    lista.error.knn.triangular,      lista.error.knn.epanechnikov,
    lista.error.knn.biweight,
    lista.error.knn.triweight,
    lista.error.knn.cos,
    lista.error.knn.inv,
    lista.error.knn.gaussian,
    lista.error.knn.optimal)
  
  a <- matplot(matriz.errores, type = "l", col = colores, lty = 1, lwd = 2,
                    xlab = "Kernel", ylab = "RMSE",
                    main = "RMSE per iteration of KNN with different kernels")
  
  
  legend("topright", legend = c("RECTANGULAR", "TRIANGULAR", "EPANECHNIKOV", "BIWEIGHT", "TRIWEIGHT", "COS", "INV", "GAUSSIAN", "OPTIMAL"), col = colores, lty = 1, lwd = 2, cex = 0.8)

  
  #graf32 <- ggplot(df.error, aes(x = metodo, y = error, fill = metodo)) +
   # geom_bar(stat = "identity") +
    #geom_text(aes(label = round(error, 2)), vjust = -0.7, size = 4) +  # tamaño de texto más pequeño
    #scale_fill_manual(values = colores, guide = "none") +  # sin leyenda
    #labs(title = "RMSE por iteración de KNN con distintos kernels",
     #    x = "Kernel",
      #   y = "RMSE") +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  list(errors = matriz.errores, df.error)
}

Calibration.RF <- function(set){
  cantidad.experimentos <- 10
  ntree_values <- c(10, 20, 50, 100, 200)
  mtry_values <- c(1, floor(sqrt(ncol(set))), floor(ncol(set) / 2), 4)
  
  lista_errores <- list()
  matriz.errores <- matrix(NA, nrow = cantidad.experimentos, ncol = length(ntree_values) * length(mtry_values))
  colnames(matriz.errores) <- paste0("mtry", rep(mtry_values, each = length(ntree_values)), "_ntree", rep(ntree_values, times = length(mtry_values)))
  
  df.error <- data.frame(metodo = character(), error = numeric(), experimento = integer())
  
  best_model <- NULL
  best_error <- Inf
  
  for (i in 1:cantidad.experimentos) {
    muestra <- createDataPartition(y = set$CPUPower, p = 0.80, list = FALSE)
    ttraining <- set[muestra, ]
    ttesting <- set[-muestra, ]
    
    errores_iter <- c()
    
    for (m in mtry_values) {
      for (n in ntree_values) {
        
        modelo <- randomForest(CPUPower ~ ., ttraining, mtry = m, ntree = n, importance = TRUE)
        predicciones <- predict(modelo, ttesting)
        mse_actual <- mean((predicciones - ttesting$CPUPower)^2)
        rmse_actual <- sqrt(mse_actual)
        errores_iter <- c(errores_iter, rmse_actual)
        df.error <- rbind(df.error, data.frame(metodo = paste0("mtry", m, "_ntree", n), error = rmse_actual, experimento = i))
        
        if (mse_actual < best_error) {
          best_error <- mse_actual
          best_model <- modelo
        }
      }
    }
    
    matriz.errores[i, ] <- errores_iter
  }
  
  #colores <- rainbow(ncol(matriz.errores))
  #graf33 <- matplot(matriz.errores, type = "l", col = colores, lty = 1, lwd = 2,
   #                 xlab = "Iteration", ylab = "RMSE",
    #                main = "RMSE per Iteration in Random Forest ")
  
  #legend("topright", legend = colnames(matriz.errores), col = colores, lty = 1, lwd = 2, cex = 0.6)
  
  df.error.summary <- aggregate(error ~ metodo, data = df.error, FUN = mean)
  
  #graf34 <- ggplot(df.error.summary, aes(x = metodo, y = error, fill = metodo)) +
    #geom_bar(stat = "identity") +
    #geom_text(aes(label = round(error, 2)), vjust = -1, size = 2) +
    #scale_fill_grey(start = 0.3, end = 0.9) +
    #labs(title = "RMSE",
     #    x = "Parameters",
      #   y = "RMSE") +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1),
     #     legend.position = "none")  # Esto elimina la leyenda
  
  
  list(errors = matriz.errores, df.error.summary )
  
}

nnet.calibration <- function(set){
  cantidad.experimentos <- 10
  size_values <- c(1, 5, 10, 15)  
  decay_values <- c(0, 0.001, 0.01, 0.1)  
  lista_errores <- list()
  matriz.errores <- matrix(NA, nrow = cantidad.experimentos, ncol = length(size_values) * length(decay_values))
  colnames(matriz.errores) <- paste0("size", rep(size_values, each = length(decay_values)), "_decay", rep(decay_values, times = length(size_values)))
  
  df.error <- data.frame(metodo = character(), error = numeric(), experimento = integer())
  
  best_model <- NULL
  best_error <- Inf
  
  for (i in 1:cantidad.experimentos) {
    sample <- createDataPartition(y = set$CPUPower, p = 0.80, list = FALSE)
    ttraining <- set[sample, ]
    ttesting <- set[-sample, ]
    
    errores_iter <- c()
    
    for (s in size_values) {
      for (d in decay_values) {
        modelo <- nnet(CPUPower ~ ., data = ttraining, size = s, decay = d, linout = TRUE, maxit = 1000, trace = FALSE)
        predicciones <- predict(modelo, ttesting)
        mse_actual <- mean((predicciones - ttesting$CPUPower)^2)
        rmse_actual <- sqrt(mse_actual)
        errores_iter <- c(errores_iter, rmse_actual)
        df.error <- rbind(df.error, data.frame(metodo = paste0("size", s, "_decay", d), error = rmse_actual, experimento = i))
        
        if (mse_actual < best_error) {
          best_error <- mse_actual
          best_model <- modelo
        }
      }
    }
    
    matriz.errores[i, ] <- errores_iter
  }
  
  colores <- rainbow(ncol(matriz.errores))
  graf35 <- matplot(matriz.errores, type = "l", col = colores, lty = 1, lwd = 2,
                    xlab = "IteraciC3n", ylab = "Error",
                    main = "Gráfico de Líneas para Errores de Redes Neuronales")
  legend("topright", legend = colnames(matriz.errores), col = colores, lty = 1, lwd = 2, cex = 0.6)
  
  df.error.summary <- aggregate(error ~ metodo, data = df.error, FUN = mean)
  
  graf36 <- ggplot(df.error.summary, aes(x = metodo, y = error, fill = metodo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(error, 2)), vjust = -0.5, size = 6) +  
    scale_fill_manual(values = colores) +
    labs(title = "Raíz del Error Cuadrático Medio (Promedio de Experimentos)",
         x = "MC)todo (CombinaciC3n de parámetros)",
         y = "Error (RMSE)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  list(plots = list(graf35, graf36), errors = matriz.errores)
}

Cross.knn <- function(datos){
  numero.filas <- nrow(datos)
  cantidad.validacion.cruzada <- 5   # Es lo mismo que cantidad.experimentos <- 5
  cantidad.grupos <- 10
  
  # Acumuladores del error
  error.knn.rectangular.total <- 0
  error.knn.triangular.total <- 0
  error.knn.epanechnikov.total <- 0
  error.knn.biweight.total <- 0
  error.knn.triweight.total <- 0
  error.knn.cos.total <- 0
  error.knn.inv.total <- 0
  error.knn.gaussian.total <- 0
  error.knn.optimal.total <- 0
  
  # Listas para guardar los errores en cada experimento para el gráfico de líneas
  lista.error.knn.rectangular    <- list()
  lista.error.knn.triangular   <- list()
  lista.error.knn.epanechnikov <- list()
  lista.error.knn.biweight   <- list()
  lista.error.knn.triweight   <- list()
  lista.error.knn.cos   <- list()
  lista.error.knn.inv   <- list()
  lista.error.knn.gaussian  <- list()
  lista.error.knn.optimal <- list()
  
  for(i in 1:cantidad.validacion.cruzada) {
    # Acumuladores del error
    error.knn.rectangular <- 0
    error.knn.triangular <- 0
    error.knn.epanechnikov <- 0
    error.knn.biweight <- 0
    error.knn.triweight <- 0
    error.knn.cos <- 0
    error.knn.inv <- 0
    error.knn.gaussian <- 0
    error.knn.optimal <- 0
    
    grupos <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
    # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
    # grupos (Folds)
    for(k in 1:cantidad.grupos) {  
      muestra       <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
      ttesting      <- datos[muestra, ]
      ttraining  <- datos[-muestra, ]
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "rectangular")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.rectangular  <- error.knn.rectangular + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "triangular")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.triangular <- error.knn.triangular + errores$raiz.error.cuadratico
    
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "epanechnikov")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.epanechnikov <- error.knn.epanechnikov + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "biweight")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.biweight <- error.knn.biweight + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "triweight")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.triweight <- error.knn.triweight + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "cos")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.cos <- error.knn.cos + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "inv")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.inv <- error.knn.inv + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "gaussian")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.gaussian <- error.knn.gaussian + errores$raiz.error.cuadratico
      
      modelo     <- train.knn(CPUPower ~., ttraining, kmax = floor(sqrt(nrow(ttraining))), kernel = "optimal")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn.optimal <- error.knn.optimal + errores$raiz.error.cuadratico
   
    }
    
    error.knn.rectangular <- error.knn.rectangular/cantidad.grupos
    error.knn.triangular <- error.knn.triangular/cantidad.grupos
    error.knn.epanechnikov <- error.knn.epanechnikov/cantidad.grupos
    error.knn.biweight  <- error.knn.biweight /cantidad.grupos
    error.knn.triweight <- error.knn.triweight/cantidad.grupos
    error.knn.cos <- error.knn.cos/cantidad.grupos
    error.knn.inv <- error.knn.inv/cantidad.grupos
    error.knn.gaussian <- error.knn.gaussian/cantidad.grupos
    error.knn.optimal <- error.knn.optimal/cantidad.grupos
    
    error.knn.rectangular.total <- error.knn.rectangular.total + error.knn.rectangular
    error.knn.triangular.total <- error.knn.triangular.total + error.knn.triangular
    error.knn.epanechnikov.total <- error.knn.epanechnikov.total + error.knn.epanechnikov
    error.knn.biweight.total <- error.knn.biweight.total + error.knn.biweight
    error.knn.triweight.total <- error.knn.triweight.total + error.knn.triweight
    error.knn.cos.total <- error.knn.cos.total + error.knn.cos
    error.knn.inv.total <- error.knn.inv.total + error.knn.inv
    error.knn.gaussian.total <- error.knn.gaussian.total + error.knn.gaussian
    error.knn.optimal.total <- error.knn.optimal.total + error.knn.optimal
    
    lista.error.knn.rectangular <- append(lista.error.knn.rectangular,error.knn.rectangular)  
    lista.error.knn.triangular <- append(lista.error.knn.triangular,error.knn.triangular)  
    lista.error.knn.epanechnikov <- append(lista.error.knn.epanechnikov,error.knn.epanechnikov)  
    lista.error.knn.biweight <- append(lista.error.knn.biweight,error.knn.biweight)  
    lista.error.knn.triweight <- append(lista.error.knn.triweight,error.knn.triweight)  
    lista.error.knn.cos <- append(lista.error.knn.cos,error.knn.cos)  
    lista.error.knn.inv <- append(lista.error.knn.inv, error.knn.inv)  
    lista.error.knn.gaussian <- append(lista.error.knn.gaussian,error.knn.gaussian)  
    lista.error.knn.optimal <- append(lista.error.knn.optimal,error.knn.optimal)
  } 
    error.knn.rectangular.total <- error.knn.rectangular.total/cantidad.validacion.cruzada
    error.knn.triangular.total <- error.knn.triangular.total/cantidad.validacion.cruzada
    error.knn.epanechnikov.total <- error.knn.epanechnikov.total/cantidad.validacion.cruzada
    error.knn.biweight.total <- error.knn.biweight.total/cantidad.validacion.cruzada
    error.knn.triweight.total <- error.knn.triweight.total/cantidad.validacion.cruzada
    error.knn.cos.total <- error.knn.cos.total/cantidad.validacion.cruzada
    error.knn.inv.total <- error.knn.inv.total/cantidad.validacion.cruzada
    error.knn.gaussian.total <- error.knn.gaussian.total/cantidad.validacion.cruzada  
    error.knn.optimal.total <- error.knn.optimal.total/cantidad.validacion.cruzada

    lista.error <- list(
      RECTANGULAR = error.knn.rectangular.total, 
      TRIANGULAR = error.knn.triangular.total,
      EPANECHNIKOV = error.knn.epanechnikov.total,
      BIWEIGHT = error.knn.biweight.total,
      TRIWEIGHT = error.knn.triweight.total,
      COS = error.knn.cos.total,
      INV = error.knn.inv.total,
      GAUSSIAN = error.knn.gaussian.total,
      OPTIMAL = error.knn.optimal.total
    )
    
  
    # Convierte la lista a un data frame
    df.error <- data.frame(
      metodo = names(lista.error),
      error = unlist(lista.error)
    )
    
    # Define el orden deseado de las barras en el eje x
    df.error$metodo <- factor(df.error$metodo, levels = names(lista.error))
    
    # Asigna colores distintos a cada barra y para el líneas
    colores <- rainbow(length(df.error$metodo))
    
    # Crear una matriz con las listas para el gráfico de líneas 
    matriz.errores <- cbind(lista.error.knn.rectangular,
                            lista.error.knn.triangular,
                            lista.error.knn.epanechnikov,
                            lista.error.knn.biweight,
                            lista.error.knn.triweight,
                            lista.error.knn.cos,
                            lista.error.knn.inv,
                            lista.error.knn.gaussian,
                            lista.error.knn.optimal)
    
    # Crea el gráfico de líneas
    a <- matplot(matriz.errores, type = "l", col = colores, lty = 1, lwd = 2,
            xlab = "Iteration", ylab = "RMSE",
            main = "RMSE per iteration of KNN with different kernels")
    
    # Agregar una leyenda al gráfico de líneas
    legend("topright", legend = c("RECTANGULAR", "TRIANGULAR", "EPANECHNIKOV", "BIWEIGHT", "TRIWEIGHT", "COS", "INV", "GAUSSIAN", "OPTIMAL"),
           col = colores, lty = 1, lwd = 2, cex = 0.8)
    
    
    # Crea un gráfico de barras con ggplot2
    b<- ggplot(df.error, aes(x = metodo, y = error, fill = metodo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(error,2)), vjust = -0.5, size = 6) +  
      scale_fill_manual(values = colores) +
      labs(title = "RMSE per iteration of KNN with different kernels",
           x = "Kernel",
           y = "RMSE") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    list(a, b)
  }
  
CrossMet <- function(datos){
  numero.filas          <- nrow(datos)
  cantidad.validacion.cruzada <- 5   # Es lo mismo que cantidad.experimentos <- 5
  cantidad.grupos             <- 10
  # Acumuladores del error
  error.svm.total <- 0
  error.knn.total <- 0
  error.bayes.total <- 0
  error.arbol.total <- 0
  error.bosques.total <- 0
  error.nnet.total <- 0
  error.xgboosting.total <- 0
  error.lm.total <- 0
  error.lasso.total <- 0
  error.ridge.total <- 0  
  
  # Listas para guardar los errores en cada experimento para el gráfico de líneas
  lista.error.svm    <- list()
  lista.error.knn    <- list()
  lista.error.bayes <- list()
  lista.error.arbol <- list()
  lista.error.bosques   <- list()
  lista.error.nnet    <- list()
  lista.error.xgboosting <- list()
  lista.error.lm <- list()
  lista.error.lasso <- list()
  lista.error.ridge <- list()
  
  for(i in 1:cantidad.validacion.cruzada) {
    # Acumuladores del error
    error.svm <- 0
    error.knn <- 0
    error.bayes <- 0
    error.arbol <- 0
    error.bosques <- 0
    error.nnet <- 0
    error.xgboosting <- 0
    error.lm <- 0
    error.lasso <- 0
    error.ridge <- 0  
    grupos <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
    # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
    # grupos (Folds)
    for(k in 1:cantidad.grupos) {  
      muestra       <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
      ttesting      <- datos[muestra, ]
      taprendizaje  <- datos[-muestra, ]
      
      modelo      <- train.svm(CPUPower~., data = taprendizaje, kernel = "radial")
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.svm   <- error.svm + errores$raiz.error.cuadratico
      
      modelo      <- train.knn(CPUPower~., data = taprendizaje, kmax = 37)
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.knn   <- error.knn + errores$raiz.error.cuadratico
      
      modelo      <- train.bayes(CPUPower~., data = taprendizaje)
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.bayes <- error.bayes + errores$raiz.error.cuadratico  
      
      modelo      <- train.rpart(CPUPower~., data = taprendizaje)
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.arbol <- error.arbol + errores$raiz.error.cuadratico  
      
      modelo      <- train.randomForest(CPUPower~., data = taprendizaje)
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.bosques <- error.bosques + errores$raiz.error.cuadratico  
      
      modelo     <- train.nnet(CPUPower~., data = taprendizaje, size = 100, MaxNWts = 5000, 
                               rang = 0.01, decay = 5e-4, maxit = 45, trace = TRUE)
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.nnet <- error.nnet + errores$raiz.error.cuadratico   
      
      modelo      <- train.xgboost(CPUPower~., data = taprendizaje, nrounds = 79,
                                   print_every_n = 10, maximize = F , eval_metric = "error",verbose = 0)
      prediccion  <- predict(modelo, ttesting)
      errores     <- indices.precision(prediccion$prediction,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.xgboosting <- error.xgboosting + errores$raiz.error.cuadratico 
      
      modelo <- lm(CPUPower~., data = taprendizaje)
      prediccion <- predict(modelo, ttesting)
      errores <- indices.precision(prediccion,ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.lm <- error.lm + errores$raiz.error.cuadratico
      
      modelo     <- train.glmnet(CPUPower~.,data = taprendizaje, alpha = 1, family = "gaussian")
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.lasso <- error.lasso + errores$raiz.error.cuadratico
      
      modelo     <- train.glmnet(CPUPower~.,data = taprendizaje, alpha = 0, family = "gaussian") 
      prediccion <- predict(modelo, ttesting)
      errores    <- indices.precision(prediccion$prediction, ttesting$CPUPower,cantidad.variables.predictoras=(ncol(ttesting)-1))
      error.ridge <- error.ridge + errores$raiz.error.cuadratico
    }
    error.svm <- error.svm/cantidad.grupos
    error.knn <- error.knn/cantidad.grupos
    error.bayes <- error.bayes/cantidad.grupos
    error.arbol <- error.arbol/cantidad.grupos
    error.bosques <- error.bosques/cantidad.grupos
    error.nnet <- error.nnet/cantidad.grupos
    error.xgboosting <- error.xgboosting/cantidad.grupos
    error.lm <- error.lm/cantidad.grupos
    error.lasso <- error.lasso/cantidad.grupos
    error.ridge <- error.ridge/cantidad.grupos
    
    error.svm.total <- error.svm.total + error.svm
    error.knn.total <- error.knn.total + error.knn
    error.bayes.total <- error.bayes.total + error.bayes
    error.arbol.total <- error.arbol.total + error.arbol
    error.bosques.total <- error.bosques.total + error.bosques
    error.nnet.total <- error.nnet.total + error.nnet
    error.xgboosting.total <- error.xgboosting.total + error.xgboosting
    error.lm.total <- error.lm.total + error.lm
    error.lasso.total <- error.lasso.total + error.lasso
    error.ridge.total <- error.ridge.total + error.ridge  
    
    lista.error.svm <- append(lista.error.svm,error.svm)  
    lista.error.knn <- append(lista.error.knn,error.knn) 
    lista.error.bayes <- append(lista.error.bayes,error.bayes) 
    lista.error.arbol <- append(lista.error.arbol,error.arbol) 
    lista.error.bosques <- append(lista.error.bosques,error.bosques) 
    lista.error.nnet <- append(lista.error.nnet,error.nnet) 
    lista.error.xgboosting <- append(lista.error.xgboosting,error.xgboosting) 
    lista.error.lm <- append(lista.error.lm,error.lm)
    lista.error.lasso <- append(lista.error.lasso,error.lasso) 
    lista.error.ridge <- append(lista.error.ridge,error.ridge) 
  }  
  error.svm.total <- error.svm.total/cantidad.validacion.cruzada
  error.knn.total <- error.knn.total/cantidad.validacion.cruzada
  error.bayes.total <- error.bayes.total/cantidad.validacion.cruzada
  error.arbol.total <- error.arbol.total/cantidad.validacion.cruzada
  error.bosques.total <- error.bosques.total/cantidad.validacion.cruzada
  error.nnet.total <- error.nnet.total/cantidad.validacion.cruzada
  error.xgboosting.total <- error.xgboosting.total/cantidad.validacion.cruzada
  error.lm.total <- error.lm.total/cantidad.validacion.cruzada
  error.lasso.total <- error.lasso.total/cantidad.validacion.cruzada
  error.ridge.total <- error.ridge.total/cantidad.validacion.cruzada
  
  # Lista de errores para el gráfico de barras
  lista.error <- list(
    SVM = error.svm, 
    KNN = error.knn,
    NB = error.bayes,
    DT = error.arbol,
    RF = error.bosques,
    NNET = error.nnet,
    XGBoost = error.xgboosting,
    LM = error.lm,
    LASSO = error.lasso,
    Ridge = error.ridge
  )
  
  # Convierte la lista a un data frame
  df.error <- data.frame(
    metodo = names(lista.error),
    error = unlist(lista.error)
  )
  
  # Define el orden deseado de las barras en el eje x
  df.error$metodo <- factor(df.error$metodo, levels = names(lista.error))
  
  # Asigna colores distintos a cada barra y para cada línea
  colores <- grey.colors(length(df.error$metodo))
  
  
  # Crear una matriz con las listas para el gráfico de líneas 
  matriz.errores <- cbind(lista.error.svm,
                          lista.error.knn,
                          lista.error.bayes,
                          lista.error.arbol,
                          lista.error.bosques,
                          lista.error.nnet,
                          lista.error.xgboosting,
                          lista.error.lm,
                          lista.error.lasso,
                          error.ridge)
  
  # Crea el gráfico de líneas
  b <- matplot(matriz.errores, type = "l", col = colores, lty = 1, lwd = 2,
          xlab = "Iteration", ylab = "RMSE",
          main = "Line Plot to Compare RMSE per Iteration Across Predictive Models")
  
  # Agregar una leyenda al gráfico de líneas
  legend("topright", legend = c("SVM", "KNN", "NB", "DT", "RF", "NNET", "XGBoost", "LM", "LASSO", "Ridge"),
         col = colores, lty = 1, lwd = 2, cex = 0.8)
  
  # Crea un gráfico de barras con ggplot2
  a <- ggplot(df.error, aes(x = metodo, y = error, fill = metodo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(error,2)), vjust = -0.5, size = 6) +  
    scale_fill_manual(values = colores) +
    labs(title = "RMSE",
         x = "Predictive model",
         y = "RMSE") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")  # Elimina la leyenda
  
  
list(a,b)  
}

# Funciones específicas para el análisis exploratorio por partición.

EDAandalan <- function(set, indice) {
  
  na.data <- set %>% filter(if_any(everything(), is.na))
  num.na <- nrow(na.data)
  
  g0 <- ggplot(na.data, aes(x = State, fill = State)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "State of NAs", 
         x = "State", 
         y = "Count") +
    scale_fill_discrete(name = "State")
  
  set <- na.omit(set)
  
  cero.data <- colSums(set==0)
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  set <- set %>% filter(CPUTimeRAW!=0) #Se eliminan los datos ceros.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  set <- function2(set, "ConsumedEnergyRaw", "CPUTimeRAW") #Se crea CPUPower 
  
  set$TimelimitRaw <- as.numeric(set$TimelimitRaw)
  set$TimelimitRaw <- set$TimelimitRaw*60
  
  g1 <- ggplot(set, aes(x = CPUPower)) + geom_bar(color="black") 
  #ggplot(set, aes(x=CPUTimeRAW)) + geom_bar(color="black", fill="blue")
  #ggplot(set, aes(x=ConsumedEnergyRaw)) + geom_bar(color="black", fill="blue")
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  g77 <- ggplot(set, aes(x=Submit)) + geom_bar(color="black")
  g8 <- ggplot(set, aes(x = State, y = CPUPower, fill=State)) + geom_boxplot()
  g81 <- ggplot(set, aes(y = CPUPower)) + geom_boxplot()
  #ggplot(set, aes(x = State, y = CPUTimeRAW, fill=State)) + geom_boxplot()
  #ggplot(set, aes(x = State, y = ConsumedEnergyRaw, fill=State)) + geom_boxplot()
  g9 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot() 
  g10 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g11 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g12 <- ggplot(set, aes(x = State, y = TimelimitRaw)) + geom_boxplot(color="black")
  g13 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g14 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  g1414 <- ggplot(set, aes(x = State, y = Submit, fill=State)) + geom_boxplot()   
  archivo.nombre <- paste0("edaset", indice, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(cantidad.na = num.na, datoscero = cero.data, info = list(Trabajos.sin.consumo.de.energia.ni.tiempo = info1, Trabajos.sin.tiempo.pero.con.consumo.de.energia = info2, Trabajos.sin.reserva.de.recursos =info3), 
       plots = list(g0, g1, g2, g3, g4, g5, g6, g7, g77, g8, g81, g9, g10, g11, g12, g13, g14, g1414))
}

EDAdribe <- function(set, indice) {
  
  na.data <- set %>% filter(if_any(everything(), is.na))
  num.na <- nrow(na.data)
  
  g0 <- ggplot(na.data, aes(x = State, fill = State)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "State of NAs", 
         x = "State", 
         y = "Count") +
    scale_fill_discrete(name = "State")
  
  set <- na.omit(set)
  
  cero.data <- colSums(set==0)
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  set <- set %>% filter(CPUTimeRAW!=0) #Se eliminan los datos ceros.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  set <- function2(set, "ConsumedEnergyRaw", "CPUTimeRAW") #Se crea CPUPower 
  
  set$TimelimitRaw <- as.numeric(set$TimelimitRaw)
  set$TimelimitRaw <- set$TimelimitRaw*60
  
  set$ReqMem <- sapply(set$ReqMem, function1)
  
  g1 <- ggplot(set, aes(x = CPUPower)) + geom_bar(color="black") 
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  
  g8 <- ggplot(set, aes(x = State, y = CPUPower, fill=State)) + geom_boxplot()
  g9 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot() 
  g10 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g11 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g12 <- ggplot(set, aes(x = State, y = TimelimitRaw)) + geom_boxplot(color="black")
  g13 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g14 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  archivo.nombre <- paste0("edaset", indice, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(cantidad.na = num.na, datoscero = cero.data, info = list(Trabajos.sin.consumo.de.energia.ni.tiempo = info1, Trabajos.sin.tiempo.pero.con.consumo.de.energia = info2, Trabajos.sin.reserva.de.recursos =info3), 
       plots = list(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14))
}

EDAkura <- function(set, indice) {
  
  na.data <- set %>% filter(if_any(everything(), is.na))
  num.na <- nrow(na.data)
  
  g0 <- ggplot(na.data, aes(x = State, fill = State)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "State of NAs", 
         x = "State", 
         y = "Count") +
    scale_fill_discrete(name = "State")
  
  set <- na.omit(set)
  
  cero.data <- colSums(set==0)
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  set <- set %>% filter(CPUTimeRAW!=0) #Se eliminan los datos ceros.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  set <- function2(set, "ConsumedEnergyRaw", "CPUTimeRAW") #Se crea CPUPower 
  
  set$TimelimitRaw <- as.numeric(set$TimelimitRaw)
  set$TimelimitRaw <- set$TimelimitRaw*60
  
  set$ReqMem <- sapply(set$ReqMem, function1)
  
  g1 <- ggplot(set, aes(x = CPUPower)) + geom_bar(color="black") 
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  
  g8 <- ggplot(set, aes(x = State, y = CPUPower, fill=State)) + geom_boxplot()
  g9 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot() 
  g10 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g11 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g12 <- ggplot(set, aes(x = State, y = TimelimitRaw)) + geom_boxplot(color="black")
  g13 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g14 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  archivo.nombre <- paste0("edaset", indice, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(cantidad.na = num.na, datoscero = cero.data, info = list(Trabajos.sin.consumo.de.energia.ni.tiempo = info1, Trabajos.sin.tiempo.pero.con.consumo.de.energia = info2, Trabajos.sin.reserva.de.recursos =info3), 
       plots = list(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14))
}

EDAnu <- function(set, indice) {
  
  na.data <- set %>% filter(if_any(everything(), is.na))
  num.na <- nrow(na.data)
  
  g0 <- ggplot(na.data, aes(x = State, fill = State)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "State of NAs", 
         x = "State", 
         y = "Count") +
    scale_fill_discrete(name = "State")
  
  set <- na.omit(set)
  
  cero.data <- colSums(set==0)
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  set <- set %>% filter(CPUTimeRAW!=0) #Se eliminan los datos ceros.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  set <- function2(set, "ConsumedEnergyRaw", "CPUTimeRAW") #Se crea CPUPower 
  
  set$TimelimitRaw <- as.numeric(set$TimelimitRaw)
  set$TimelimitRaw <- set$TimelimitRaw*60
  
  set$ReqMem <- sapply(set$ReqMem, function1)
  
  g1 <- ggplot(set, aes(x = CPUPower)) + geom_bar(color="black") 
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  
  g8 <- ggplot(set, aes(x = State, y = CPUPower, fill=State)) + geom_boxplot()
  g9 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot() 
  g10 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g11 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g12 <- ggplot(set, aes(x = State, y = TimelimitRaw)) + geom_boxplot(color="black")
  g13 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g14 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  archivo.nombre <- paste0("edaset", indice, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(cantidad.na = num.na, datoscero = cero.data, info = list(Trabajos.sin.consumo.de.energia.ni.tiempo = info1, Trabajos.sin.tiempo.pero.con.consumo.de.energia = info2, Trabajos.sin.reserva.de.recursos =info3), 
       plots = list(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14))
}

EDAnukwa <- function(set, indice) {
  
  na.data <- set %>% filter(if_any(everything(), is.na))
  num.na <- nrow(na.data)
  
  g0 <- ggplot(na.data, aes(x = State, fill = State)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "State of NAs", 
         x = "State", 
         y = "Count") +
    scale_fill_discrete(name = "State")
  
  set <- na.omit(set)
  
  cero.data <- colSums(set==0)
  f1 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw==0)
  info1 <- summary(factor(f1$State)) #Trabajos sin consumo de energía ni tiempo.
  f2 <- set %>% filter(CPUTimeRAW==0 & ConsumedEnergyRaw!=0)
  info2 <- summary(factor(f2$State)) #Trabajos sin tiempo, pero con consumo de energía.
  set <- set %>% filter(CPUTimeRAW!=0) #Se eliminan los datos ceros.
  f3 <- set %>% filter(ResvCPURAW==0)
  info3 <- summary(factor(f3$State)) #Trabajos sin reserva de recursos.
  
  set <- function2(set, "ConsumedEnergyRaw", "CPUTimeRAW") #Se crea CPUPower 
  
  set$TimelimitRaw <- as.numeric(set$TimelimitRaw)
  set$TimelimitRaw <- set$TimelimitRaw*60
  
  set$ReqMem <- sapply(set$ReqMem, function1)
  
  g1 <- ggplot(set, aes(x = CPUPower)) + geom_bar(color="black") 
  g2 <- ggplot(set, aes(x=ReqCPUS)) + geom_bar(color="black")
  g3 <- ggplot(set, aes(x=ResvCPURAW)) + geom_bar(color="black")
  g4 <- ggplot(set, aes(x=Priority)) + geom_bar(color="black")
  g5 <- ggplot(set, aes(x=TimelimitRaw)) + geom_bar(color="black")
  g6 <- ggplot(set, aes(x=SubmitHour)) + geom_bar(color="black")
  g7 <- ggplot(set, aes(x=SubmitWeekday)) + geom_bar(color="black")
  
  g8 <- ggplot(set, aes(x = State, y = CPUPower, fill=State)) + geom_boxplot()
  g9 <- ggplot(set, aes(x = State, y = ReqCPUS, fill=State)) + geom_boxplot() 
  g10 <- ggplot(set, aes(x = State, y = ResvCPURAW, fill=State)) + geom_boxplot()
  g11 <- ggplot(set, aes(x = State, y = Priority, fill=State)) + geom_boxplot()
  g12 <- ggplot(set, aes(x = State, y = TimelimitRaw)) + geom_boxplot(color="black")
  g13 <- ggplot(set, aes(x = State, y = SubmitHour, fill=State)) + geom_boxplot()
  g14 <- ggplot(set, aes(x = State, y = SubmitWeekday, fill=State)) + geom_boxplot()
  
  archivo.nombre <- paste0("edaset", indice, ".csv")
  write.csv(set, archivo.nombre, row.names = FALSE)
  
  list(cantidad.na = num.na, datoscero = cero.data, info = list(Trabajos.sin.consumo.de.energia.ni.tiempo = info1, Trabajos.sin.tiempo.pero.con.consumo.de.energia = info2, Trabajos.sin.reserva.de.recursos =info3), 
       plots = list(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14))
}

# Funciones específicas para correlación por partición.

Corrandalan <- function(set) {
  
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g15 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Pearson correlation')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g16 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  list(pairs, m.correlacion.pearson, m.correlacion.spearman, g15, g16)
}

Corrdribe<- function(set) {
  
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g15 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Pearson correlation')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g16 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  list(pairs, m.correlacion.pearson, m.correlacion.spearman, g15, g16)
}

Corrkura <- function(set) {
  
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g15 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Pearson correlation')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g16 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  list(pairs, m.correlacion.pearson, m.correlacion.spearman, g15, g16)
}

Corrnu <- function(set) {
  
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g15 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Pearson correlation')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g16 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  list(pairs, m.correlacion.pearson, m.correlacion.spearman, g15, g16)
}

Corrnukwa <- function(set) {
  
  
  pairs <- ggpairs(set)
  
  m.correlacion.pearson <- cor(set)
  g15 <- ggcorrplot(m.correlacion.pearson,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Pearson correlation')
  
  m.correlacion.spearman <- cor(set, method = "spearman")
  g16 <- ggcorrplot(m.correlacion.spearman,show.diag = F,type='lower', lab = TRUE, lab_size=3) + scale_fill_gradient2(low='red',high='blue',breaks=c(-1, -0.5,0,0.5,1),limit=c(-1,1),name='Correlation Spearman')
  
  list(pairs, m.correlacion.pearson, m.correlacion.spearman, g15, g16)
}
