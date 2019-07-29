test <- data_rain


head(test)

which(is.na(test$Rainfall.amount..millimetres.))



test1 <- test[220:251,]



for (i in 1:test1$Rainfall.amount..millimetres.) {
  if(is.na(i)) {
    print()
  }
}





for (i in 1:length(test1$Rainfall.amount..millimetres.)) {
  if(is.na(test1$Rainfall.amount..millimetres.[i])) {
    print(test1$Rainfall.amount..millimetres.[i])
  } else {
    print(test1$Rainfall.amount..millimetres.[i + 1])
  }
}
