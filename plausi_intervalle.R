
#### Plausibilitätsytest der Intervall Funktion ####

for(i in c(2 : 6)) {
  print(i)
  print(all(pred.interval$o_intervall[, i] > pred.interval$u_intervall[, i]))
  print(all(pred.interval$mean[, i] < pred.interval$o_intervall[, i]) && all(pred.interval$mean[, i] > pred.interval$u_intervall[, i]))
  print(all(pred.interval$median[, i] < pred.interval$o_intervall[, i]) && all(pred.interval$median[, i] > pred.interval$u_intervall[, i]))
}

