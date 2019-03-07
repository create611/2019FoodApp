prod_generator = function(area, xt, xr, xs, mt=60, mr=40, ms=60, sigt=20, sigr=16, sigs=15, sigerr=1, A, Aerr){
  prod = area*A*pnorm(xt, mean = mt, sd = sigt)*
    pnorm(xs, mean = ms, sd = sigs)*
    pnorm(xr, mean = mr, sd = sigr)+
    Aerr*rnorm(length(xt), mean = 0, sd = sigerr);
  return(prod);
}

randomizer = function(nyear, m, sig){
  return(rnorm(length(m)*nyear, mean = m, sd = sig));
}

data = read.csv("corn.csv")


rain = randomizer(100, data$precipitation, sig = 0.05*data$precipitation);
sun  = randomizer(100, data$sunshine, sig = 0.05*data$sunshine);
temp = randomizer(100, data$temperature, sig = 0.05*data$temperature);
area = randomizer(100, data$Harvested.Acres, sig = 0.005*data$Harvested.Acres);
prod = randomizer(100, data$production, sig = 0.005*data$production)


MLdata = data.frame(prod,rain,sun,temp,area)
write.csv(MLdata, file = "tr.csv")

rain = randomizer(10, data$precipitation, sig = 0.05*data$precipitation);
sun  = randomizer(10, data$sunshine, sig = 0.05*data$sunshine);
temp = randomizer(10, data$temperature, sig = 0.05*data$temperature);
area = randomizer(10, data$Harvested.Acres, sig = 0.005*data$Harvested.Acres);
prod = randomizer(10, data$production, sig = 0.005*data$production)

MLdata = data.frame(prod,rain,sun,temp,area)
write.csv(MLdata, file = "td.csv")