# Noah Gundotra, Maya Mysore, Srijani Saha
# Perturbing voltage simulation

simulateLIF = function(milliseconds,
                       perturb,            # matrix of times and amt voltage
                       absRefractory = 5,
                       stimDefault = 20
                       ) { 
  # returns voltage for both cells... (unperturbed comes first)
  # iter            - time (no units)
  # absRefractory   - time of refractory (no units)
  iter = milliseconds*1000
  dormantCell = array(0, iter) # voltage (mV)
  activeCell = array(0, iter)
  
  stim = array(stimDefault, iter)
  
  dt = .0105                # seemed like a good value
  R = 1                   # resistance
  T = 1                   # time membrane onstant
  dormantRefractory = 0          # set to 0 (assuming neuron hasn't spiked b4 simulation start)
  activeRefractory = 0  
  c = 1                   # index
  
  while (c < iter) {
    if (dormantRefractory <= 0) {
      # this sets up refractory period
      dv = ((-dormantCell[c] + R * stim[c]) / T) * dt
      dormantCell[c + 1] = dormantCell[c] + dv
    }
    else {
      dormantRefractory = dormantRefractory - 1
    }
    if (activeRefractory <= 0) {
      # this sets up refractory period
      dv = ((-activeCell[c] + R * stim[c]) / T) * dt
      activeCell[c + 1] = activeCell[c] + dv
    }
    else {
      activeRefractory = activeRefractory - 1
    }
    for (i in 1:dim(perturb)[1]) {
      if (perturb[i,1]*1000 == c) { # there should be max 1 voltage per time
        activeCell[c+1] = activeCell[c] + perturb[i,2]
      }
    }
    if (dormantCell[c + 1] > 15) {
      dormantCell[c + 1] = 0
      dormantRefractory = absRefractory
    }
    if (activeCell[c + 1] > 15) {
      activeCell[c + 1] = 0
      activeRefractory = absRefractory
    }
    c = c + 1 #reset voltage when it gets to 15
  }                                     #loop for 5 iterations
  return(cbind(dormantCell, activeCell))
}

time = 1
perturbMat = matrix(2, # voltage perturbation
                 nrow=5,
                 ncol=2)
for (i in 1:dim(perturbMat)[1]) {
  perturbMat[i,1] = i*time/(dim(perturbMat)[1])
}
voltageMat = simulateLIF(time, perturbMat)
unperturbHist = voltageMat[,1]
perturbHist = voltageMat[,2]

iter = dim(voltageMat)[1]

plot((1:iter) / iter,
     unperturbHist,
     lty=23,
     type = 'l',
     xlab = "time",
     ylab = "Voltage (mV)")

points((1:iter)/iter, perturbHist, type = 'l')
unperturbSpike = array(0)
perturbSpike = array(0)
for(i in 2:iter) {
  if (unperturbHist[i] == 0 && unperturbHist[i-1] > 0) {
    unperturbSpike = c(unperturbSpike, i)
    abline(v = i / iter, lty=23, col = 'magenta')
  }
  if (perturbHist[i] == 0 && perturbHist[i-1] > 0) {
    perturbSpike = c(perturbSpike, i)
    abline(v = i / iter, lty=23, col = 'wheat')
  }
}