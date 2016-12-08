
# Noah Gundotra (MSN) 12-7-16
# Updated neuron simulator that allows us to
#   interact with PRC at any given time.
#   Times and voltages we want to perturb neuron
#   with are given in Nx2 "perturb" matrix.

simulateLIF = function(milliseconds,
                       perturb,            # matrix of times and amt voltage
                       absRefractory = 0,
                       stimDefault = 20
                       ) { 
  # returns voltage for both cells... (unperturbed comes first, perturbed comes next)
  # iter            - time (no units)
  # absRefractory   - time of refractory (no units)
  iter = milliseconds*1000
  dormantCell = array(0, iter) # voltage (mV)
  activeCell = array(0, iter)
  
  stim = array(stimDefault, iter)
  
  dt = .0105/100          # consistent with other simulation
  R = 1                   # resistance
  T = 1                   # time membrane onstant
  dormantRefractory = 0   # set to 0 (assuming neuron hasn't spiked b4 simulation start)
  activeRefractory = 0  
  c = 1                   # index
  
  # "dormant" refers to unperturbed neuron
  # "active" refers to perturbed neuron
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
    
    # checking to see if "active cell" should be perturbed
    # first col of perturb matrix are times to perturb
    if (c %in% (1000*perturb[,1])) {
      activeCell[c+1] = activeCell[c] + perturb[ which(perturb[,1]==c/1000), 2]
    }
    
    # reset cell if refractory period up
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

# set up & running the actual code

# setting up times to perturb the neuron
time = 100
perturbMat = matrix(nrow=1, ncol=2)
perturbMat[1,1] = time/5
perturbMat[1,2] = 1

voltageMat = simulateLIF(time, perturbMat)
unperturbHist = voltageMat[,1]
perturbHist = voltageMat[,2]

iter = dim(voltageMat)[1]

getPhi = function(voltageMat) {
  # does not make any assumptions abt num perturb
  dormantSpike = getSpike(voltageMat[,1], FALSE)
  activeSpike  = getSpike(voltageMat[,2], FALSE)
  spikeTime = dormantSpike[2] - dormantSpike[1]
  
  # PR returned btwn 0 and 1
  phiArray = unique(dormantSpike-activeSpike)/spikeTime
  return(phiArray[2])
}
# plotting the results of the simulation
# dotted line is unperturbed neuron
# full line is perturbed neuron
plot((1:iter) / iter,
     unperturbHist,
     lty=23,
     type = 'l',
     xlab = "Time",
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

print(getPhi(voltageMat))

getPRC = function(stimMin,
                  stimMax,
                  resolution,
                  time,
                  perturb) {
  curve = matrix(0,resolution,2)
  for(i in 1:resolution) {
    stim = i*(stimMax-stimMin)/resolution + stimMin
    voltageMat = simulateLIF(time, perturb, absRefractory = 0, stimDefault = stim)
    phi = getPhi(voltageMat)
    curve[i,] = c(stim, phi)
    if (i %% (resolution/5) == 0) {print(i)}
  }
  plot(curve[,1], y = curve[,2], type = 'l', main = "PRC", ylab="Phi", xlab="stim")
  return(curve)
}

getPRC(20, 60, 150, 100, perturbMat)