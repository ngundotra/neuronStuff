# Noah Gundotra (MSN) 12-6-16
# This document could be put into StageOne.R,
#   however we figured that it is simpler putting
#   these functions here. These two functions are
#   used for generating F-I curve. See functions

# getAvgFrequency
#               takes spike array
#               returns average frequency of spikes
getAvgFrequency = function(spikeTimes) {
  total = 0
  for(i in (2:length(spikeTimes))) {
    total = total + spikeTimes[i] - spikeTimes[i-1]
  }
  avg_period = total/length(spikeTimes)
  return(1/avg_period)
}

# getFI
#       takes: a range of stimulus values, 
#             number of stimuli values to try (resolution)
#       returns: average frequency of spikes for a given neuron
#               this nueron is parameterized within FI
getFI = function(stimMin,
                  stimMax,
                  resolution = 100, # picks resolution values btwn Min and Max
                  max_time = 1, # in milliseconds
                  refractory = 0,
                  plot_on = TRUE) {
  freqHist = array(0, resolution)
  for(i in (1:resolution)) {
    stim = stimMin + i*(stimMax-stimMin)/resolution
    print(stim)
    voltHist = simulateLIF(max_time, absRefractory = refractory, stimDefault=stim) # simulates neuron
    spikeHist = getSpike(voltHist, plot_on=FALSE)     # finds spike times
    freqHist[i] = getAvgFrequency(spikeHist)                # gets avg freq for given stim
  }
  if (plot_on) {
    plot((1:resolution)*(stimMax-stimMin)/resolution+stimMin,
         freqHist,
         type='l',
         xlab="Stimulus (mV)",
         ylab="Frequency (Hz)"
    )
  }
  return(freqHist)
}