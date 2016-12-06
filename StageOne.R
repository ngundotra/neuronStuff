# Noah Gundotra (MSN) 12-5-16
# Main Script (run this one first)
#   Responsible for setting up basic cell simulation.
#   We created a function to simulate a neuron and a
#   function to retroactively find, record, and plot spikes.
#   These functions are simulateLIF and getSpike.
#
# To simulate a cell and plot it, conjoin the two calls
# ex getSpike(simulateLIF(10, 5, 20, FALSE)) 
#     gives you an array of times where the neuron spiked
#     and plots the neuron's voltage over the simulation time
#
# Running the whole script will give you a basic example
#

# simulateLIF
#         returns an array of recorded voltage of a simulated neuron
#         dt = 0.0105 and iter = 1000 --> ~100 ms in real time
#         (approximated using Brian code in paper)
simulateLIF = function(milliseconds,
                       absRefractory = 5,
                       stimDefault = 20,
                       randomI = FALSE) { # returns v
  # iter            - time (no units)
  # absRefractory   - time of refractory (no units)
  iter = milliseconds*1000
  v = array(0, iter) # voltage (mV)
  if(randomI) {
    stim = rnorm(iter, mean = stimDefault, sd = 5) # randomStim is normally distributed sample
  } else {
    stim = array(stimDefault, iter)
  }
  dt = .0105                # seemed like a good value
  R = 1                   # resistance
  T = 1                   # time membrane onstant
  refractory = 0          # set to 0 (assuming neuron hasn't spiked b4 simulation start)
  c = 1                   # index
  
  while (c < iter) {
    if (refractory <= 0) {
      # this sets up refractory period
      dv = ((-v[c] + R * stim[c]) / T) * dt
      v[c + 1] = v[c] + dv
    }
    else {
      refractory = refractory - 1
    }
    if (v[c + 1] > 15) {
      v[c + 1] = 0
      refractory = absRefractory
    }
    c = c + 1 #reset voltage when it gets to 15
  }                                     #loop for 5 iterations
  return(v)
}
# getSpike
#         takes in an array of voltages (from simulateLIF)
#         records spike when v = 0
#         returns spike times + plots spike times
getSpike = function(voltHist, plot_on=TRUE) {
  iter = length(voltHist)
  if(plot_on) {
    plot((1:iter) / iter,
         voltHist,
         type = 'l',
         xlab = "time",
         ylab = "Voltage (mV)")
  }
  spikeTimes = array(0)
  for(i in 2:iter) {
    if (voltHist[i] == 0 && voltHist[i-1] > 0) {
      spikeTimes = c(spikeTimes, i)
      if(plot_on) {
        abline(v = i / iter, lty=23, col = 'magenta')
      }
    }
  }
  return(spikeTimes)
}

# First simulating a neuron
# Then getting where it spiked
#     since plot_on is set to True by default, 
#     getSpike will plot the spikes on the graph
num_iter = 1000
vHistory = simulateLIF(1)
sHistory = getSpike(vHistory)
