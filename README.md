# simmer
[![Build Status](https://travis-ci.org/Bart6114/simmer.svg?branch=master)](https://travis-ci.org/Bart6114/simmer)

*by Bart Smeets (bartsmeets86@gmail.com) & Iñaki Ucar (i.ucar86@gmail.com)*

__*simmer* is under heavy development and its internals and syntax can still change extensively over the coming time__

*simmer* is a discrete event package for the R language. It is developed with my own specific requirements for simulating day-to-day hospital proceses and thus might not be suited for everyone. It is designed to be as simple to use as possible and uses the chaining/piping workflow introduced by [R6](https://cran.r-project.org/web/packages/R6/) classes. 



## Installation

The installation requires the [devtools](https://github.com/hadley/devtools) package to be installed.


```r
devtools::install_github("Bart6114/simmer")
```

Please note that the package contains some C++ code and you thus need a development environment to build the package (e.g. [Rtools](http://cran.r-project.org/bin/windows/Rtools/) for Windows). If you don't want to build the package yourself and you're on Windows you could try a pre-built binary package [here](https://github.com/Bart6114/simmer/releases/).

## Using simmer

First load the package.


```r
library(simmer)
```

Set-up a simple trajectory. Let's say we want to simulate a ambulatory consultation where a patient is first seen by a nurse for an intake, next by a doctor for the consultation and finally by administrative staff to schedule a follow-up appointment.


```r
t0 <- Trajectory$new("my trajectory") $
  ## add an intake event 
  seize("nurse", 1) $
  timeout(function() rnorm(1, 15)) $
  release("nurse", 1) $
  ## add a consultation event
  seize("doctor", 1) $
  timeout(function() rnorm(1, 20)) $
  release("doctor", 1) $
  ## add a planning event
  seize("administration", 1) $
  timeout(function() rnorm(1, 5)) $
  release("administration", 1)
```

The time-out duration is evaluated dynamically, so it must be a function. This means that if you want an static value instead of a probability, let's say ```3```, you need to enter ```function() 3```.

When the trajectory is know, a simulation environment can be build. In the below example, an environment is instantiated and three types of resources are added. The *nurse* and *administration* resource, each with a capacity of 1, and the *doctor* resource with a capacity of 2. We specify that we want to replicate the simulation 100 times using the ```rep``` argument.


```r
simmer <- Simmer$new("SuperDuperSim", rep=100) $
  add_resource("nurse", 1) $
  add_resource("doctor", 2) $
  add_resource("administration", 1) $
  add_generator("patient", t0, function() rnorm(1, 10, 2))
```

The last method above extends the simulation environment by adding a generator of arrivals following the trajectory ```t0```, wich are activated with an interval of about 10 minutes (a gaussian of ```mean=10``` and ```sd=2```).

The simulation is now ready for a test run; just let it ```simmer``` for a bit. Below, we specify that we want to limit the run-time to 80 time units using the ```until``` argument.


```r
simmer$run(until=80)
```

It is possible to resume the execution simply by specifying a longer run-time. Below, we continue the execution until 120 time units.


```r
simmer$run(until=120)
```

Also, if the simulation is heavy and you want to parallelize the execution of replicas, you can add the argument ```parallel=n```, where ```n``` is the number of threads required (this functionality requires the package [doParallel](https://cran.r-project.org/web/packages/doParallel/)). Let's restart the execution from the beginning with 2 threads.


```r
simmer$reset() $
  run(until=120, parallel=2)
```

### Optional events

The *branch method* introduces the possibility of introducing probability in whether or not to include a branch in a trajectory. The following example shows how a trajectory can be build with a 50-50 chance for the arrival to undergo the second *time-out event*.


```r
t1 <- Trajectory$new("trajectory with a branch") $
  seize("server", 1) $
  branch(prob=0.5, merge=T, Trajectory$new("branch1") $
    timeout(function() 1)
  ) $
  branch(prob=0.5, merge=T, Trajectory$new("branch2") $
    timeout(function() rexp(1, 3))
  ) $
  release("server", 1)
```

The argument ```merge``` indicates whether the arrival must continue executing the events after the branch or not.

### Resource utilization

After you've left it simmering for a bit (pun intended), we can have a look at the overall resource utilization. The top and bottom of the error bars show respectively the 25th and 75th percentile of the utilization across all the replications. The top of the bar shows the median utilization.


```r
plot_resource_utilization(simmer, c("nurse", "doctor","administration"))
```

![](README_files/figure-html/unnamed-chunk-9-1.png) 

It is also possible to have a look at a specific resource and its activity during the simulation.


```r
plot_resource_usage(simmer, "doctor", types="server", steps=T)
```

![](README_files/figure-html/unnamed-chunk-10-1.png) 

In the above graph, the individual lines are all seperate replications. The step lines are instantaneous utilization and the smooth line is a running average. You can also see here that the ```until``` time of 120 was most likely lower than the unrestricted run time of the simulation. It is also possible to get a graph about a specific replication by simply specifying the replication number. In the example below the 6th replication is shown.


```r
plot_resource_usage(simmer, "doctor", 6, types="server", steps=T)
```

![](README_files/figure-html/unnamed-chunk-11-1.png) 

One can also query the raw resource monitor data.


```r
head(
  simmer$get_mon_resources()
  )
```

```
##       time server queue system resource replication
## 1 10.72273      0     0      0    nurse           1
## 2 23.71142      1     0      1    nurse           1
## 3 25.65425      1     1      2    nurse           1
## 4 31.91200      1     0      1    nurse           1
## 5 39.56192      1     1      2    nurse           1
## 6 39.91773      1     0      1    nurse           1
```

### Flow time

Next we can have a look at the evolution of the arrivals' flow time during the simulation. In the below plot, each individual line represents a replication. A smoothline is drawn over them. All arrivals that didn't finish their entire trajectory are excluded from the plot.


```r
plot_evolution_arrival_times(simmer, type = "flow_time")
```

![](README_files/figure-html/unnamed-chunk-13-1.png) 

Similarly one can have a look at the evolution of the activity times with ```type = "activity_time"``` and waiting times with ```type = "waiting_time"```.

It is also possible to extract the arrival monitor data.


```r
head(
  simmer$get_mon_arrivals()
  )
```

```
##        name start_time  end_time activity_time finished replication
## 1 patient13  10.722730  52.14979      41.42706        1           1
## 2 patient14  23.711416  67.84983      42.19558        1           1
## 3 patient15  31.912001  77.87165      38.30973        1           1
## 4 patient16  39.917731  95.15135      41.47199        1           1
## 5 patient17  49.069160 109.31771      39.40658        1           1
## 6 patient14   9.880601  48.85323      38.97263        1           2
```

**DOCUMENTATION TO BE CONTINUED**

## Roadmap

* Time-specific resource availability
* Refine queue discipline (and allow multiple types?)

## Contact

For bugs and/or issues: create a new issue on GitHub.

Other questions or comments: bartsmeets86@gmail.com
