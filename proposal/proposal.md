590 Final Proposal
==================

#### Participants: *Rachel Steele*

#### Project type: *Package*

#### Git workflow: *Centralized*

Bacterial growth curves are an important aspect of many microbiology
projects. In many labs, these are often graphed using Excel, Prism, or
other software, and the calculations used (especially when graphing data
in triplicate or calculating generation time) are typed out in the
software each time a new set of data is analyzed, or previously
generated templates containing the calculations needed are re-used over
and over. These methods lead to non-reproducible analysis of bacterial
growth curve data. In this project, I will create a package which will
increase reproducibility for this particular problem.

The package I create will contain a handful of functions which will take
input bacterial growth curve data (in the form of time points along with
optical densities corresponding to those time points) and will create
graphs from these data. This package will be able to create growth
curves from data gathered as single pilot tests or from data gathered in
triplicate. In the case of data gathered in triplicate, the means of
each sister data point will be graphed on the curve, and the standard
deviations will be graphed as error bars. In addition to its growth
curve graphing functionality, this package will have the ability to
calculate generation (doubling) times for the bacteria.
