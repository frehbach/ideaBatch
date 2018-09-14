# Package 'ideaBatch'
### General Info
'ideaBatch' is an R-Package for the internal use in the IDE+A institute. It's an installation wrapper around the 'batchtools' R-package: https://github.com/mllg/batchtools

<a href="http://www.repostatus.org/#wip"><img src="http://www.repostatus.org/badges/latest/wip.svg" alt="Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public." /></a>
[![Build Status](https://travis-ci.org/frehbach/ideaBatch.svg?branch=master)](https://travis-ci.org/frehbach/ideaBatch)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/frehbach/ideaBatch?branch=master&svg=true)](https://ci.appveyor.com/project/frehbach/ideaBatch)
[![codecov](https://codecov.io/gh/frehbach/ideaBatch/branch/master/graph/badge.svg)](https://codecov.io/gh/frehbach/ideaBatch)

### Requirements:
 
* A passwordless ssh connection to the cluster. So that you can connect via "ssh #ServerName#"
* The devtools package has to be installed on the cluster
  * ssh #ServerName# > R > install.packages("devtools")

### Installation:

```R
require(devtools)
install_github("frehbach/ideaBatch")
```

### Setup:
Run the init command and provide answers to the questions you are asked...

```R
initBatchTools()
```

### Tutorials
After the intial setup, it is recommended to take a look at some of the provided tutorials in the 'ideaBatch'-package.
List them via:

```R
ideaTutorial()
```

Open the tutorial explanation by specifying the tutorial index in the function call e.g.:
```R
ideaTutorial(1)
```
