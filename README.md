# Package 'ideaBatch'
## An R-Package for the internal use in the IDE+A institute.

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
