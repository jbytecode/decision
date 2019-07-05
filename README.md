# decisionR: An R package for decision science

Initially, this package includes some multiple-criterion decision making tools such as Topsis and Vikor.
The future plan is to add several methods and algorithms in the research areas of Operations Research and Decision Science.

Package content (The methods signed with &#10004; are implemented):

### Multiple-criteria decision making (MCDA) tools 
* Implement Dematel &#10004; 
* Implement Topsis &#10004;
* Implement Vikor &#10004;
* Implement Moora &#10004;
* Implement Electre &#10004;
* Implement AHP &#10004;
* Implement ANP
* Implement Promethee

### Project management
* CPM (Critical Path Method) &#10004;
* PERT &#10004;

### Decision making under uncertainity and risk
* Laplace &#10004;
* Maximin &#10004;
* Minimax &#10004;
* Maximax &#10004;
* Minimin &#10004;
* Savage &#10004;
* Hurwicz &#10004;
* Maximum Likelihood &#10004;
* Expected Regret &#10004;
* Expected value of perfect information &#10004;

### Game Theory
* Solution of m x n game matrix using linear programming &#10004;

### Markov Chains
* Solutions of n period ahead and equilibrium states &#10004;

### Linear Programming
* Assignment problem &#10004;
* Knapsack Problem &#10004;  

### Scheduling Problem
* n jobs - 2 machines &#10004;
* n jobs - 3 machines &#10004;
* n jobs - m machines &#10004;

### Classification
* Naive Bayes Classifier &#10004;


### Installation:
-------------

devtools::install_github(repo = "https://github.com/jbytecode/decision", subdir = "package/decisionR")

### Updating the package:

devtools::update_packages(packages = "decisionR")


### Examples
* [Multiple-criteria decision making tools] (https://github.com/jbytecode/decision/blob/master/examples/multiple-criteria.md)

* [Game Theory] (https://github.com/jbytecode/decision/blob/master/examples/game-theory.md)

* [CPM - PERT] (https://github.com/jbytecode/decision/blob/master/examples/cpm-pert.md)



