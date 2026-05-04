# gamess_functions

A collection of R functions for parsing and analysing [GAMESS (US)](http://www.msg.ameslab.gov/gamess/) output files.

---

## Overview

GAMESS (US) produces detailed text-based output files that can be difficult to work with programmatically. This repository provides a set of reusable R functions to extract and structure key data from these outputs.

The goal is to support reproducible and scalable computational chemistry workflows.

---

## Available functions

- [extract_nmr()](./R/extract_nmr.R) – Extract NMR shielding data  
- [IRC_energy()](./R/IRC_energy.R) – Retrieve calculated energies  
* *(ongoing)*

---

## Example usage

```r
source("extract_nmr.R")

data <- extract_nmr("sample.out")
head(data)
```
An example of a GAMESS (US) log file is [Methanol NMR](http://figshare.com/articles/Methanol_NMR/1262213)

---

## Structure

Each function is stored as a separate `.R` file for clarity and reuse.

---

## Use cases

* NMR data analysis
* Comparing computed and experimental results
* Feeding data into downstream models (e.g. DP4)
* Automating extraction from multiple GAMESS jobs

---

## Future work

* Combine functions into a cohesive workflow
* Add validation and error handling
* Develop into a lightweight R package
