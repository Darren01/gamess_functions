# gamess_functions

A collection of R functions for parsing and analysing [GAMESS (US)](http://www.msg.ameslab.gov/gamess/) output files.

---

## Overview

GAMESS (US) produces detailed text-based output files that can be difficult to work with programmatically. This repository provides a set of reusable R functions to extract and structure key data from these outputs.

The goal is to support reproducible and scalable computational chemistry workflows.

---

## Available functions

### Output parsing
- [extract_nmr()](./R/extract_nmr.R) – Extract NMR shielding data  
- [IRC_energy()](./R/IRC_energy.R) – Retrieve calculated energies  

### Input parsing
- [extract_basis_name()](./R/extract_basis.R) – Extract and interpret basis sets (e.g. `6-31+G*`, `6-311++G**`, `aug-cc-pVXZ`)

* *(ongoing)*

---

## Example usage

```r
source("extract_nmr.R")

data <- extract_nmr("sample.out")
head(data)
```
An example of a GAMESS (US) log file is [Methanol NMR](http://figshare.com/articles/Methanol_NMR/1262213)

```r
source("R/extract_basis.R")

res <- extract_basis_folder("input_files/")
head(res)
```

example of what the function returns

file         basis
job1.inp     6-31+G*
job2.inp     6-311++G**
job3.inp     aug-cc-pVXZ

---

## Structure

gamess_functions/
├── R/
│   ├── extract_nmr.R
│   ├── IRC_energy.R
│   ├── extract_basis.R
│   └── ...
├── examples/
└── README.md

Each function is stored as a separate `.R` file for clarity and reuse.

---

## Use cases

* NMR data analysis
* Comparing computed and experimental results
* Feeding data into downstream models (e.g. DP4)
* Automating extraction from multiple GAMESS jobs
* Auditing computational setups (e.g. basis sets across many calculations)

---

## Future work

* Expand input parsing (e.g. functional, solvent, job type)
* Improve support for Dunning and ECP basis sets
* Add validation and error handling
* Develop into a lightweight R package
