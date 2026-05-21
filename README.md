# gamess_functions

A collection of R functions for parsing and analysing [GAMESS (US)](http://www.msg.ameslab.gov/gamess/) output files.

---

## Overview

GAMESS (US) produces detailed text-based output files that can be difficult to work with programmatically. This repository provides a set of reusable R functions to extract and structure key data from these outputs.

The goal is to support reproducible and scalable computational chemistry workflows.

These tools are designed not just for extraction, but for diagnosing the quality and reliability of quantum chemical calculations.

---

## Available functions

### Output parsing
- [extract_nmr()](./R/extract_nmr.R) – Extract NMR shielding data  
- [IRC_energy()](./R/IRC_energy.R) – Retrieve calculated energies  
- [extract_ir_diagnostics()](./R/extract_ir_diagnostics.R) – Extract vibrational frequencies and assess geometry quality via translation/rotation modes

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

```r
source("./R/extract_ir_diagnostics.R")

extract_ir_diagnostics("./examples/rem01d.log")
$frequencies
 [1] -377.22 -147.32   93.21    0.10    0.07    0.12   47.14   55.54  100.92  177.99  253.11  312.74  373.41
[14]  439.74  482.12  609.56  637.11  669.88  807.45  825.05  864.97  883.19  910.57  925.01  950.19  972.47
[27] 1001.40 1056.48 1085.48 1098.75 1108.81 1153.04 1187.76 1217.21 1229.49 1237.44 1267.43 1281.27 1305.17
[40] 1327.98 1392.45 1398.54 1430.28 1441.63 1533.28 1541.64 1577.20 3089.05 3122.07 3131.28 3163.56 3181.41
[53] 3197.93 3200.14 3265.35 3311.38 3532.79

$trans_rot
[1] 93.21  0.10  0.07  0.12 47.14 55.54

$trans_rot_modes
[1] 3 4 5 6 7 8

$tr_range
[1] 3 8

$max_trans_rot_error
[1] 93.21

$has_imaginary
[1] TRUE
```
In this example, the large translation/rotation value (93 cm⁻¹) indicates a poorly converged geometry or problematic Hessian.

---

## Structure

gamess_functions/
├── R/
│   ├── extract_nmr.R
│   ├── IRC_energy.R
│   ├── extract_basis.R
│   ├── extract_ir_diagnostics.R
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
