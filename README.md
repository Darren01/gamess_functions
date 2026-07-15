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
- **[extract_geometry_trajectory()](./R/extract_geometry_trajectory.R)** - Extract optimisation trajectory (geometries + energies) from GAMESS output


### Input parsing
- [strip_input_card_prefix()](./R/gamess_input_utils.R), [get_gamess_block()](./R/gamess_input_utils.R), [parse_gamess_block()](./R/gamess_input_utils.R) – Shared block-matching helpers: find and parse a `$GROUP ... $END` block from either a raw `.inp` file or a `.log` file's echoed `INPUT CARD>` text. Every other input-parsing function below is built on these - source this file first.
- [extract_basis_name()](./R/extract_basis.R) – Extract and interpret basis sets (e.g. `6-31+G*`, `6-311++G**`, `aug-cc-pVXZ`). Works on `.inp` or `.log`.
- [classify_gamess_job()](./R/classify_gamess_jobs.R), [classify_gamess_jobs()](./R/classify_gamess_jobs.R) – Classify a job by RUNTYP/HSSEND (GeometryOptimization, SinglePoint, VibrationalAnalysis), single file or batch over a directory. Works on `.inp` or `.log`.
- [extract_input_parameters()](./R/extract_input_parameters.R) – Full `$CONTRL`/`$STATPT`/`$SCF` run parameters plus basis set in one call (RUNTYP, SCFTYP, DFTTYP, charge, multiplicity, convergence criteria). The calculation-metadata companion to `classify_gamess_job()`. Works on `.inp` or `.log`.

### Ontology integration
- [sparql_query()](./R/sparql_to_file.R) – Run a SPARQL SELECT against a local ontology/graph file via `robot query`
- [resolve_file_url()](./R/sparql_to_file.R) – Resolve an `ex:fileURL` (`file://...`) to a local filesystem path
- [batch_resolve()](./R/sparql_to_file.R) – Resolve and check a whole vector of `ex:fileURL` values at once, flagging anything missing

* *(ongoing)*

---

## Geometry trajectory extraction

The function `extract_geometry_trajectory()` parses geometry optimisation output and returns:

- All Cartesian geometries
- Corresponding energies (from `NSERCH:` lines)
- Step indices
- Minimum-energy structure

### Key features

- Uses `COORDINATES OF ALL ATOMS` blocks for geometry extraction
- Extracts energies directly from:

NSERCH: n E= -XXX.XXXXXXXX

- Handles mismatches between number of geometries and energies
- Identifies the **final converged structure**, not just the first minimum

### Important notes

- GAMESS may print:
- more geometries than energies
- repeated geometries at convergence
- Therefore:
- `NA` values may appear if no matching energy exists
- multiple identical minimum energies may occur

The function resolves this by selecting the **last occurrence of the minimum energy**, which corresponds to the converged structure (as used by visualisation tools such as Avogadro).

---

## Example usage

```r
source("R/extract_geometry_trajectory.R")

res <- extract_geometry_trajectory("input_files.log")

res$n_geometries
res$energies
res$min_energy
```

Access the final optimised structure:

```r
res$geometries[[res$min_step]]
```

NMR example

```r
source("extract_nmr.R")

data <- extract_nmr("sample.out")
head(data)
```
An example of a GAMESS (US) log file is [Methanol NMR](http://figshare.com/articles/Methanol_NMR/1262213)

Basis extraction

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

IR diagnostics

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

Set `drop_imaginary = TRUE` to return only real vibrational modes:

```r
extract_ir_spectrum("file.log", drop_imaginary = TRUE)
```

---

## SPARQL / ontology integration

`sparql_to_file.R` is the entry point for pipelines driven by an ontology
(e.g. [ont_mm](https://github.com/Darren01/ont_mm)) rather than by a
folder of files: query the graph for which files are involved, resolve
those results to real paths, then hand them straight to any extractor
above.

It shells out to [`robot query`](http://robot.obolibrary.org/), so `robot`
must be on your `PATH` (or pass `robot_cmd = "java -jar /path/to/robot.jar"`).

```r
source("R/sparql_to_file.R")

# Which files came out of a given experiment?
res <- sparql_query(
  graph_file = "path/to/gc_core_full.ttl",
  query = "SELECT ?output WHERE {
             ?exp ex:hasInputFile ex:file_rem01_inp .
             ?output prov:wasGeneratedBy ?exp .
           }"
)

# Get every fileURL in the graph and check what's actually on disk
urls <- sparql_query(
  graph_file = "path/to/gc_core_full.ttl",
  query = "SELECT ?url WHERE { ?f ex:fileURL ?url . }"
)
br <- batch_resolve(urls$url)

# Feed a resolved path straight into an existing extractor
source("R/extract_geometry_trajectory.R")
traj <- extract_geometry_trajectory(br$path[br$exists][1])
```

Common PREFIX declarations (`ex:`, `gc:`, `prov:`, `rdf:`, `rdfs:`, `owl:`,
`dcterms:`) are added automatically unless your query already declares
them.

See [`tests/test_sparql_to_file.R`](./tests/test_sparql_to_file.R) for a
runnable end-to-end check against a real ont_mm graph.

---

## Input parsing internals

`.inp` files and `.log` files represent the same GAMESS input differently:
a raw `.inp` has `$CONTRL RUNTYP=OPTIMIZE $END` starting the line, while a
`.log` echoes the same line prefixed with `INPUT CARD>`. Several functions
here need to read `$CONTRL`/`$STATPT`/`$SCF`/`$BASIS` blocks, and used to
each have their own regex for this - which meant they silently disagreed
about which file type they supported. `extract_basis_name()` only worked
on `.inp`; `extract_input_parameters()` only worked on `.log`; combining
them (as `extract_input_parameters()` does internally, for basis) meant
one half of the result was always `NA` with no warning.

`gamess_input_utils.R` is the fix: one matcher
(`strip_input_card_prefix()` + `get_gamess_block()` + `parse_gamess_block()`)
that works identically on either file type, used by every other
input-parsing function. Source it first.

A related fix in `extract_input_parameters()`: `charge`/`multiplicity`
fall back to GAMESS's real defaults (`ICHARG=0`, `MULT=1`) only when the
`$CONTRL` block was found but that specific keyword was absent - a
legitimate case. If the block wasn't found at all, they come back `NA`
rather than a default that looks like a real value. Check
`contrl_found` in the result if you need to tell the two apart.

---

## Structure

```
gamess_functions/
├── R/
│ ├── extract_nmr.R
│ ├── IRC_energy.R
│ ├── gamess_input_utils.R
│ ├── extract_basis.R
│ ├── classify_gamess_jobs.R
│ ├── extract_input_parameters.R
│ ├── extract_ir_diagnostics.R
│ ├── extract_geometry_trajectory.R
│ ├── sparql_to_file.R
│ └── ...
├── examples/
├── tests/
│ ├── test_sparql_to_file.R
│ └── test_classify_gamess_job.R
└── README.md
```

Each function is stored as a separate `.R` file for clarity and reuse.

---

## Use cases

* NMR data analysis
* Comparing computed and experimental results
* Feeding data into downstream models (e.g. DP4)
* Automating extraction from multiple GAMESS jobs
* Tracking optimisation convergence
* Extracting full reaction or optimisation trajectories

---

## Future work

* Feed extract_input_parameters() calculation metadata (basis, charge,
  multiplicity, functional) into ont_mm results-template writers, once
  the results schema is aligned to gc:CalculationResult/FloatValue
  (in progress)
* Improve support for Dunning and ECP basis sets
* Add validation and error handling to the older functions (extract_nmr,
  IRC_energy)
* Export trajectories to .xyz for visualisation
* Develop into a lightweight R package

## Author

[Darren Rhodes]

## License

This project is licensed under the MIT License – see the [LICENSE](./LICENSE.txt) file for details.
