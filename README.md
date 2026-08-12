# gamess_functions

A collection of R functions for parsing and analysing [GAMESS (US)](http://www.msg.ameslab.gov/gamess/) output files.

---

## Overview

GAMESS (US) produces detailed text-based output files that can be difficult to work with programmatically. This repository provides a set of reusable R functions to extract and structure key data from these outputs.

The goal is to support reproducible and scalable computational chemistry workflows.

These tools are designed not just for extraction, but for diagnosing the quality and reliability of quantum chemical calculations.

**This project pairs with [ont_mm](https://github.com/Darren01/ont_mm)**, which takes what gets extracted here and builds it into a queryable RDF/OWL ontology graph (grounded in the [Gainesville Core](https://github.com/Darren01/GC-Ontology-Mirror) vocabulary). The "Output parsing" functions below work entirely standalone - point one at a `.log` file and get structured data back, no ontology involved at all. The "Ontology writers" and "Ontology integration and querying" sections are specifically where the two projects meet: writers turn extracted data into `gc:`/`ex:` template rows for `ont_mm`'s `build_ontology_graph()`, and the querying functions (`summarize_graph()`, `compare_energies()`, `sparql_query()`) read back the graph that produces. Neither half needs the other to be useful on its own.

---

## Available functions

### Output parsing
Raw extraction from `.log` files - what a calculation actually produced.

- [extract_ir_spectrum()](./R/extract_ir_spectrum.R) – Extract the full vibrational frequency/intensity spectrum
- [extract_ir_diagnostics()](./R/extract_ir_diagnostics.R) – Extract vibrational frequencies and identify translation/rotation modes specifically, for geometry-quality diagnosis
- [extract_thermochemistry()](./R/extract_thermochemistry.R) – Zero-point energy, enthalpy, entropy, Gibbs free energy, and the temperature they were computed at
- [extract_electronic_energy()](./R/extract_electronic_energy.R) – The uncorrelated SCF reference energy from a `SinglePoint` run (`FINAL ... ENERGY IS`)
- [extract_pcm_free_energy()](./R/extract_pcm_free_energy.R) – The real, correlated/solvated "gold standard" energy from a high-precision `SinglePoint` run - correctly distinguishes a PCM-solvated CCSD(T)-type result from the plain SCF reference that `extract_electronic_energy()` finds, and fails loudly rather than silently returning an incomplete value for a correlated-method-without-PCM or unrecognized-solvation-method file
- [extract_nmr()](./R/extract_nmr.R) – Extract GIAO NMR shielding tensors (isotropic + anisotropy) per atom
- [extract_irc_trajectory()](./R/extract_irc_trajectory.R) – Extract one direction (forward or backward) of an intrinsic reaction coordinate run
- [combine_irc_trajectories()](./R/combine_irc_trajectories.R) – Stitch a forward + backward IRC pair sharing the same saddle point into one combined reaction path
- [extract_constraints()](./R/extract_constraints.R) – Extract distance/angle/dihedral constraints from a geometry optimisation
- [extract_geometry_trajectory()](./R/extract_geometry_trajectory.R) – Extract the full optimisation trajectory (every geometry + energy + step index), identifying the converged final structure
- [geometry_to_atoms()](./R/geometry_to_atoms.R) – Extract per-atom Cartesian coordinates as individually-addressable atoms

### Input parsing
Shared, `.inp`-or-`.log`-agnostic parsing of what a calculation actually asked for.

- [strip_input_card_prefix()](./R/gamess_input_utils.R), [get_gamess_block()](./R/gamess_input_utils.R), [parse_gamess_block()](./R/gamess_input_utils.R) – Shared block-matching helpers: find and parse a `$GROUP ... $END` block from either a raw `.inp` file or a `.log` file's echoed `INPUT CARD>` text. Every other input-parsing function below is built on these - source this file first.
- [extract_basis_name()](./R/extract_basis.R), [extract_basis_folder()](./R/extract_basis.R) – Extract and interpret basis sets (e.g. `6-31+G*`, `6-311++G**`, `aug-cc-pVXZ`), single file or a whole folder
- [extract_level_of_theory()](./R/extract_level_of_theory.R) – A human-readable level-of-theory label combining basis set, method, and solvent (e.g. `"CCSD(T)/aug-cc-pVTZ PCM(water)"`) - reuses `extract_basis_name()` rather than its own basis interpretation, and explicitly refuses to guess at a `BASNAM` (mixed/custom per-atom basis) job rather than produce a misleadingly simple label
- [classify_gamess_job()](./R/classify_gamess_jobs.R), [classify_gamess_jobs()](./R/classify_gamess_jobs.R) – Classify a job by RUNTYP/HSSEND (GeometryOptimization, SinglePoint, VibrationalAnalysis, SaddlePoint, IRC), single file or batch over a directory
- [extract_input_parameters()](./R/extract_input_parameters.R) – Full `$CONTRL`/`$STATPT`/`$SCF` run parameters plus basis set in one call (RUNTYP, SCFTYP, DFTTYP, charge, multiplicity, convergence criteria)

### Quality and diagnostic checks
Is this result trustworthy, converged, and consistent with what came before it?

- [check_vibrational_quality()](./R/check_vibrational_quality.R) – Is this geometry converged, or does it need another optimisation iteration? The pipeline-driving check used by `process_results.R` - reports status, doesn't decide whether to write results
- [check_geometry_quality()](./R/check_geometry_quality.R) – Classifies the stationary point type (minimum / transition state / higher-order saddle) from the number of imaginary frequencies, and reports translation/rotation quality
- [check_geometry_continuity()](./R/check_geometry_continuity.R) – Does a later geometry genuinely follow from an earlier one, or do they look unrelated?
- [check_geometry_chain()](./R/check_geometry_chain.R) – Walk a whole sequence of runs, flagging genuine breaks vs. deliberate large moves (e.g. a scan step) vs. continuous refinement
- [check_deliberate_constraint_adjustment()](./R/check_deliberate_constraint_adjustment.R) – Confirm a geometry change matches a specific, deliberate constraint adjustment (the changed atoms moved as expected, everything else stayed put)
- [check_source_sync()](./R/check_source_sync.R) – Confirm two copies of a shared source file (e.g. the `gc:` ontology mirror) are genuinely in sync before a build

### Ontology writers
Turn extracted data into `gc:`/`ex:` template rows for `ont_mm`'s `build_ontology_graph()` - the bridge between raw extraction and a queryable graph.

- [ir_spectrum_to_templates()](./R/ir_spectrum_to_templates.R) – Spectrum/peak/float-value rows from `extract_ir_spectrum()`'s output
- [thermochemistry_to_templates()](./R/thermochemistry_to_templates.R) – SystemEnergies rows from `extract_thermochemistry()`'s output
- [electronic_energy_to_templates()](./R/electronic_energy_to_templates.R) – SystemEnergies rows from a `SinglePoint` run's electronic energy
- [reaction_path_to_templates()](./R/reaction_path_to_templates.R) – ReactionPath/ReactionPathPoint rows from `combine_irc_trajectories()`'s output
- [constraints_to_templates()](./R/constraints_to_templates.R) – DistanceConstraint/AngleConstraint/DihedralConstraint rows from `extract_constraints()`'s output
- [nmr_to_templates()](./R/nmr_to_templates.R) – Links real `gc:Atom` individuals to their computed NMR shielding values (requires `geometry_to_atoms()` already run for the same experiment)
- [notes_to_annotations()](./R/notes_to_annotations.R), [write_annotations()](./R/notes_to_annotations.R) – Turn a `run_notes.tsv` of your own review comments into `skos:editorialNote` annotation rows. Safe to re-run after editing your notes - regenerates fresh each time rather than appending, so revisiting old notes never duplicates them

### Ontology integration and querying
Once a graph is built - explore and analyse it.

- [sparql_query()](./R/sparql_to_file.R) – Run a SPARQL SELECT against a local ontology/graph file via `robot query`, with common `PREFIX` declarations added automatically
- [resolve_file_url()](./R/sparql_to_file.R), [batch_resolve()](./R/sparql_to_file.R) – Resolve an `ex:fileURL` (or a whole vector of them) to real local filesystem paths, flagging anything missing
- [summarize_graph()](./R/summarize_graph.R) – A full overview of a built graph with zero SPARQL required: experiments, your own review notes, imaginary-frequency quality flags, constraints
- [shorten_uris()](./R/shorten_uris.R) – Strip namespace prefixes from any query result for readable display (`http://purl.org/gc/angstrom` → `angstrom`)
- [compare_energies()](./R/compare_energies.R) – Raw energy differences (electronic, ZPE, enthalpy, entropy, Gibbs) between two experiments - e.g. substrate vs. transition state for an activation energy
- [thermochemistry_table()](./R/thermochemistry_table.R), [print_markdown_table()](./R/thermochemistry_table.R) – A summary table across multiple log files (ScanNo/ZPE/Enthalpy/Entropy/Gibbs/LevelOfTheory/Notes), with a copy-paste-ready Markdown print option

### Legacy
- [IRC_energy()](./R/IRC_energy.R) – Plots a reaction-path energy profile from a [wxMacMolPlt](https://github.com/brettbode/wxmacmolplt)-exported `.cml` file (Bode, B. M.; Gordon, M. S. *J. Mol. Graphics Modell.* 1999, 16(3), 133-138, DOI: 10.1016/S1093-3263(99)00002-9 - the GAMESS (US) community's standard visualization tool). Superseded by `extract_irc_trajectory()` + `combine_irc_trajectories()`, which build the same combined path directly from the two native GAMESS (US) logs, no wxMacMolPlt export step needed. Had a known unit-label bug (fixed): the y-axis was labelled kJ/mol, but the conversion factor (627.51) is actually Hartree→kcal/mol. Kept for now in case existing `.cml` files are still in use somewhere, not recommended for new work.

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

- GAMESS (US) may print:
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

## Querying a built graph

Once [ont_mm](https://github.com/Darren01/ont_mm)'s `build_ontology_graph()`
has produced a real graph, these are the tools for actually getting answers
back out of it - from zero SPARQL required, up to writing your own.

**No SPARQL at all** - a full overview in one call:

```r
source("R/summarize_graph.R")
summarize_graph("path/to/gc_core_full.ttl")
```

```
=== Summary of gc_core_full.ttl ===

Experiments: 3
GeometryOptimization  VibrationalAnalysis
                   2                    1

Your own review notes: 0

Imaginary (negative) frequencies found: 0

Constraints: 6
```

(Real output against `ont_mm`'s own bundled `rem01`/`rem01a`/`rem01b`
example - genuinely sparse for a small 3-experiment demo, shown honestly
rather than dressed up. On an actively-used project, the review-notes and
imaginary-frequency lines are usually where the useful signal shows up.)

**One specific, real question** - raw energy differences between two
experiments (e.g. an activation energy, substrate vs. transition state):

```r
source("R/compare_energies.R")
compare_energies("path/to/gc_core_full.ttl", "ex:exp_rem01", "ex:exp_rem01b")
```

Returns a data.frame: one row per energy quantity present in both
experiments (quantity, value_a, value_b, difference, unit) - see
`?compare_energies` for the exact shape.

**A summary table across several files**, for a write-up - combines
`extract_thermochemistry()` and `extract_level_of_theory()` directly:

```r
source("R/thermochemistry_table.R")

files <- c("examples/rem01.log", "examples/rem01b.log")
notes <- c(rem01b = "Second optimisation step")

tbl <- thermochemistry_table(files, notes)
print_markdown_table(tbl)   # copy-paste-ready for a Markdown document
```

**Writing your own SPARQL**, with a tiered path from templates to
first-principles: see
[`examples/query_your_ontology.R`](./examples/query_your_ontology.R).

Every function above works entirely independently of the others - use
`summarize_graph()` on its own without ever touching `compare_energies()`,
or write raw SPARQL via `sparql_query()` without any of the higher-level
wrappers at all.

---

## Standalone use, without the ontology at all

Everything in "Output parsing" and "Input parsing" above works as a
complete, self-contained tool against a single file - most were built as
building blocks in the larger `ont_mm`-driven pipeline, but none require
it:

```r
source("R/extract_thermochemistry.R")
extract_thermochemistry("examples/rem01b.log")
```

Returns a one-row data.frame: file, temperature (+ unit), zpe (+ unit),
enthalpy (+ unit), gibbs (+ unit), entropy (+ unit) - no graph, no
ontology, no `ont_mm` involved, just the numbers from that one file.

---

## Input parsing internals

`.inp` files and `.log` files represent the same GAMESS (US) input differently:
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
fall back to GAMESS (US)'s real defaults (`ICHARG=0`, `MULT=1`) only when the
`$CONTRL` block was found but that specific keyword was absent - a
legitimate case. If the block wasn't found at all, they come back `NA`
rather than a default that looks like a real value. Check
`contrl_found` in the result if you need to tell the two apart.

---

## Structure

```
gamess_functions/
├── R/
│ ├── gamess_input_utils.R
│ ├── extract_basis.R
│ ├── extract_level_of_theory.R
│ ├── classify_gamess_jobs.R
│ ├── extract_input_parameters.R
│ ├── extract_ir_spectrum.R
│ ├── extract_ir_diagnostics.R
│ ├── extract_thermochemistry.R
│ ├── extract_electronic_energy.R
│ ├── extract_pcm_free_energy.R
│ ├── extract_nmr.R
│ ├── extract_irc_trajectory.R
│ ├── combine_irc_trajectories.R
│ ├── extract_constraints.R
│ ├── extract_geometry_trajectory.R
│ ├── geometry_to_atoms.R
│ ├── check_vibrational_quality.R
│ ├── check_geometry_quality.R
│ ├── check_geometry_continuity.R
│ ├── check_geometry_chain.R
│ ├── check_deliberate_constraint_adjustment.R
│ ├── check_source_sync.R
│ ├── ir_spectrum_to_templates.R
│ ├── thermochemistry_to_templates.R
│ ├── electronic_energy_to_templates.R
│ ├── reaction_path_to_templates.R
│ ├── constraints_to_templates.R
│ ├── nmr_to_templates.R
│ ├── notes_to_annotations.R
│ ├── sparql_to_file.R
│ ├── summarize_graph.R
│ ├── shorten_uris.R
│ ├── compare_energies.R
│ ├── thermochemistry_table.R
│ └── IRC_energy.R (legacy)
├── examples/
│ ├── query_your_ontology.R
│ └── rem01d.log
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
* Automating extraction from multiple GAMESS (US) jobs
* Tracking optimisation convergence
* Extracting full reaction or optimisation trajectories
* Querying a built [ont_mm](https://github.com/Darren01/ont_mm) graph directly with SPARQL - or without writing any SPARQL at all, via `summarize_graph()`. `examples/query_your_ontology.R` has a tiered path between the two: pre-built functions, copy-paste query templates, then a short primer for writing your own
* Every function here works standalone too, independent of the ontology side entirely - most were built as part of the larger extraction pipeline, but each is a complete, self-contained tool in its own right for ad-hoc use against a single file

---

## Future work

* Feed calculation metadata (basis, method, solvent) into ont_mm
  results-template writers, so it lives in the graph itself, not just
  a summary table - `extract_level_of_theory()` is a start (produces
  the label), but nothing writes it into the graph yet
* A combined level-of-theory label spanning two files (e.g.
  `CCSD(T)/aug-cc-pVTZ // 6-31G(d,p)`, geometry level // energy level)
  for gold-standard single points run at a different geometry's level
  than the geometry itself was optimised at
* Extend `extract_electronic_energy()`/`thermochemistry_to_templates()`
  to correctly extract electronic energy for non-`SinglePoint` job
  types (`VibrationalAnalysis`, etc.) - GAMESS (US) prints multiple `FINAL
  ... ENERGY IS` lines across an optimisation's steps, only the last
  is the converged one, not yet handled
* Extend `extract_pcm_free_energy()` for a correlated method run
  without PCM, and for solvation methods other than PCM (COSMO, SMD) -
  currently fails loudly rather than guessing at either, deliberately,
  until each has its own verified extraction path
* Temperature isn't yet captured in the graph at all - `gc:
  hasSystemTemperature`'s real declared domain is `gc:MolecularSystem`,
  a concept this project hasn't instantiated; needs a deliberate design
  decision, not a quick patch
* Improve support for Dunning and ECP basis sets
* IRC_energy()'s unit-label bug is now fixed, but it still only
  produces a plot with no structured data returned - unlike everything
  else in this package. Worth deciding whether to bring it in line
  with the rest (return a data.frame, plot as a convenience) or retire
  it entirely now that extract_irc_trajectory()/
  combine_irc_trajectories() cover the same ground natively
* Export trajectories to .xyz for visualisation
* Develop into a lightweight R package
* A Shiny front end for the ontology-querying functions, once there's
  a clear audience beyond direct R/RStudio use to justify the added
  maintenance surface - deliberately parked, not forgotten.
  `summarize_graph()`'s design (return structured data, print as a
  convenience) is deliberately kept that way specifically to make this
  cheap to add later

## Author

[Darren Rhodes]

## Acknowledgements
Built by Darren Rhodes, using Claude (Anthropic) as a technical assistant for R development, ontology design, and documentation.

## License

This project is licensed under the MIT License – see the [LICENSE](./LICENSE.txt) file for details.
