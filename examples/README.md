# Querying your ontology, without needing to know SPARQL first

This folder exists specifically for one thing: getting real answers out
of a graph you've built with [ont_mm](https://github.com/Darren01/ont_mm),
whether or not you're comfortable writing SPARQL yourself.

## Start here: [`query_your_ontology.R`](./query_your_ontology.R)

A single R file, three tiers, roughly in order of how much SPARQL you
need to know:

- **Tier 1 - zero SPARQL.** `summarize_graph(graph_file)`, one function
  call, a real overview: experiments, your own review notes,
  imaginary-frequency quality flags, constraints. Most people's real,
  day-to-day questions stop here.
- **Tier 2 - copy-paste templates.** Four ready-made queries covering
  the most common real questions (everything about one experiment,
  system energies with units, provenance chains, filtered constraints)
  - copy the closest one, change the ID or filter value, run it.
- **Tier 3 - write your own.** A short primer at the bottom of the file
  - not a substitute for a real SPARQL tutorial, just enough to read
  and adapt what's already there. Every lesson in it is a real bug
  this project actually hit (the `FILTER` vs. direct-literal-match
  datatype issue, missing `ORDER BY` giving unpredictable results),
  not textbook theory.

`shorten_uris()` is already wired in throughout - wrap any result in it
for short, readable names instead of full URIs, e.g.
`print(shorten_uris(res))`.

## Using it

Open `query_your_ontology.R` and change one line near the top:

```r
GRAPH_FILE <- "path/to/your/built_graph.ttl"
```

Point it at:

- **The bundled `ont_mm` example graph** (`ont_mm/examples/ont/gc_core_full_*.ttl`)
  to practice on something small and safe first
- **Your own project's graph**, once you've built one with
  `build_ontology_graph()` - this is where it actually earns its keep

Everything else in the file works as-is against either.

## A richer dataset is coming

The bundled example is deliberately small (3 experiments) - enough to
prove the mechanics, not enough to feel like a real project. A larger,
real chloroacetaldehyde dataset is being prepared and will eventually
give a genuinely "meaty" graph to practice against - watch this space.
In the meantime, your own real data is the best practice material
available.

## Note on `rem01d.log`

This file is currently sitting in this folder unused - not referenced
by `query_your_ontology.R` or anywhere else. Likely a leftover from
earlier work rather than something intentionally part of this example;
worth confirming and removing if it's genuinely not needed, rather than
leaving it here implying a role it doesn't have.
