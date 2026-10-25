#!/usr/bin/env python3
"""Self-contained checks for inchi_species.py (geometries are built with RDKit, no repo files needed).
Run:  python3 tests/test_inchi_species.py      (exit status 0 = all pass)"""
import os, sys
here = os.path.dirname(os.path.abspath(__file__))
for rel in (("..", "python"), ("..", "..", "python")):
    sys.path.insert(0, os.path.join(here, *rel))
from rdkit import Chem
from rdkit.Chem import AllChem
import inchi_species as ic

def geom(smiles, seed=7):
    m = Chem.AddHs(Chem.MolFromSmiles(smiles)); AllChem.EmbedMolecule(m, randomSeed=seed); AllChem.MMFFOptimizeMolecule(m)
    c = m.GetConformer()
    return [(a.GetSymbol(), *c.GetAtomPosition(a.GetIdx())) for a in m.GetAtoms()]

def run(g, claims=(), mult=1, charge=0):
    return ic.run(g, charge, mult, list(claims))

fails = []
def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond: fails.append(name)

ald = geom("ClCC=O"); lac = geom("C[C@H](O)C(=O)O"); but = geom("C/C=C/C")
r = run(ald); check("generate: chloroacetaldehyde", r["overall"] == "ok" and r["species"][0]["inchi"] == "InChI=1S/C2H3ClO/c3-1-2-4/h2H,1H2")
r = run(lac); check("generate: lactic acid carries its stereo layer", "/t" in r["species"][0]["inchi"] and r["overall"] == "ok")
check("supplied: right InChI accepted", run(ald, [{"inchi": "InChI=1S/C2H3ClO/c3-1-2-4/h2H,1H2"}])["overall"] == "ok")
check("supplied: wrong key refused", run(ald, [{"inchi": "InChI=1S/C2H3ClO/c3-1-2-4/h2H,1H2", "inchikey": "AAAAAAAAAAAAAA-BBBBBBBBSA-N"}])["overall"] == "refused")
check("supplied: wrong isomer refused", run(ald, [{"inchi": "InChI=1S/C2H3ClO/c1-2(3)4/h1H3"}])["overall"] == "refused")
check("supplied: wrong formula refused, and nothing generated instead", run(ald, [{"inchi": "InChI=1S/H2O/h1H2"}])["species"][0]["status"] == "refused")
check("SMILES: right one accepted", run(ald, [{"smiles": "ClCC=O"}])["overall"] == "ok")
check("SMILES: unparsable refused", run(ald, [{"smiles": "ClCC(=O"}])["overall"] == "refused")
check("SMILES: several components refused", run(ald, [{"smiles": "ClCC=O.O"}])["overall"] == "refused")
check("USER STORY: chiral geometry, SMILES without stereo -> refused", run(lac, [{"smiles": "CC(O)C(=O)O"}])["overall"] == "refused")
check("chiral geometry, InChI without stereo layer -> refused", run(lac, [{"inchi": "InChI=1S/C3H6O3/c1-2(4)3(5)6/h2,4H,1H3,(H,5,6)"}])["overall"] == "refused")
check("chiral geometry, right enantiomer accepted", run(lac, [{"smiles": "C[C@H](O)C(=O)O"}])["overall"] == "ok")
check("chiral geometry, wrong enantiomer refused", run(lac, [{"smiles": "C[C@@H](O)C(=O)O"}])["overall"] == "refused")
check("double bond: SMILES without E/Z refused", run(but, [{"smiles": "CC=CC"}])["overall"] == "refused")
check("double bond: wrong isomer refused", run(but, [{"smiles": "C/C=C\\C"}])["overall"] == "refused")
check("open shell (single Cl atom, doublet) refused", run([("Cl", 0, 0, 0)], mult=2)["overall"] == "refused")
check("one bad claim withholds the whole system", all(s["status"] == "refused" for s in run(ald, [{"smiles": "ClCC(=O"}])["species"]))
sys.exit(1 if fails else 0)
