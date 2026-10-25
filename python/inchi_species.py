#!/usr/bin/env python3
"""inchi_species.py - species identifiers (InChI / InChIKey) for a GAMESS geometry.

Rule of this tool: a blank identifier is better than a misleading one.
Whenever there is any doubt, a species is REFUSED: no InChI, no key, and a
reason. There is no "warn and carry on" path for a missing stereo layer.

Scope: minima and isolated species only. Do not call it for transition-state
or IRC geometries (bond perception is not meaningful there).

Input geometry: CSV with columns atom,x,y,z (angstrom), in the order the atoms
appear in the $DATA block (so atom numbers match the graph's atom numbering).

Usage:
  inchi_species.py --geometry geom.csv --charge 0 --mult 1 [--claims claims.json] [--out out.json]

claims.json (optional): a list. Each entry may hold "inchi" and "inchikey"
(user-supplied) and/or "smiles" (converted, never stored). An entry is matched
to a species by molecular formula; two species of the same formula that are
different isomers are caught by the connectivity check.

Output JSON: {"species": [ {index, atoms, formula, status, route, inchi,
inchikey, reasons, notes} ... ], "overall": "ok" | "partly refused" | "refused"}
"""
import argparse
import csv
import json
import re
import sys

try:
    from rdkit import Chem, RDLogger
    from rdkit.Chem import rdDetermineBonds
    from rdkit.Chem.rdMolDescriptors import CalcMolFormula
    RDLogger.DisableLog("rdApp.*")
    HAVE_RDKIT = True
except ImportError:  # pragma: no cover
    HAVE_RDKIT = False

KEY_RE = re.compile(r"^[A-Z]{14}-[A-Z]{8}[SN][A-Z]-[A-Z]$")
INCHI_RE = re.compile(r"^InChI=1S/[^\s]+$")


def hill(counts):
    """Hill-order formula string from {element: count}."""
    def one(e):
        return e + (str(counts[e]) if counts[e] > 1 else "")
    els = sorted(counts)
    if "C" in counts:
        order = ["C"] + (["H"] if "H" in counts else []) + [e for e in els if e not in ("C", "H")]
    else:
        order = els
    return "".join(one(e) for e in order)


def formula_from_inchi(inchi):
    m = re.match(r"^InChI=1S/([^/]+)", inchi)
    return m.group(1) if m else None


def normalise_formula(f):
    """Parse a formula such as C2H4O into Hill string (also handles a leading multiplier)."""
    if f is None:
        return None
    counts = {}
    for el, n in re.findall(r"([A-Z][a-z]?)(\d*)", f):
        counts[el] = counts.get(el, 0) + (int(n) if n else 1)
    return hill(counts)


def read_geometry(path):
    atoms = []
    with open(path, newline="") as fh:
        for row in csv.DictReader(fh):
            sym = row["atom"].strip()
            atoms.append((sym[0].upper() + sym[1:].lower(), float(row["x"]), float(row["y"]), float(row["z"])))
    if not atoms:
        raise SystemExit("geometry file has no atoms")
    return atoms


def perceive(atoms, charge, mult):
    """Split the system into species. Returns a list of dicts (never raises on chemistry)."""
    species = []
    xyz = f"{len(atoms)}\n\n" + "\n".join(f"{a[0]} {a[1]} {a[2]} {a[3]}" for a in atoms)
    mol = Chem.MolFromXYZBlock(xyz)
    if mol is None:
        return None, "could not read the geometry"
    rdDetermineBonds.DetermineConnectivity(mol)
    frag_atoms = Chem.GetMolFrags(mol)
    frags = Chem.GetMolFrags(mol, asMols=True, sanitizeFrags=False)
    for k, (idx, f) in enumerate(zip(frag_atoms, frags), start=1):
        sp = dict(index=k, atoms=[i + 1 for i in idx], status="refused", route=None,
                  inchi="", inchikey="", reasons=[], notes=[], formula="", _mol=None, _skel=None)
        counts = {}
        for a in f.GetAtoms():
            counts[a.GetSymbol()] = counts.get(a.GetSymbol(), 0) + 1
        sp["formula"] = hill(counts)
        # an H with two neighbours is a hydrogen bond perceived as a bond
        bridging = [i for i in idx if mol.GetAtomWithIdx(i).GetSymbol() == "H"
                    and mol.GetAtomWithIdx(i).GetDegree() != 1]
        if bridging:
            sp["reasons"].append("hydrogen atom(s) %s do not have exactly one neighbour, so bond "
                                 "perception is not trustworthy for this geometry"
                                 % ",".join(str(i + 1) for i in bridging))
            species.append(sp)
            continue
        if len(frags) == 1:
            fch = charge
        else:
            fch = 0  # several species: only the neutral-species assignment is unambiguous
            if charge != 0:
                sp["reasons"].append("the system has total charge %d and several species; the charge "
                                     "per species cannot be assigned automatically" % charge)
                species.append(sp)
                continue
        if mult != 1:
            sp["reasons"].append("multiplicity %d: only closed-shell (singlet) species are handled" % mult)
            species.append(sp)
            continue
        try:
            rdDetermineBonds.DetermineBondOrders(f, charge=fch)
            Chem.SanitizeMol(f)
        except Exception as e:  # noqa: BLE001
            sp["reasons"].append("bond orders could not be assigned (%s)" % str(e).split("\n")[0][:80])
            species.append(sp)
            continue
        charged = [a.GetIdx() + 1 for a in f.GetAtoms() if a.GetFormalCharge() != 0]
        if charged and fch == 0:
            sp["reasons"].append("perception produced charged atoms for a neutral species "
                                 "(a zwitterion or a proton in transit); not a clean minimum")
            species.append(sp)
            continue
        if any(a.GetNumRadicalElectrons() for a in f.GetAtoms()):
            sp["reasons"].append("perception produced radical electrons")
            species.append(sp)
            continue
        Chem.AssignStereochemistryFrom3D(f)
        pot = Chem.FindPotentialStereo(f)
        unspec = [s for s in pot if s.specified == Chem.StereoSpecified.Unspecified]
        if unspec:
            sp["reasons"].append("stereo element(s) could not be assigned from the geometry")
            species.append(sp)
            continue
        sp["_mol"] = f
        sp["_has_stereo"] = len(pot) > 0
        species.append(sp)
    return species, None


def skeleton_inchi(mol):
    """InChI of the molecule with stereo removed - used to compare constitution only."""
    m = Chem.Mol(mol)
    m.RemoveAllConformers()  # otherwise InChI re-reads stereo from the 3D coordinates
    Chem.RemoveStereochemistry(m)
    return Chem.MolToInchi(m)


def attach_from_geometry(sp):
    f = sp["_mol"]
    inchi = Chem.MolToInchi(f)
    if not inchi:
        sp["reasons"].append("InChI generation failed")
        return
    sp.update(status="ok", route="generated", inchi=inchi, inchikey=Chem.InchiToInchiKey(inchi))
    if sp.get("_has_stereo"):
        sp["notes"].append("stereo layer comes from this geometry (the computed structure), not from a label")
    if re.search(r"\(H", inchi):
        sp["notes"].append("standard InChI treats mobile-H tautomers as one species")


def check_supplied(sp, claim):
    """User route: inchi (+ optional key) and/or smiles. Refuse on any problem."""
    why = []
    inchi = (claim.get("inchi") or "").strip()
    key = (claim.get("inchikey") or "").strip()
    smiles = (claim.get("smiles") or "").strip()
    cand = ""
    if smiles:
        m = Chem.MolFromSmiles(smiles)
        if m is None:
            why.append("the SMILES could not be parsed")
        else:
            if len(Chem.GetMolFrags(m)) > 1:
                why.append("the SMILES holds several components; give one species per entry")
            if any(a.GetFormalCharge() for a in m.GetAtoms()) or any(a.GetNumRadicalElectrons() for a in m.GetAtoms()):
                why.append("the SMILES has charges or radicals, which do not match a neutral closed-shell species")
            pot = Chem.FindPotentialStereo(m)
            missing = [s for s in pot if s.specified == Chem.StereoSpecified.Unspecified]
            if missing:
                kinds = sorted({str(s.type).split(".")[-1] for s in missing})
                why.append("the SMILES leaves %d stereo element(s) undefined (%s); a standard InChI made "
                           "from it would describe a mixture of stereoisomers, not your molecule"
                           % (len(missing), ", ".join(kinds)))
            if not why:
                cand = Chem.MolToInchi(m)
                if not cand:
                    why.append("InChI generation from the SMILES failed")
        if inchi and cand and inchi != cand:
            why.append("the supplied InChI differs from the one derived from the supplied SMILES")
    if inchi:
        if not INCHI_RE.match(inchi):
            why.append("the InChI is not in standard form (it must start 'InChI=1S/')")
        else:
            m2 = Chem.MolFromInchi(inchi)
            if m2 is None:
                why.append("the InChI could not be read")
            else:
                pot = Chem.FindPotentialStereo(m2)
                if any(s.specified == Chem.StereoSpecified.Unspecified for s in pot):
                    why.append("the InChI has stereo element(s) with no stereo layer; refused because it "
                               "would match more than one stereoisomer")
        cand = cand or inchi
    if not cand and not why:
        why.append("no InChI or SMILES supplied")
    if cand and not why:
        # key checks
        derived = Chem.InchiToInchiKey(cand)
        if key:
            if not KEY_RE.match(key):
                why.append("the InChIKey is not in the form XXXXXXXXXXXXXX-XXXXXXXXXX-X")
            elif key != derived:
                why.append("the supplied InChIKey does not match the InChI (expected %s)" % derived)
        # identity against the geometry
        fi = normalise_formula(formula_from_inchi(cand))
        if fi != sp["formula"]:
            why.append("formula %s does not match the geometry's species %s" % (fi, sp["formula"]))
        else:
            sk_claim = Chem.MolFromInchi(cand)
            if sk_claim is not None:
                a = skeleton_inchi(sk_claim)
                b = skeleton_inchi(sp["_mol"])
                if a != b:
                    why.append("same formula, different connectivity: this is a different isomer from the one "
                               "in the geometry")
        if not why:
            sp.update(status="ok", route="supplied" if inchi else "from SMILES", inchi=cand, inchikey=derived)
            if sp.get("_has_stereo") and "/t" not in cand and "/b" not in cand:
                sp.update(status="refused", inchi="", inchikey="")
                why.append("the geometry has stereo elements but the identifier has no stereo layer")
            if sp["status"] == "ok" and sp.get("_has_stereo"):
                # compare stereo with the geometry's own
                if Chem.MolToInchi(sp["_mol"]) != cand:
                    sp.update(status="refused", inchi="", inchikey="")
                    why.append("stereochemistry differs from the geometry (a different stereoisomer or enantiomer)")
    if why:
        sp.update(status="refused", inchi="", inchikey="")
        sp["reasons"].extend(why)


def claim_formula(c):
    """Return (formula or None, [problems]) for a claim, before it is matched to a species."""
    prob = []
    inchi = (c.get("inchi") or "").strip()
    smiles = (c.get("smiles") or "").strip()
    f = None
    if inchi:
        if not INCHI_RE.match(inchi):
            prob.append("the InChI is not in standard form (it must start 'InChI=1S/')")
        else:
            f = normalise_formula(formula_from_inchi(inchi))
    if smiles:
        m = Chem.MolFromSmiles(smiles)
        if m is None:
            prob.append("the SMILES could not be parsed")
        elif len(Chem.GetMolFrags(m)) > 1:
            prob.append("the SMILES holds several components; give one species per entry")
        else:
            fs = normalise_formula(CalcMolFormula(Chem.AddHs(m)))
            if f and fs != f:
                prob.append("the SMILES formula %s and the InChI formula %s disagree" % (fs, f))
            f = f or fs
    if not inchi and not smiles:
        prob.append("the entry has neither an InChI nor a SMILES")
    return f, prob


def run(atoms, charge, mult, claims):
    species, err = perceive(atoms, charge, mult)
    if err:
        return dict(species=[], claims=[], overall="refused", error=err)
    cl = []
    for i, c in enumerate(claims, start=1):
        f, prob = claim_formula(c)
        cl.append(dict(entry=i, formula=f, matched=[], status="refused", reasons=list(prob)))
    for sp in species:
        if sp["_mol"] is None:
            continue
        mine = [i for i, c in enumerate(cl) if c["formula"] == sp["formula"]]
        if mine:
            c = cl[mine[0]]
            c["matched"].append(sp["index"])
            check_supplied(sp, claims[mine[0]])
        else:
            attach_from_geometry(sp)
            if claims and sp["status"] == "ok":
                sp["route"] = "generated"
                sp["notes"].append("no identifier was supplied for this species, so it was generated from the geometry")
    for c in cl:
        sps = [s for s in species if s["index"] in c["matched"]]
        if c["reasons"]:
            pass
        elif not sps:
            c["reasons"].append("formula %s matches none of the species in this geometry (%s)"
                                % (c["formula"], ", ".join(sorted({s["formula"] for s in species}))))
        elif all(s["status"] == "ok" for s in sps):
            c["status"] = "ok"
        else:
            c["reasons"].append("refused for the species it was matched to (see that species)")
    if cl and any(c["status"] != "ok" for c in cl):
        # a supplied identifier was wrong: do not paper over it with a generated one
        for sp in species:
            if sp["status"] == "ok":
                sp.update(status="refused", inchi="", inchikey="")
                sp["reasons"].append("withheld: an identifier you supplied for this system was refused; "
                                     "nothing is written for the system until every supplied identifier passes")
    ok = [s for s in species if s["status"] == "ok"]
    overall = "ok" if len(ok) == len(species) else ("refused" if not ok else "partly refused")
    for sp in species:
        for k in ("_mol", "_has_stereo", "_skel"):
            sp.pop(k, None)
    return dict(species=species, claims=cl, overall=overall)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--geometry", required=True)
    ap.add_argument("--charge", type=int, default=0)
    ap.add_argument("--mult", type=int, default=1)
    ap.add_argument("--claims")
    ap.add_argument("--out", help="write JSON here (default: print)")
    ap.add_argument("--tsv", help="also write tables: <prefix>.species.tsv and <prefix>.claims.tsv")
    a = ap.parse_args()
    if not HAVE_RDKIT:
        print(json.dumps(dict(species=[], overall="refused",
                              error="RDKit is not installed (conda install -c conda-forge rdkit); "
                                    "nothing can be verified, so nothing is written")))
        sys.exit(2)
    claims = []
    if a.claims:
        if a.claims.endswith(".tsv"):
            with open(a.claims, newline="") as fh:
                claims = [{k: (v or "") for k, v in r.items()} for r in csv.DictReader(fh, delimiter="\t")]
        else:
            claims = json.load(open(a.claims))
    res = run(read_geometry(a.geometry), a.charge, a.mult, claims)
    if a.tsv:
        with open(a.tsv + ".species.tsv", "w", newline="") as fh:
            w = csv.writer(fh, delimiter="\t")
            w.writerow(["index", "atoms", "formula", "status", "route", "inchi", "inchikey", "message", "overall"])
            for s in res["species"]:
                w.writerow([s["index"], ",".join(map(str, s["atoms"])), s["formula"], s["status"], s["route"] or "",
                            s["inchi"], s["inchikey"], " | ".join(s["reasons"] + s["notes"]), res["overall"]])
            if not res["species"]:
                w.writerow(["", "", "", "refused", "", "", "", res.get("error", ""), res["overall"]])
        with open(a.tsv + ".claims.tsv", "w", newline="") as fh:
            w = csv.writer(fh, delimiter="\t")
            w.writerow(["entry", "formula", "status", "message"])
            for c in res.get("claims", []):
                w.writerow([c["entry"], c["formula"] or "", c["status"], " | ".join(c["reasons"])])
    txt = json.dumps(res, indent=1)
    if a.out:
        open(a.out, "w").write(txt)
    elif not a.tsv:
        print(txt)


if __name__ == "__main__":
    main()
