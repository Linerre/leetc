// Hard  726: https://leetcode.cn/problems/number-of-atoms/
export function countOfAtoms(formula: string): string {
  const [atoms, _] = parse(formula.split(''), 0);
  // need to sort the key-val pairs in the map
  const con: string[] = [];
  atoms.forEach((v,k) => con.push(k + (v > 1 ? v : '')));
  con.sort();
  return con.join('');
}

// In general, there are two types of info to collect:
// 1. Atom (either a single uppercase letter or a combination of upper and lower case numbers)
// 2. Sub Atom, which is 1 in parenthesis
// type 1 goes into top level map and type 2 goes into a tem map merged into top level map later
function parse(
  formula: string[],
  i: number,
): [Map<string, number>, number] {
  // top level map each atom to its appearance time
  const atoms = new Map<string, number>();
  // sub map the sub combination to its appearance time
  let sub: Map<string, number> | null = new Map<string, number>();
  let cnt = 0;
  let name = '';
  while (i < formula.length && formula[i] !== ')') {
    const c = formula[i].charCodeAt(0);
    if ((c >= 65 && c <= 90) || c === 40) { // A-Z or left paren
      // this indicates the time to merge the sub and clean the cache
      merge(atoms, sub, name, cnt);
      name = '';
      sub = null;
      cnt = 0;
      if (c >= 65 && c <= 90) {
        // A-Z
        name = name.concat(formula[i]);
        i++;
      } else {
        // left paren
        const [newSub, pos] = parse(formula, i + 1);
        sub = newSub;
        i = pos + 1;
      }
    } else if (c >= 97 && c <= 122) { // a-z
      name = name.concat(formula[i]);
      i++;
    } else { // subscript number
      cnt = cnt * 10 + (c - 48);
      i++;
    }
  }
  // merge again to include the possible non-recursive tail
  merge(atoms, sub, name, cnt);
  return [atoms, i];
}

function merge(
  atoms: Map<string, number>,
  sub: Map<string, number> | null,
  name: string,
  cnt: number,
): void {
  // only when there is a new Atom (name not empty) or the sub is non-null, will we merge
  const factor = cnt === 0 ? 1 : cnt;
  if (name.length > 0) {
    atoms.set(name, (atoms.get(name) ?? 0) + factor);
  }

  if (sub) {
    sub.forEach((v,k) => {
      const av = atoms.get(k) ?? 0;
      atoms.set(k, av + v * (cnt === 0 ? 1 : cnt))
    })
  }
}
