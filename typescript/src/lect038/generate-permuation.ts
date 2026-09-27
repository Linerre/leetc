
export function generatePermuation(s: string): string[] {
  const path: string[] = [];
  const set = new Set<string>();
  f1(s, 0, path, set);

  return set.values().toArray();
}

function f1(s: string, i: number, path: string[], set: Set<string>): void {
  if (i === s.length) {
    set.add(path.join(''));
  } else {
    path.push(s.charAt(i));
    f1(s, i + 1, path, set);
    path.pop();
    f1(s, i + 1, path, set);
  }
}
