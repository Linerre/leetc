
export function generatePermuation(s: string): string[] {
  const path: string[] = [];
  const set = new Set<string>();
  // f1(s, 0, path, set);
  f2(s, 0, 0, path, set);
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

function f2(s: string, i: number, size: number, path: string[], set: Set<string>): void {
  if (i === s.length) {
    set.add(path.slice(0, size).join(''));
  } else {
    path[size] = s.charAt(i);
    f2(s, i + 1, size + 1, path, set);
    f2(s, i + 1, size, path, set);
  }
}
