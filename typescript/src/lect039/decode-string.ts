export function decodeString(s: string): string {
  const [_, str] = decode(s.split(''), 0);
  return str;
};

function decode(input: string[], i: number): [number, string] {
  let cnt = 0;
  let path: string = '';
  while (i < input.length && input[i] !== ']') {
    const c = input[i].charCodeAt(0);
    if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) { // a-zA-Z
      path = path.concat(input[i]);
      i++;
    } else if (c >= 48 && c <= 57) { // 0-9
      cnt = cnt * 10 + (c - 48);
      i++;
    } else { // see left bracket
      const [pos, subpath] = decode(input, i+1);
      path = path.concat(repeat(cnt, subpath));
      i = pos + 1;
      cnt = 0;
    }
  }
  return [i, path];
}

function repeat(cnt: number, str: string): string {
  let ret = '';
  for (let i = 0; i < cnt; i++) {
    ret = ret.concat(str);
  }
  return ret;
}

