
export function calculate(expr: string): number {
  const [ans, _]  = parse(expr.split(''), 0);
  return ans;
}

function parse(expr: string[], i: number): [number, number] {
  let cur = 0;
  const nums: number[] = [];
  const ops: string[] = [];
  while (i < expr.length && expr[i] !== ')') {
    const c = expr[i].charCodeAt(0);
    if (c >= 48 && c <= 57) {              // '0'..'9'
      cur = cur * 10 + (c - 48);
      i++;
    } else if (c !== 40) { // c is not left paren so it must be an op
      push(nums, ops, cur, expr[i++]);
      cur = 0;                  // restore cur
    } else {
      const [val, stop] = parse(expr, i + 1);
      cur = val;
      i = stop + 1;
    }
  }
  push(nums, ops, cur, '+');
  return [compute(nums, ops), i];
}

function push(nums: number[], ops: string[], cur: number, op: string): void {
  const n = nums.length;
  if (n === 0 || ops.at(-1) === '+' || ops.at(-1) === '-') {
    // if ops is + or -, push them onto stack
    nums.push(cur);
    ops.push(op);
  } else {
    // op is * or /, calculate it and then push
    const topNum = nums.at(-1) ?? 0;
    const topOp = ops.at(-1);
    if (topOp === '*') {
      nums[n-1] = (topNum * cur);
    } else {
      nums[n-1] = (Math.trunc(topNum / cur));
    }
    ops[n-1] = (op);
  }
}

function compute(nums: number[], ops: string[]): number {
  const n = nums.length;
  let ret = nums.at(0) ?? 0;
  for (let i = 1; i < n; i++) {
    ret += ops[i-1] === '+' ? nums[i] : -nums[i];
  }
  return ret;
}
