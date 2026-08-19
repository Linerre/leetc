// Medium 29: https://leetcode.cn/problems/divide-two-integers/description/
// Use of Math.pow() is prohibited as it is a built-in fn performing exponentiation/multiplication.
// So we hardcoded the MAX and MIN

const MIN = -2_147_483_648; // -Math.pow(2, 31)
const MAX = 2_147_483_647;  // Math.pow(2,31) - 1

export function divide(a: number, b: number): number {
    if (a === MIN && b === MIN) return 1;
    if (a !== MIN && b !== MIN) return div(a, b);
    // a is not MIN
    if (b === MIN) return 0;
    // a is MIN and b is -1;
    if (b === neg(1)) return MAX;
    // a === MIN && b !== MIN && b !== -1
    let m = add(a, b > 0 ? b : neg(b));
    let quot = div(m, b);
    let offset = b > 0 ? neg(1) : 1;
    return add(quot, offset);
}

export function add(a: number, b: number): number {
    let sum = 0;
    while (b !== 0) {
        sum = a ^ b;
        b = (a & b) << 1;
        a = sum;
    }
    return sum;
}

export function sub(a: number, b: number): number {
    return add(a, neg(b));
}

// a and b must not be the minumum integer
export function div(a: number, b: number): number {
    let x = a < 0 ? neg(a) : a;
    let y = b < 0 ? neg(b) : b;
    let quot = 0;
    for (let i = 30; i >= 0; i = sub(i, 1)) {
        if ((x >> i) >= y) {
            quot = quot | (1 << i);
            x = sub(x, y << i);
        }
    }
    return (a < 0 && 0 < b) || (b < 0 && 0 < a) ? neg(quot) : quot;
}

function neg(n: number): number {
    return add(~n, 1);
}

export function multi(a: number, b: number): number {
    let prod = 0;
    // zero-fill shift b right until it becomes 0
    while (b !== 0) {
        if ((b & 1) !== 0) {
            prod = add(prod, a);
        }
        a = a << 1;
        b = b >>> 1;
    }
    return prod;
}
