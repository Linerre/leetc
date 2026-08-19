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
    return a < 0 || b < 0 ? neg(quot) : quot;
}

function neg(n: number): number {
    return add(~n, 1);
}
