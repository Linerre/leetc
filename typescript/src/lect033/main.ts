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
    return 0;
}

export function div(a: number, b: number): number {
    return 0;
}

function neg(n: number): number {
    return add(~n, 1);
}
