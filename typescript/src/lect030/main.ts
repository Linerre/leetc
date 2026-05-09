
// Works only when i != j
export function swap(arr: number[], i: number, j: number): void {
    if (i === j) return;
    arr[i] = arr[i] ^ arr[j];
    arr[j] = arr[i] ^ arr[j];
    arr[i] = arr[i] ^ arr[j];
}

// Return the max of the given 2 numbers without using any conditionals
// Vulnerable to overflow when arbitrary large numbers are passed
export function getMax1(a: number, b: number): number {
    const c = a - b;
    const returnA: number = signBit(c);
    const returnB: number = flip(returnA);
    return a * returnA + b * returnB;
}

export function getMax2(a: number, b: number): number {
    return 0;
}


// Helper
function flip(n: number): number {
    return n ^ 1;
}

// Get the sign bit of the passed number: 1 means non-negative and 0 means negative
function signBit(n: number): number {
    return flip(n >>> 31);
}
