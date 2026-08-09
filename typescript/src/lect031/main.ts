export function powerOfTwo(num: number): boolean {
    // Brian Kernighan to extract rightmost 1
    return num > 0 && num == (num & -num);
}

export function powerOfThree(n: number): boolean {
    return n > 0 && 1162261467 % n == 0;
}
