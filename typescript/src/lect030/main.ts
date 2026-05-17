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
    const c = a - b;
    const sa = signBit(a);
    const sb = signBit(b);
    const sc = signBit(c);

    // diff == 1 then a and b have different signs; otherwise diff == 0 indicates the opposite
    const diff = sa ^ sb;
    // same == 1 then a and b have the same sign; othersiw same == 0 indicates the opposite
    const same = flip(diff);

    // Return a in either of the following conditions:
    // 1. When a and b have opposite signs and a is non-negative
    // 2. When a and b have the same sign and c is non-negative
    const returnA = diff * sa + same * sc;
    const returnB = flip(returnA);
    return a * returnA + b * returnB;
}

// 268: https://leetcode.com/problems/missing-number/description/
export function missingNumber(nums: number[]): number {
    let xorAll = 0;
    let xorHas = 0;
    for (let i = 0; i < nums.length; i++) {
        xorAll ^= i;
        xorHas ^= nums[i];
    }
    xorAll ^= nums.length;
    return xorAll ^ xorHas;
}

// 136: https://leetcode.com/problems/single-number
export function singleNumber(nums: number[]): number {
    const xor = 0;
    // for (const num of nums) xor ^= num;
    return nums.reduce((acc, num) => acc ^ num, xor);
}

// 260: https://leetcode.com/problems/single-number-iii/
export function doubleNumber(nums: number[]): number[] {
    const xor = 0;
    // calculate xor sum of the array, including the two target nums, e.g a, b
    // this essentially gives a ^ b, which must has at least 1 bit of 1
    const xorSum1 = nums.reduce((acc, num) => acc ^ num, xor);

    // extract the rightmost bit that is 1
    // Brian Kernighan
    const rone = xorSum1 & (-xorSum1);

    let xorSum2 = 0;
    // calculate xor sum of the array, excluding the two target nums
    // this essentially gives either a or b
    for (let num of nums) {
        // xor with nums whose same bit position as that of xorSum1 is NOT 1 (i.e. 0)
        // thus, xxxxx0xx & 00000100 = 0
        if ((num & rone) == 0) {
            xorSum2  ^= num;
        }
    }
    return [xorSum2, xorSum1 ^ xorSum2];
}


// Helper
function flip(n: number): number {
    return n ^ 1;
}

// Get the sign bit of the passed number: 1 means non-negative and 0 means negative
function signBit(n: number): number {
    return flip(n >>> 31);
}
