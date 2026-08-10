// RadixSort

const BASE = 10;
const MAXN = 5000;
const help = Array(MAXN);
const cnts = Array(BASE);

// Return number of digits of the given number according to BASE
function digits(n: number): number {
    let digits = 0;
    while (n > 0) {
        digits++;
        n = Math.floor(n / BASE);
    }
    return digits;
}

/**
 * @param nums the array to be sorted
 * @param n array length
 * @param digits of the max number in array in BASE
 */
function radixSort(num: number[], n: number, digits: number): void {
    // Loop through each digit in the number
    for (let offset = 1;  digits > 0; offset *= BASE, digits--) {
        cnts.fill(0);
        // extract a specific digit in ith number and
        // count the total times it appears. For example
        // (30 / 1) % 10 == 0
        // (120 / 1) % 10 == 0
        // Thus at offset 1, digit 0 appears twice
        // (30 / 10) % 10 == 3
        // (120 / 10) % 10 == 2
        // Thus at offset 10, digit 3 appears once
        // Thus at offset 10, digit 2 appears once
        for (let i = 0; i < n; i++) {
            const d = Math.floor(nums[i] / offest) % BASE;
            cnt[d] += 1;
        }
        // Add up prefixes. For example,
        // if at digit 1, 0 appears 10 times and at digit 2, 3 appear twice,
        // then increase the time of appearance for 3 to 13
        // That is to say, at any digit, the cnt[i] means the total counts of numbers <= cnts[i]
        for (let i = 0; i < BASE; i++) {
            cnts[i] = cnts[i] + cnts[i-1];
        }
        // Iterate in reverse order to put numbers in nums to help
        for (let i = n - 1; i >=0; i--) {
            const d = Math.floor(nums[i] / offest) % BASE;
            cnts[d] -= 1;
            help[cnts[d]] = nums[i];
        }

        // Overwrite nums with help to reflect the sorted array
        for (let i = 0; i < n; i++) {
            nums[i] = help[i];
        }
    }
}
