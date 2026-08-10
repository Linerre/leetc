// RadixSort

const BASE = 10;
const MAXN = 5000;
const help: number[] = Array(MAXN);
const cnts: number[] = Array(BASE);

export function sortArray(nums: number[]): number[] {
    if (nums.length > 1) {
		const n = nums.length;
        // find the min in array
		let min = nums[0];
		for (let i = 1; i < n; i++) {
			min = Math.min(min, nums[i]);
		}
		let max = 0;
		for (let i = 0; i < n; i++) {
            // make each number in nums non-negative
			nums[i] -= min;
			// find the max
			max = Math.max(max, nums[i]);
		}
		// max decides the digits within BASE
		radixSort(nums, n, digits(max));
		// restore the nums
		for (let i = 0; i < n; i++) {
			nums[i] += min;
		}
	}
	return nums;
}

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
function radixSort(nums: number[], n: number, digits: number): void {
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
            const d = Math.floor(nums[i] / offset) % BASE;
            cnts[d] += 1;
        }
        // Add up prefixes. For example,
        // if at digit 1, 0 appears 10 times and at digit 2, 3 appear twice,
        // then increase the time of appearance for 3 to 13
        // That is to say, at any digit, the cnt[i] means the total counts of numbers <= cnts[i]
        for (let i = 1; i < BASE; i++) {
            cnts[i] = cnts[i] + cnts[i-1];
        }
        // console.debug('cnts', cnts);
        // Iterate in reverse order to put numbers in nums to help
        for (let i = n - 1; i >=0; i--) {
            // Put ith number in nums to cnts[d] - 1 in help;
            // Then decrease cnts[d]
            // This is equivalent to decreasing cnts[d] and then putting ith number in nums to help
            const d = Math.floor(nums[i] / offset) % BASE;
            cnts[d] -= 1;
            help[cnts[d]] = nums[i];
        }

        // Overwrite nums with help to reflect the sorted array
        for (let i = 0; i < n; i++) {
            nums[i] = help[i];
        }
    }
}
