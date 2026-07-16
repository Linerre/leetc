// Medium 912: https://leetcode.cn/problems/sort-an-array/
const MAXN = 50001;
const help: number[] = new Array(MAXN);

export function sortArray(nums: number[]): number[] {
    if (nums.length > 1) {
        // nums passed as a reference becuase Array is object
        mergeSort1(nums);
    }
    return nums;
}

// T: O(n * logn)
// S: O(n)
export function sortArray2(nums: number[]): number[] {
    if (nums.length > 1) {
        // nums passed as a reference becuase Array is object
        mergeSort2(nums);
    }
    return nums;
}

// Recursive implementation
function mergeSort1(nums: number[]): void {
    sort(nums, 0, nums.length - 1);
}

// Non-recursive implemention
function mergeSort2(nums: number[]): void {
    const n = nums.length;
    let l = 0;
    let r, m;
    for (let step = 1; step < n; step <<= 1) {
        l = 0;
        while (l < n) {
            m = l + step - 1;
            // if right part is nothing, break
            if (m + 1 >= n) break;
            // r can be either l + 2*step - 1 or n - 1
            r = Math.min(l + (step << 1) - 1, n - 1);
            merge(nums,l,m,r);
            // go to next step
            l = r + 1;
        }

    }
}


// Recursively partition the array into two parts and sort each part accordingly
function sort(nums: number[], l: number, r: number): void {
    // only return when the sub-array has exactly one element
    if (l === r) return;

    const m = Math.floor((l + r) / 2) ;
    sort(nums, l, m);
    sort(nums, m + 1, r);         // r will be reduced to as small as l
    merge2(nums, l, m, r);
}

function merge(nums: number[], l: number, m: number, r: number): void {
    let i = l;
    let a = l;                // start pointer of left part
    let b = m + 1;            // start pointer of right part

    // Only when both pointers are in boundary
    while (a <= m && b <= r) {
        // fill in help array from original array (devided into 2 parts)
        help[i++] = nums[a] <= nums[b] ? nums[a++] : nums[b++];
    }
    // When pointer of right part goes out of boundary first, copy over rest of left
    while (a <= m) {
        help[i++] = nums[a++];
    }
    // When pointer of left part goes out of boundary first, copy over rest of right
    while (b <= r) {
        help[i++] = nums[b++];
    }
    // Update the original array with the sorted result of range [l...r]
    for (i = l; i <= r; i++)
        nums[i] = help[i];
}

function merge2(nums: number[], l: number, m: number, r: number): void {
    // Copy ONLY left part [l..m] to help
    for (let i = l; i <= m; i++) {
        help[i] = nums[i];
    }

    let a = l;      // pointer in help (left part)
    let b = m + 1;  // pointer in nums (right part)
    let i = l;      // current position in nums

    // Merge back to nums
    while (a <= m && b <= r) {
        if (help[a] <= nums[b]) {
            nums[i++] = help[a++];
        } else {
            nums[i++] = nums[b++];
        }
    }

    // Only need to handle leftover left elements.
    // Right part is already in place if left exhausts first.
    while (a <= m) {
        nums[i++] = help[a++];
    }
}
