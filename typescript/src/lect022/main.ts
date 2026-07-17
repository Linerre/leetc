const MAXN = 1000;
const help: number[] = Array(MAXN);

// Return smallSum of range [l...r] and sort the range
export function smallSum(nums: number[], l: number, r: number): number {
    if (l === r) return 0;      // base case

    const m = Math.floor((l + r) / 2);
    // For base case, this is 0 + 0 + smallSum(0,1)
    return smallSum(nums, l, m) + smallSum(nums, m+1, r) + merge(nums, l, m, r);
}

// Return smallSum of cross range [l...m] and [m+1...r] and make them sorted
function merge(nums: number[], l: number, m: number, r: number): number {
    let result = 0;
    let sum = 0;
    let j = m + 1;
    // Calculate cross-range smallSum first
    // i points to start of left part and j points to start of right part
    for (let i = l, j = m + 1, sum = 0; j <= r; j++) {
        while (i <= m && nums[i] <= nums[j])
            sum += nums[i++];   // add to sum and inc i

        result += sum;
    }

    // Merge to make the cross-range sorted
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

    return result;
}
