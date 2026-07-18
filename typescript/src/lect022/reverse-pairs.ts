const MAXN = 50000;
const help: number[] = Array(MAXN);

// Hard 493: https://leetcode.cn/problems/reverse-pairs/
export function reversePairs(nums: number[]): number {
    return count(nums, 0, nums.length - 1);
}

function count(nums: number[], l: number, r: number): number {
    if (l === r) return 0;

    const m = Math.floor((l + r)/2);
    return count(nums, l, m) + count(nums, m+1, r) + merge(nums, l, m, r);
}

function merge(nums: number[], l: number, m: number, r: number): number {
    // count
    let result = 0;
    // Iterate over left part and for each iteration
    // Move right pointer and take down the valid count
    // [l...m][m+1...r]
    for (let i = l, j = m + 1; i <= m; i++) {
        while (j <= r && nums[i] > nums[j] * 2)
            j++;

        result += j - m - 1;
    }

    // sort merge as usual
    let i = l;
    let a = l;                  // left start pointer
    let b = m+1;                // right start pointer

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
