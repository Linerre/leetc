/**
 * (For a quicksort pivot between `left` and `right` inclusive, use:
 * Math.floor(left + Math.random() * (right - left + 1)))
 * Medium 912: https://leetcode.cn/problems/sort-an-array/description/
 */
export function quickSort(nums: number[], l: number, r: number): void {
    // invalid range: either 1 number or no number
    if (l >= r) return;

    // Randomly selected a number from the given range [l...r]
    // r - l + 1 = length of the range,
    // random fn * range to get a random number of [0...len-1]
    const x = nums[Math.floor(l + Math.random() * (r - l + 1))];

    // const mid = partition1(nums, l, r, x);
    // mid = a - 1 will remain unchanged so move right bound left by 1 number
    // quickSort(nums, l, mid - 1);
    // quickSort(nums, mid + 1, r);

    const { left, right } = partition2(nums, l, r, x);
    quickSort(nums, l, left - 1);
    quickSort(nums, right + 1, r);
}

// Modify the array in place so that in range [l...r]:
// 1. [l...a-1] contains numbers <= x (randomly selected)
// 2. [a...r] contains numbers > x (randomly selected)
// return the mid position for recursion
function partition1(nums: number[], l: number, r: number, x: number): number {
    let a = l;                  // a marks the boundary where nums[a] > x
    let xi = 0;                 // also remember any index where nums[xi] = x
    for (let i = l; i <= r; i++) {
        if (nums[i] <= x) {
            swap(nums, a, i);
            if (nums[a] === x)
                xi = a;

            // extend the range by moving a forward
            a++;
        }
        // if current num > x, move i forward only
    }

    // put x at the last in the range where all nums <= x
    swap(nums, xi, a - 1);
    return a - 1;
}

function partition2(nums: number[], l: number, r: number, x: number): Boundary {
    let left = l;
    let right = r;
    let i = l;
    while (i <= right) {
        if (nums[i] === x) {
            i++;
        } else if (nums[i] < x) {
            swap(nums, left, i);
            left++;
            i++;
        } else {
            swap(nums, right, i);
            right--;
        }
    }
    return { left, right };
}

function swap(nums: number[], i: number, j: number): void {
    // if i === j, can return early
    const tmp = nums[i];
    nums[i] = nums[j];
    nums[j] = tmp;
}

// Mark the left and right boundaries after each partition2 so that
// Next recursions start on correctly ranges
interface Boundary {
    left: number;
    right: number;
}
