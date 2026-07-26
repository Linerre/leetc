// Medium 215: https://leetcode.cn/problems/kth-largest-element-in-an-array/description/
// 1 <= k <= nums.length <= 105
export function findKthLargest(nums: number[], k: number): number {
    // When k == 1, find the largest number in the array
    // When k == 2, find the second largest number in the array
    // When k == nums.length, find the 0th
    return randomSelect(nums, nums.length - k);
}

// If sorted, what number will be at ith index
function randomSelect(nums: number[], i: number) {
    let result = 0;
    // Randomly selected a number from the given range [l...r]
    // r - l + 1 = length of the range,
    // random fn * range to get a random number of [0...len-1]

    for (let l = 0, r = nums.length - 1; l <= r;) {
        const x = nums[Math.floor(l + Math.random() * (r - l + 1))];
        const {left, right} = partition(nums, l, r, x);
        if (i < left) {
            r = left - 1;
        } else if (i > right) {
            l = right + 1;
        } else {
            result = nums[i];
            break;
        }
    }
    return result;
}

// Mark the left and right boundaries after each partition2 so that
// Next recursions start on correctly ranges
interface Boundary {
    left: number;
    right: number;
}

function partition(nums: number[], l: number, r: number, x: number): Boundary {
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
