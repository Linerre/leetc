// Medium 912: https://leetcode.cn/problems/sort-an-array/
export function sortArray(nums: number[]): number[] {
    if (nums.length > 1)
        heapSort1(nums);

    return nums;
}

function heapSort1(nums: number[]): void {
    const n = nums.length;
    // Insert and adjust to maintain a large-root heap
    for (let i = 0; i < n; i++) {
        heapInsert(nums, i);
    }
    // Sort the heap by swapping largest (0) with last (n-1)
    let size = n;
    while (size > 1) {
        // size dec by 1 first before passing to swap
        swap(nums, 0, --size);
        heapify(nums, 0, size);
    }
}

function heapSort2(nums: number[]): void {

}

/**
 * Insert a number into heap and maintain any substree in such a way
 * that the largest number always sits at the top of the subtree
 * @param: nums the array that represents a heap
 * @param: i the index at which the number to be inserted
 */
function heapInsert(nums: number[], i: number): void {
    while (nums[i] > nums[Math.floor((i-1)/2)]) {
        swap(nums, i, Math.floor((i-1)/2));
        i = Math.floor((i-1)/2);
    }
}

/**
 * Push smaller number down to main the large-root heap structure
 * @param: nums the array the represents a heap
 * @param: i index of current number
 * @param: size the array/heap size
 */
function heapify(nums: number[], i: number, size: number): void {
    let l = i * 2 + 1;          // left child
    while (l < size) {
        // Has left child and right child
        // Decide which one is larger
        let best = l + 1 < size && nums[l + 1] > nums[l] ? l + 1 : l;
        // Decide if best child is larger than current number
        best = nums[best] > nums[i] ? best : i;
        if (best === i) break;
        // push down smaller one
        swap(nums, best, i);
        // Update current idx to best (one of left or right child) and continue
        i = best;
        l = i * 2 + 1;
    }
}

// Assume both i and j <= nums.length - 1
function swap(nums: number[], i: number, j: number): void {
    if (i === j || nums[i] === nums[j]) return;
    const temp = nums[i];
    nums[i] = nums[j];
    nums[j] = temp;
}
