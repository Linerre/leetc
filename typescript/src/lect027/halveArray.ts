import { PriorityQueue } from '@datastructures-js/priority-queue';

/**
 **************************************************
 ****************** Use PriorityQueu **************
 **************************************************
 */

// Medium 2208: https://leetcode.cn/problems/minimum-operations-to-halve-array-sum/description/
export function halveArray1(nums: number[]): number {
    // Max priority queue
    const heap = new PriorityQueue<number>((a, b) => b - a);
    let sum = 0;
    nums.forEach((num) => {
        heap.enqueue(num);
        sum += num;
    });

    // Reduce by goal
    const goal = sum / 2;
    let count = 0;
    for (let amt = 0, cur: number | null; amt < goal; amt = cur !== null ? amt + cur : amt + 0, count++) {
        cur = heap.dequeue();
        if (cur) {
            cur = cur / 2;
            heap.enqueue(cur);
        }
    }
    return count;
}


/**
 **************************************************
 ****************** Use heap **********************
 **************************************************
 */

export function halveArray2(nums: number[]): number {
    const heap: number[] = nums.map(num => num << 20);
    const size = heap.length;
    // Build heap bottom up
    for (let i = size - 1; i >= 0; i--) {
        heapify(heap, i);
    }
    const sum = heap.reduce((a, b) => a + b, 0);
    const goal = sum / 2;
    let count = 0;
    for (let amt = 0; amt < goal; count++) {
        heap[0] = heap[0] / 2;
        amt += heap[0];
        heapify(heap, 0);
    }
    return count;
}

/**
 * Push smaller number up to maintain the large-root heap structure
 * @param: nums the array the represents a heap
 * @param: i index of current number
 * @param: size the array/heap size
 */
export function heapify(nums: number[], i: number): void {
    const size = nums.length;
    let l = i * 2 + 1;          // left child
    while (l < size) {
        // Has left child and right child
        // Decide which one is smaller
        let best = l + 1 < size && nums[l + 1] > nums[l] ? l + 1 : l;
        // Decide if best child is larger than current number
        best = nums[best] > nums[i] ? best : i;
        if (best === i) break;
        // push up smaller one
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
