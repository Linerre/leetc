import { PriorityQueue } from '@datastructures-js/priority-queue';
/**
 * Definition for singly-linked list.
 */
export class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

// Hard 23: https://leetcode.cn/problems/merge-k-sorted-lists/
export function mergeKLists(lists: Array<ListNode | null>): ListNode | null {
    // Basically a small-root heap
    const heap = new PriorityQueue<ListNode>((a, b) => a.val - b.val);
    lists.forEach((node) => { if (node) heap.enqueue(node); });

    if (heap.isEmpty()) return null;

    const h: ListNode | null = heap.dequeue();
    let p: ListNode | null = h;

    if (p && p.next) heap.enqueue(p.next);

    while (!heap.isEmpty()) {
        const curr: ListNode | null = heap.dequeue();
        // Link curr to previous node and move previous to current
        if (p) p.next = curr;
        p = curr;
        if (curr && curr.next) heap.enqueue(curr.next);
    }
    return h;
};

/**
 *************************************************************
 * Implement a heap (small-root) and use it as priority queue
 *************************************************************
 */

/**
 * Re-arrange an array into a heap where any substree has smallest number at top
 * @param: nums the array that represents a heap
 * @param: i the index at which the number to be inserted
 */
export function heapInsert(nums: number[], i: number): void {
    while (nums[i] < nums[Math.floor((i-1)/2)]) {
        swap(nums, i, Math.floor((i-1)/2));
        i = Math.floor((i-1)/2);
    }
}

/**
 * Push smaller number up to maintain the large-root heap structure
 * @param: nums the array the represents a heap
 * @param: i index of current number
 * @param: size the array/heap size
 */
export function heapify(nums: number[], i: number, size: number): void {
    let l = i * 2 + 1;          // left child
    while (l < size) {
        // Has left child and right child
        // Decide which one is smaller
        let best = l + 1 < size && nums[l + 1] < nums[l] ? l + 1 : l;
        // Decide if best child is larger than current number
        best = nums[best] < nums[i] ? best : i;
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
