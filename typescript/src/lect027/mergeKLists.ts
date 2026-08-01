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
