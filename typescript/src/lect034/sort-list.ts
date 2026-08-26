import { ListNode, printNode } from './util.ts';

/**
 * Time O(n*logn), space O(1), stable
 * For a singly-linked list, a few common patterns as follows
 * node.next = someNode is to link two nodes
 * node.next = null is to break two nodes
 * someNode = node.next is to store/remember the next node
 */
export function sortList(head: ListNode | null): ListNode | null {
    if (head === null) return null;
    if (head && head.next === null) return head;

    let cnt = 0;
    let h: ListNode | null = head;
    while (h !== null) {
        cnt++;
        h = h.next;
    }
    let l1= null as ListNode | null,
        l2= null as ListNode | null,
        r1= null as ListNode | null,
        r2= null as ListNode | null,
        next = null as ListNode | null,
        lastGroupEnd = null as ListNode | null;

    for (let step = 1; step < cnt; step *= 2) {
        // first group is special as they decide the new list's head
        l1 = head;
        r1 = groupEnd(l1, step);
        l2 = r1?.next ?? null;
        if (l2 === null) return head;
        r2 = groupEnd(l2, step);
        next = r2?.next ?? null;
        if (r1) r1.next = null;
        if (r2) r2.next = null;
        let [start, end] = merge(l1,r1,l2,r2);
        head = start;
        lastGroupEnd = end;
        while (next !== null) {
            l1 = next;
            r1 = groupEnd(l1, step);
            l2 = r1?.next ?? null;
            if (l2 === null) {
                lastGroupEnd!.next = l1;
                break;
            }
            r2 = groupEnd(l2, step);
            next = r2?.next ?? null;
            if (r1) r1.next = null;
            if (r2) r2.next = null;
            [start, end] = merge(l1,r1,l2,r2);
            if (lastGroupEnd) lastGroupEnd.next = start;
            lastGroupEnd = end;
        }
    }

    return head;
};


// Count k nodes, starting with s (inclusive) and return the last non-null node
// if there are not enough k nodes, return last non-null node
function groupEnd(start: ListNode | null, k: number): ListNode | null {
    if (start === null) return null;

    let s: ListNode | null = start;
    k--;
    while(k > 0 && s.next) {
        s = s.next;
        k--;
    }
    return s;
}

// l1...r1->null, left part sorted
// l2...r2->null, right part sorted
// Merge the two parts to make the whole sorted and return start and end nodes
function merge(
    l1: ListNode | null,
    r1: ListNode | null,
    l2: ListNode | null,
    r2: ListNode | null
): [ListNode | null, ListNode | null] {
    let prev: ListNode | null = null;
    let start: ListNode | null = null;
    let end: ListNode | null = null;
    // Start with the node with the smallest value in both left and
    // right parts If two parts' first node have the samw value, start
    // with left part to maintain stability.
    if (l1 && l2 && l1.val <= l2.val) {
        start = l1;
        prev = l1;
        l1 = l1.next;
    } else if (l1 && l2) {
        start = l2;
        prev = l2;
        l2 = l2.next;
    }

    while (l1 && l2) {
        // keep moving prev forward (from left to right)
        if (l1.val <= l2.val) {
            prev!.next = l1;
            prev = l1;
            l1 = l1.next;
        } else {
            prev!.next = l2;
            prev = l2;
            l2 = l2.next;
        }
    }
    // if l1 arrives at r1 and l1.val <= l2.val still holds, l1 now
    // points to null and prev.next points to l2.  if l1 remains
    // non-null after the above while loop, that indicates at some
    // point, l1...r1 are all larger than l2...r2 from that point
    // foward. Then prev.next points to l1 and end points to r1.
    if (l1) {
        prev!.next = l1;
        end = r1;
    } else {
        prev!.next = l2;
        end = r2;
    }

    return [start, end];
}
