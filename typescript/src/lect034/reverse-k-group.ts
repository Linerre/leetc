import { ListNode } from './util.ts';

// Hard 25: https://leetcode.cn/problems/reverse-nodes-in-k-group/description/
export function reverseKGroup(head: ListNode | null, k: number): ListNode | null {
    if (head === null) return null;

    let start = head;           // start narrowed to ListNode
    let end = groupEnd(start, k);
    // Fewer than k nodes, no reverse
    if (end === null) return head;

    // Remember the end of 1st group as it becomes new head of the entire list
    head = end;
    reverse(start, end);
    // Now start becomes the real end of 1st group
    let lastGroupEnd = start;
    while (lastGroupEnd && lastGroupEnd.next !== null) {
        start = lastGroupEnd.next;
        end = groupEnd(start, k);
        if (end === null) return head;
        reverse(start, end);
        // connect last group end to the new start of current group
        lastGroupEnd.next = end;
        // update last group end to be the end of current group
        lastGroupEnd = start;
    }
    return head;
}

// Find the k-group end with start node `start`
function groupEnd(start: ListNode, k: number): ListNode | null {
    let s: ListNode | null = start;
    // start with s, move k - 1 times
    // s will never ever be null as k <= n (total number of nodes in the list)
    while(k - 1 > 0 && s) {
        s = s.next;
        k--;
    }
    return s;
}

// Given a list with start and end, reverse it so that
// end becomes new start and start becomes new end. For example
// ...->a->b->c->d->... will become ...->d->c->b->a->...
function reverse(start: ListNode, end: ListNode): void {
    // Remember the next of old end
    const rest = end.next;
    let cur: ListNode | null = start;
    let prev: ListNode | null = null;
    let next: ListNode | null = null;
    while (cur && cur !== rest ) {
        next = cur!.next;
        // In the first loop, start/a next will become null
        // Since start never changes, we connect it to rest in the end
        cur.next = prev;
        prev = cur;
        cur = next;
    }
    start.next = rest;
}
