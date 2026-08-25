import { ListNode } from './util.ts';

/**
 * A simpler solution is to use a hash set to store each node reference.
 * The first duplicate reference to be stored is the entry node.
 * That will require extra space O(n).
 */
export function detectCycle(head: ListNode | null): ListNode | null {
    if (head === null || head.next === null || head.next.next === null)
        return null;

    let slow: ListNode | null = head.next;
    let fast: ListNode | null = head.next.next;
    // step 1: fast and slow meet in the circle (if any) and fast goes back to head
    while (slow !== fast) {
        if (fast && fast.next === null) return null;
        if (fast && fast.next && fast.next.next === null) return null;

        slow = slow ? slow.next : null;
        fast = fast && fast.next ? fast.next.next : null;
    }
    fast = head;

    // step 2: both fast and slow move 1 node each time until they meet again
    // the node they meet is the entry node of the circle
    while (fast && slow && fast !== slow) {
        fast = fast.next;
        slow = slow.next;
    }
    return slow;
};
