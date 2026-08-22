import { ListNode } from './util.ts';

// Simple 160: https://leetcode.cn/problems/intersection-of-two-linked-lists/
export function getIntersectionNode(headA: ListNode | null, headB: ListNode | null): ListNode | null {
    if (headA == null || headB == null) return null;

    // Find the length diff of two lists
    let a = headA;
    let b = headB;
    let diff: number = 0;

    // At this place, a can't be null but we force compiler to skip null-checking
    // Same goes for b and through to the end
    while (a.next !== null) {
        a = a.next;
        diff++;
    }

    while (b.next !== null) {
        b = b.next;
        diff--;
    }

    // If two lists intersect, they must share the same ListNode at the end
    if (a !== b) return null;

    // Let a be the longer list and b the shorter one
    if (diff >= 0) {
        a = headA;
        b = headB;
    } else {
        a = headB;
        b = headA;
    }

    diff = Math.abs(diff);
    // Move `diff` on the longer list so that we have the same length left as the shorter
    while (diff !== 0 && a.next) {
        a = a.next;
        diff--;
    }

    // Now find where a and b intersects
    while (a !== b && a.next && b.next) {
        a = a.next;
        b = b.next;
    }

    return a;
}
