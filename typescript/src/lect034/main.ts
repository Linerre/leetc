
// Simple 160: https://leetcode.cn/problems/intersection-of-two-linked-lists/
class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}


function getIntersectionNode(headA: ListNode | null, headB: ListNode | null): ListNode | null {
    if (headA == null || headB == null) return null;

    // Find the length diff of two lists
    let a: ListNode | null = headA;
    let b: ListNode | null = headB;
    let diff: number = 0;

    // At this place, a can't be null but we force compiler to skip null-checking
    // Same goes for b and through to the end
    while (a!.next !== null) {
        a = a.next;
        diff++;
    }

    while (b!.next !== null) {
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
    while (diff !== 0) {
        a = a!.next;
        diff--;
    }

    // Now find where a and b intersects
    while (a !== b) {
        a = a!.next;
        b = b!.next;
    }

    return a;
}
