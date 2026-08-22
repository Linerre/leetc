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

/**
 * A navie, stupid implementation that only updates head's next in each loop
 * function makeListFromArray(nums: number[]): ListNode | null {
 *     const head =  new ListNode(0, null);
 *     nums.reduce(
 *         (accNode, num) => {
 *             accNode!.next = new ListNode(num, null);
 *             return accNode;
 *         },
 *         head
 *     );
 *     console.log(head);
 *     return head ? head.next : null;
 * }
 */

// Helpers to verify correctness
export function makeListFromArray(nums: number[]): ListNode | null {
    if (nums.length < 1) return null;

    let head = new ListNode(0, null);
    let h: ListNode | null  = head;
    for (let i = 0; i < nums.length; i++) {
        if (i === 0) {
            head.val = nums[i];
            continue;
        }
        h.next = new ListNode(nums[i], null);
        h = h.next;
    }
    return head;
}

export function intersect(
    headA: ListNode | null,
    headB: ListNode | null,
    skipA: number,
    skipB: number
): boolean {
    if (headA === null || headB === null) return false;

    let a = headA;
    let b = headB;
    let skip = 0;
    // At most a moves to the last node
    while (skip < skipA && a.next) {
        a = a.next;
        skip++;
    }
    skip = 0;
    while (skip < skipB - 1 && b.next) {
        b = b.next;
        skip++;
    }

    // no intersection
    if (b.next === null) return false;

    // intersect at the node with the same value. A: 1->2->5 A: 4->7->5
    // if skipA === skipB === 3, then the two lists will still
    // intersect even your intention is different. To pretend they are
    // not intersected, skipA must not equal skipB. This has nothing to
    // do with the solution, just to make the simulation correct.
    if (a.val === b.next.val) {
        b.next = a;
        return true;
    } else {
        return false;
    }
}

export function skipToNode(head: ListNode | null, skip: number): number | null {
    if (head == null) return null;

    let a = head;
    let cnt = 0;
    while (cnt < skip && a.next) {
        a = a.next;
        cnt++;
    }
    return a.val;
}

export function printList(head: ListNode | null): number[] {
    if (!head) return [];
    const vals: number[] = [];
    while(head) {
        vals.push(head.val);
        head = head.next;
    }
    return vals;
}
