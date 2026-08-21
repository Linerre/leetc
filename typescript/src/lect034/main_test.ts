import { assert, assertFalse, assertExists, assertEquals, assertNotEquals  } from 'jsr:@std/assert';
import {
    ListNode,
    getIntersectionNode,
} from './main.ts';

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
function makeListFromArray(nums: number[]): ListNode | null {
    if (nums.length < 1) return null;

    let head = new ListNode(0, null);
    let h: ListNode | null  = head;
    for (let i = 0; i < nums.length - 1; i++) {
        if (i === 0) {
            head.val = nums[i];
            continue;
        }
        h.next = new ListNode(nums[i], null);
        h = h.next;
    }
    return head;
}

function intersect(
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

function skipToNode(head: ListNode | null, skip: number): number | null {
    if (head == null) return null;

    let a = head;
    let cnt = 0;
    while (cnt < skip && a.next) {
        a = a.next;
        cnt++;
    }
    return a.val;
}

Deno.test('Test getIntersectionNode 1', () => {
    const listA = [4,1,8,4,5];
    const listB = [5,6,1,8,4,5];
    const skipA = 2;
    const skipB = 3;
    const hA = makeListFromArray(listA);
    const hB = makeListFromArray(listB);
    assertExists(hA);
    assertExists(hB);
    const stopA = skipToNode(hA, skipA);
    const stopB = skipToNode(hB, skipB);
    assertEquals(stopA, stopB);
    // intersect
    assert(intersect(hA,hB,skipA,skipB));
    const intersection = getIntersectionNode(hA, hB);
    // console.log(intersection);
    assertExists(intersection);
    assertEquals(intersection.val, stopA);
    assertEquals(intersection.val, stopB);
})

Deno.test('Test getIntersectionNode 2', () => {
    const listA = [1,9,1,2,4], listB = [3,2,4];
    const skipA = 3, skipB = 1;
    const hA = makeListFromArray(listA);
    const hB = makeListFromArray(listB);
    assertExists(hA);
    assertExists(hB);
    const stopA = skipToNode(hA, skipA);
    const stopB = skipToNode(hB, skipB);
    assertEquals(stopA, stopB);
    // intersect
    assert(intersect(hA,hB,skipA,skipB));
    const intersection = getIntersectionNode(hA, hB);
    // console.log(intersection);
    assertExists(intersection);
    assertEquals(intersection.val, stopA);
    assertEquals(intersection.val, stopB);
})

Deno.test('Test getIntersectionNode 3', () => {
    const listA = [2,6,4], listB = [1,5];
    const skipA = 3, skipB = 2;
    const hA = makeListFromArray(listA);
    const hB = makeListFromArray(listB);
    assertExists(hA);
    assertExists(hB);
    const stopA = skipToNode(hA, skipA);
    const stopB = skipToNode(hB, skipB);
    assertNotEquals(stopA, stopB);
    // intersect
    assertFalse(intersect(hA,hB,skipA,skipB));
    const intersection = getIntersectionNode(hA, hB);
    // console.log(intersection);
    assert(intersection === null, 'Expected intersection to be null but got non-null value');
})
