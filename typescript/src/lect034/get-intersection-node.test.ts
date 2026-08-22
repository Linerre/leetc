import { assert, assertFalse, assertExists, assertEquals, assertNotEquals  } from 'jsr:@std/assert';
import { getIntersectionNode } from './get-intersection-node.ts';
import {
    intersect,
    makeListFromArray,
    skipToNode,
    ListNode,
} from './util.ts';

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
