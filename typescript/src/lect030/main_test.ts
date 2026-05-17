import {  assert, assertEquals, assertFalse } from "jsr:@std/assert";
import {
    swap,
    getMax1,
    getMax2,
    doubleNumber,
    missingNumber,
    singleNumber,
} from './main.ts';

Deno.test('Test swap elements', () => {
    const arr = [1, -3, 2, 5, 8, 10, 321, 9];
    swap(arr, 1, 4);
    assertEquals(arr[1], 8);
    assertEquals(arr[4], -3);
})

Deno.test('Test getMax1 without overlfow', () => {
    const a = 20;
    const b = -100;
    const max = getMax1(a, b);
    assertEquals(max, a);
})

Deno.test('Test getMax1 with overlfow', () => {
    // const a = Number.MIN_VALUE; // 5E-324
    const a = -10;
    const b = Number.MAX_VALUE; // 1.7976931348623157E+308
    const max = getMax1(a, b);
    assertFalse(max == b);
})

Deno.test('Test getMax2', () => {
    const a = -10;
    const b = Number.MAX_VALUE; // 1.7976931348623157E+308
    const max = getMax2(a, b);
    assertEquals(max, b);
})

Deno.test('Test missingNumber', () => {
    const nums1 = [3,0,1];
    assertEquals(missingNumber(nums1), 2);
    const nums2 = [0,1];
    assertEquals(missingNumber(nums2), 2);
    const nums3 = [9,6,4,2,3,5,7,0,1];
    assertEquals(missingNumber(nums3), 8);
})

Deno.test('Test singleNumber', () => {
    const nums1 = [2,2,1];
    const nums2 = [4,1,2,1,2];
    const nums3 = [1];
    assertEquals(singleNumber(nums1), 1);
    assertEquals(singleNumber(nums2), 4);
    assertEquals(singleNumber(nums3), 1);
})

Deno.test('Test doubleNumber', () => {
    const nums1 = [1,2,1,3,2,5];
    const nums2 = [-1,0];
    const nums3 = [0,1];
    const ret1 = doubleNumber(nums1);
    const ret2 = doubleNumber(nums2);
    const ret3 = doubleNumber(nums3);
    assertEquals(ret1.length, 2);
    assert(ret1.includes(3) && ret1.includes(5));
    assertEquals(ret2.length, 2);
    assert(ret2.includes(-1) && ret2.includes(0));
    assertEquals(ret3.length, 2);
    assert(ret3.includes(0) && ret3.includes(1));
})
