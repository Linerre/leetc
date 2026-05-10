import { assertEquals, assertFalse } from "jsr:@std/assert";
import { swap, getMax1, getMax2 } from './main.ts';

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
