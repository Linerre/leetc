import { assertEquals } from "jsr:@std/assert";
import { swap, getMax1 } from './main.ts';

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
