import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { sortArray, sortArray2 } from './main.ts';

Deno.test('Test merge sort 1-1', () => {
    const nums = [5,2,3,1];
    const sorted = sortArray(nums);
    assertEquals(sorted, [1,2,3,5]);
})


Deno.test('Test merge sort 1-2', () => {
    const nums = [5,1,1,2,0,0];
    const sorted = sortArray2(nums);
    assertEquals(sorted, [0,0,1,1,2,5]);
})

Deno.test('Test merge sort 2-1', () => {
    const nums = [6,2,3,3,4,6,9,4,7];
    const sorted = sortArray2(nums);
    assertEquals(sorted, [2,3,3,4,4,6,6,7,9]);
})

Deno.test('Test merge sort 2-2', () => {
    const nums = [5,1,1,2,0,0];
    const sorted = sortArray(nums);
    assertEquals(sorted, [0,0,1,1,2,5]);
})
