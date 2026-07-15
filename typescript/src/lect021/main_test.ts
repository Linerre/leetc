import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { sortArray } from './main.ts';

Deno.test('Test merge sort 1-1', () => {
    const nums = [5,2,3,1];
    const sorted = sortArray(nums);
    assertEquals(sorted, [1,2,3,5]);
})


Deno.test('Test merge sort 1-2', () => {
    const nums = [5,1,1,2,0,0];
    const sorted = sortArray(nums);
    assertEquals(sorted, [0,0,1,1,2,5]);
})
