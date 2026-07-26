import { assertEquals } from 'jsr:@std/assert';
import { findKthLargest } from './main.ts';

Deno.test('Test findKthLargest 1', () => {
    const nums = [3,2,1,5,6,4];
    assertEquals(findKthLargest(nums, 2), 5);
})


Deno.test('Test findKthLargest 1', () => {
    const nums = [3,2,3,1,2,4,5,5,6];
    assertEquals(findKthLargest(nums, 4), 4);
})
