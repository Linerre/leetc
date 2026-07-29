import { assertEquals } from 'jsr:@std/assert';
import { sortArray, sortArray2 } from './main.ts';

Deno.test('Heap sort 1', () => {
    const nums1 = [5,2,3,1];
    const nums2 = [5,1,1,2,0,0];
    assertEquals(sortArray(nums1), [1,2,3,5]);
    assertEquals(sortArray(nums2), [0,0,1,1,2,5]);
})


Deno.test('Heap sort 2', () => {
    const nums1 = [5,2,3,1];
    const nums2 = [5,1,1,2,0,0];
    assertEquals(sortArray2(nums1), [1,2,3,5]);
    assertEquals(sortArray2(nums2), [0,0,1,1,2,5]);
})
