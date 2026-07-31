import { assertEquals } from 'jsr:@std/assert';
import {
    heapInsert,
    heapify,
    sortArray,
    sortArray2
} from './main.ts';

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

Deno.test('Heap insert', () => {
    const nums = [1,2,3,4,6];
    heapInsert(nums, 4);
    console.log(nums);
})

Deno.test('Heapify', () => {
    const nums = [1,2,4,3,6,5];
    heapify(nums, 2, 6);
    console.log(nums);
})
