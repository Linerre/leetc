import { assertEquals } from 'jsr:@std/assert';
import { quickSort } from './quick-sort.ts';

Deno.test('Test quickSort case-1', () => {
    const nums = [1,3,5,2,4,6];
    // console.log('x:', nums[0 + Math.floor((Math.random() * (5 - 0 + 1)))])
    quickSort(nums, 0, nums.length-1);
    assertEquals(nums, [1,2,3,4,5,6]);
})

Deno.test('Test quickSort case-2', () => {
    const nums = [1,3,5,2,4,6];
    // console.log('x:', nums[0 + Math.floor((Math.random() * (5 - 0 + 1)))])
    quickSort(nums, 0, nums.length-1);
    assertEquals(nums, [1,2,3,4,5,6]);
})

Deno.test('Test quickSort case-2', () => {
    const nums = [5,1,1,2,0,0];
    // console.log('x:', nums[0 + Math.floor((Math.random() * (5 - 0 + 1)))])
    quickSort(nums, 0, nums.length-1);
    assertEquals(nums, [0,0,1,1,2,5]);
})
