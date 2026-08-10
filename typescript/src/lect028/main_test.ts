import { assertEquals } from 'jsr:@std/assert';
import { sortArray } from './main.ts';

Deno.test('Radix Srot 1', () => {
    const nums1 = [5,2,3,1];
    const nums2 = [5,1,1,2,0,0];
    const nums3 = [14, 2, 19, 7, 11, 20, 1, 16, 5, 18, 3, 9, 12, 6, 17, 4, 10, 15, 8, 13];
    assertEquals(sortArray(nums1), [1,2,3,5]);
    assertEquals(sortArray(nums2), [0,0,1,1,2,5]);
    assertEquals(
        sortArray(nums3),
        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
    );
})
