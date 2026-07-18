import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { smallSum } from './main.ts';

Deno.test('Test smallSum case-1', () => {
    const nums = [1,3,5,2,4,6];
    const result = smallSum(nums, 0, nums.length -1);
    assertEquals(nums, [1,2,3,4,5,6]);
    assertEquals(result, 27);
})

Deno.test('Test smallSum case-2', () => {
    const nums = [2,1,6,4,5,3,7,8];
    const expect = (1+2)+(1+2)+(1+2+4)+(1+2)+(1+2+6+4+5+3)+(1+2+6+4+5+3+7);
    const result = smallSum(nums, 0, nums.length -1);
    assertEquals(nums, [1,2,3,4,5,6,7,8]);
    assertEquals(result, expect);
})
