import { assertEquals } from '@std/assert';
import { findNumber } from './basic-binary-search.ts';
import { findNumberLinear, randomArray } from './util.ts';

Deno.test({
  name: 'Test findNumber 1',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort();
    assertEquals(findNumber(nums, target), findNumberLinear(nums, target));
  }
});
