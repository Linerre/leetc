import { assertEquals } from '@std/assert';
import { findNumber, findLeft, findRight, findPeakElement } from './basic-binary-search.ts';
import {
  findNumberLinear,
  findLeftLinear,
  findRightLinear,
  randomArray,
} from './util.ts';

Deno.test({
  name: 'Test findNumber',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort((a, b) => a - b);
    assertEquals(findNumber(nums, target), findNumberLinear(nums, target));
  }
});

Deno.test({
  name: 'Test findLeft',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort((a, b) => a - b);
    assertEquals(findLeft(nums, target), findLeftLinear(nums, target));
  }
});

Deno.test({
  name: 'Test findRight',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort();
    assertEquals(findRight(nums, target), findRightLinear(nums, target));
  }
})
