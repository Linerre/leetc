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
    nums.sort((a, b) => a - b); // numeric sort, not lexicographic
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
    nums.sort((a, b) => a - b);
    assertEquals(findRight(nums, target), findRightLinear(nums, target));
  }
});


Deno.test({
  name: 'Test findPeakElement 1',
  timeout: 1000,
  fn: () => {
    const nums = [1,2,3,1];
    const output = 2;
    assertEquals(findPeakElement(nums), output);
  }
});

Deno.test({
  name: 'Test findPeakElement 2',
  timeout: 1000,
  fn: () => {
    const nums = [1,2,1,3,5,6,4];
    const output = 5;
    assertEquals(findPeakElement(nums), output);
  }
})
