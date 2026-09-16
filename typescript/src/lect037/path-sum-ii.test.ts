import { assertEquals } from '@std/assert/equals';
import { pathSum } from './path-sum-ii.ts';
import { makeBTreeFromArray } from '../lect036/util.ts';

Deno.test({
  name: 'Test pathSum 1',
  timeout: 1000,
  fn: () => {
    const input = [5,4,8,11,null,13,4,7,2,null,null,5,1];
    const output = [[5,4,11,2],[5,8,4,5]];
    const targetSum = 22;
    const root = makeBTreeFromArray(input);
    assertEquals(pathSum(root, targetSum), output);
  }
});


Deno.test({
  name: 'Test pathSum 2',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3];
    const targetSum = 5;
    const output: number[][] = [];
    const root = makeBTreeFromArray(input);
    assertEquals(pathSum(root, targetSum), output);
  }
});


Deno.test({
  name: 'Test pathSum 3',
  timeout: 1000,
  fn: () => {
    const input = [1,2];
    const targetSum = 0;
    const output: number[][] = [];
    const root = makeBTreeFromArray(input);
    assertEquals(pathSum(root, targetSum), output);
  }
});
