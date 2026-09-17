import { assert, assertFalse } from '@std/assert';
import { isBalanced } from './is-balance.ts';
import { makeBTreeFromArray } from '../lect036/util.ts';

Deno.test({
  name: 'Test isBalance 1',
  timeout: 1000,
  fn: () => {
    const input = [3,9,20,null,null,15,7];
    const root = makeBTreeFromArray(input);
    assert(isBalanced(root));
  }
});

Deno.test({
  name: 'Test isBalance 2',
  timeout: 1000,
  fn: () => {
    const input = [1,2,2,3,3,null,null,4,4]
    const root = makeBTreeFromArray(input);
    assertFalse(isBalanced(root));
  }
});

Deno.test({
  name: 'Test isBalance 3',
  timeout: 1000,
  fn: () => {
    const input: number[] = [];
    const root = makeBTreeFromArray(input);
    assert(isBalanced(root));
  }
});
