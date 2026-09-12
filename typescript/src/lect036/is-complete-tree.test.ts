import { assert, assertFalse } from '@std/assert';
import { isCompleteTree } from './is-complete-tree.ts';
import { makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test isCompleteTree 1',
  timeout: 1000,
  fn: () => {
    const input1 = [1,2,3,4,5,6];
    const head1 = makeBTreeFromArray(input1);
    assert(isCompleteTree(head1));
  }
});

Deno.test({
  name: 'Test isCompleteTree 2',
  timeout: 1000,
  fn: () => {
    const input2 = [1,2,3,4,5,null,7];
    const head2 = makeBTreeFromArray(input2);
    assertFalse(isCompleteTree(head2));
  }
});

Deno.test({
  name: 'Test isCompleteTree 3',
  timeout: 1000,
  fn: () => {
    const input3 = [1,2,3,5,null,7,8];
    const head3 = makeBTreeFromArray(input3);
    assertFalse(isCompleteTree(head3));
  }
});
