import { assert, assertFalse } from '@std/assert';
import { isValidBST } from './is-valid-bst.ts';
import { makeBTreeFromArray } from '../lect036/util.ts';


Deno.test({
  name: 'Test isValidBST 1',
  timeout: 1000,
  fn: () => {
    const input = [2,1,3];
    const root = makeBTreeFromArray(input);
    assert(isValidBST(root));
  }
});

Deno.test({
  name: 'Test isValidBST 2',
  timeout: 1000,
  fn: () => {
    const input = [5,1,4,null,null,3,6];
    const root = makeBTreeFromArray(input);
    assertFalse(isValidBST(root));
  }
});
