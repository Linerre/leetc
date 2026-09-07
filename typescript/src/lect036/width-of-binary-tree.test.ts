import { assertEquals } from '@std/assert/equals';
import { widthOfBinaryTree } from './width-of-binary-tree.ts';
import { makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test widthOfBinaryTree',
  timeout: 1000,
  fn: () => {
    const input1 = [1,3,2,5,3,null,9];
    const r1 = makeBTreeFromArray(input1);
    assertEquals(widthOfBinaryTree(r1), 4);

    const input2 = [1,3,2,5,null,null,9,6,null,7];
    const r2 = makeBTreeFromArray(input2);
    assertEquals(widthOfBinaryTree(r2), 7);

    const input3 = [1,3,2,5];
    const r3 = makeBTreeFromArray(input3);
    assertEquals(widthOfBinaryTree(r3), 2);
  }
});
