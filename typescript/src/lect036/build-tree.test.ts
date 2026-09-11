import { assertEquals } from '@std/assert/equals';
import { buildTree } from './build-tree.ts';
import { treeToArray } from './util.ts';


Deno.test({
  name: 'Test buildTree',
  timeout: 1000,
  fn: () => {
    const preOrder1 = [3,9,20,15,7];
    const inOrder1 = [9,3,15,20,7];
    const output1 = [3,9,20,null,null,15,7];
    const head1 = buildTree(preOrder1, inOrder1);
    assertEquals(treeToArray(head1), output1);

    const preOrder2 = [-1];
    const inOrder2 = [-1];
    const output2 = [-1];
    const head2 = buildTree(preOrder2, inOrder2);
    assertEquals(treeToArray(head2), output2);
  }
});


