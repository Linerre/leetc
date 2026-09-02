import { assertEquals } from '@std/assert/equals';
import { levelOrder1, levelOrder2 } from './level-order-traversal.ts';
import {
  type NodeVal,
  TreeNode,
  makeBTreeFromArray,
} from './util.ts';

Deno.test({
  name: 'Test levelOrder1',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [3,9,20,null,null,15,7];
    const root = makeBTreeFromArray(input);
    const output = [[3],[9,20],[15,7]];
    assertEquals(levelOrder1(root), output);
    assertEquals(levelOrder2(root), output);
  }
});
