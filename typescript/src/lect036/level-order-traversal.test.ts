import { assertEquals } from '@std/assert/equals';
import { levelOrder1, levelOrder2 } from './level-order-traversal.ts';
import {
  type NodeVal,
  makeBTreeFromArray,
} from './util.ts';

Deno.test({
  name: 'Test levelOrder 1',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [3,9,20,null,null,15,7];
    const root = makeBTreeFromArray(input);
    const output = [[3],[9,20],[15,7]];
    assertEquals(levelOrder1(root), output);
    assertEquals(levelOrder2(root), output);
  }
});

Deno.test({
  name: 'Test levelOrder 2',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [1];
    const root = makeBTreeFromArray(input);
    const output = [[1]];
    assertEquals(levelOrder1(root), output);
    assertEquals(levelOrder2(root), output);
  }
})

Deno.test({
  name: 'Test levelOrder 3',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [3,7,11,-2,null,8,25,9,null,null,0,-101,4];
    const root = makeBTreeFromArray(input);
    const output = [[3], [7,11], [-2,8,25], [9,0,-101,4]];
    assertEquals(levelOrder1(root), output);
    assertEquals(levelOrder2(root), output);
  }
})
