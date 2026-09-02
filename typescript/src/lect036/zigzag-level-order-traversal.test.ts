import { assertEquals } from '@std/assert/equals';
import { zigzagLevelOrder } from './zigzag-level-order-traversal.ts';
import { type NodeVal, makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test zigzagLevelOrder 1',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [3,9,20,null,null,15,7];
    const output: number[][] = [[3],[20,9],[15,7]];
    const root = makeBTreeFromArray(input);
    assertEquals(zigzagLevelOrder(root), output);
  }
});

Deno.test({
  name: 'Test zigzagLevelOrder 2',
  timeout: 1000,
  fn: () => {
    const input: NodeVal[] = [3,7,11,-2,null,8,25,9,null,null,0,-101,4];
    const output = [[3], [11,7], [-2,8,25], [4,-101,0,9]];
    const root = makeBTreeFromArray(input);
    assertEquals(zigzagLevelOrder(root), output);
  }
})
