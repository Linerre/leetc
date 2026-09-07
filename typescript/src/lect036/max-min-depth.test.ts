import { assertEquals } from '@std/assert/equals';
import { maxDepth, minDepth } from './max-min-depth.ts';
import { makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test maxDepth',
  timeout: 1000,
  fn: () => {
    const input1 = [3,9,20,null,null,15,7];
    const root1 = makeBTreeFromArray(input1);
    assertEquals(maxDepth(root1), 3);

    const input2 = [1,null,2];
    const root2 = makeBTreeFromArray(input2);
    assertEquals(maxDepth(root2), 2);
  }
});

Deno.test({
  name: 'Test minDepth',
  timeout: 1000,
  fn: () => {
    const input1 = [3,9,20,null,null,15,7];
    const root1 = makeBTreeFromArray(input1);
    assertEquals(minDepth(root1), 2);

    const input2 = [2,null,3,null,4,null,5,null,6];
    const root2 = makeBTreeFromArray(input2);
    assertEquals(minDepth(root2), 5);
  }
})
