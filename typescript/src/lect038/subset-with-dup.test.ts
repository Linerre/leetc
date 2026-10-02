import { assertEquals } from '@std/assert';
import { subsetsWithDup } from './subset-with-dup.ts';

Deno.test({
  name: 'test subsetWithDup',
  timeout: 1000,
  fn: () => {
    const nums = [1,2,2];
    const output = [[],[2],[2,2],[1],[1,2],[1,2,2]];
    assertEquals(subsetsWithDup(nums), output);
  }
});

Deno.test({
  name: 'Test subsetWithDup 2',
  timeout: 1000,
  fn: () => {
    const nums = [0];
    const output = [[],[0]];
    assertEquals(subsetsWithDup(nums), output);
  }
});
