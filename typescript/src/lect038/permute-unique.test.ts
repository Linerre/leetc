import { assertEquals } from '@std/assert';
import { permuteUnique } from './permute-unique.ts';

Deno.test({
  name: 'Test permuteUnique 1',
  timeout: 1000,
  fn: () => {
    const input = [1,1,2];
    const output = [[1,1,2],[1,2,1],[2,1,1]];
    assertEquals(permuteUnique(input), output);
  }
});

Deno.test({
  name: 'Test permuteUnique 2',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3];
    const output = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,2,1], [3,1,2]];
    assertEquals(permuteUnique(input), output);
  }
});
