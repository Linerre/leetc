import { assertEquals } from '@std/assert';
import { permute } from './permute.ts';

Deno.test({
  name: 'Test permute 1',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3];
    const output = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,2,1], [3,1,2]];
    assertEquals(permute(input), output);
  }
});

Deno.test({
  name: 'Test permute 2',
  timeout: 1000,
  fn: () => {
    const input = [0,1];
    const output = [[0,1],[1,0]];
    assertEquals(permute(input), output);
  }
});

Deno.test({
  name: 'Test permute 2',
  timeout: 1000,
  fn: () => {
    const input = [1];
    const output = [[1]];
    assertEquals(permute(input), output);
  }
});
