import { assertEquals } from '@std/assert/equals';
import { Stack } from '@datastructures-js/stack';
import { sort } from './sort-stack-recursive.ts';

Deno.test({
  name: 'Test sort 1',
  timeout: 1000,
  fn: () => {
    const input = [1,5,4,5,3,2,3,1,4,2];
    const sorted = input.toSorted((a, b) => a - b);
    const output: number[] = [];
    const stack = new Stack(input);
    sort(stack);
    while (!stack.isEmpty()) {
      output.push(stack.pop() ?? 0);
    }
    assertEquals(output, sorted);
  }
});
