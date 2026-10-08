import { assertEquals } from '@std/assert/equals';
import { Stack } from '@datastructures-js/stack';
import { reverse } from './reverse-stack-recursive.ts';

Deno.test({
  name: 'Test reverse',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3,4,5];
    const stack = new Stack<number>();
    for (const n of input) stack.push(n);
    reverse(stack);
    const output: number[] = [];
    while (!stack.isEmpty()) output.push(stack.pop() ?? 0);
    assertEquals(input, output);
  }
});
