import { assertEquals } from '@std/assert/equals';
import { decodeString } from './decode-string.ts';

Deno.test({
  name: 'Test decode string 1',
  timeout: 1000,
  fn: () => {
    const input = '3[a]2[bc]';
    const output = 'aaabcbc';
    assertEquals (decodeString(input), output);
  }
});

Deno.test({
  name: 'Test decode string 2',
  timeout: 1000,
  fn: () => {
    const input = '3[a2[c]]';
    const output = 'accaccacc';
    assertEquals (decodeString(input), output);
  }
});

Deno.test({
  name: 'Test decode string 3',
  timeout: 1000,
  fn: () => {
    const input = '2[abc]3[cd]ef';
    const output = 'abcabccdcdcdef';
    assertEquals (decodeString(input), output);
  }
});
