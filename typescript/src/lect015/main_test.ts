import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { MinStack  } from './main.ts';

Deno.test('MinStack', () => {
    const ms = new MinStack();
    ms.push(-2);
    ms.push(0);
    ms.push(-3);
    assertEquals(ms.getMin(), -3);
    ms.pop();
    assertEquals(ms.top(), 0);
    assertEquals(ms.getMin(), -2);
})
