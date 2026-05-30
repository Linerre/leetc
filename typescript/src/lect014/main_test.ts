import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { StackQueue  } from './main.ts';


Deno.test('Test stack-based queue', () => {
    const sq = new StackQueue();
    sq.push(1);
    sq.push(2);
    assertEquals(sq.peek(), 1);
    assertEquals(sq.pop(), 1);
    assertFalse(sq.empty());
})
