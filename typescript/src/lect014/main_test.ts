import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { StackQueue, QueueStack  } from './main.ts';


Deno.test('Test stack-based queue', () => {
    const sq = new StackQueue();
    sq.push(1);
    sq.push(2);
    assertEquals(sq.peek(), 1);
    assertEquals(sq.pop(), 1);
    assertFalse(sq.empty());
})

Deno.test('Test queue-based stack', () => {
    const qs = new QueueStack();
    qs.push(1);
    qs.push(2);
    assertEquals(qs.top(), 2);
    assertEquals(qs.pop(), 2);
    assertFalse(qs.empty());
})
