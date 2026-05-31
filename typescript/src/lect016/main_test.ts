import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { CircularDequeu } from './main.ts';

Deno.test('Test circular deque 1', () => {
    const cd = new CircularDequeu(3);
    assert(cd.insertLast(1));
    assert(cd.insertLast(2));
    assert(cd.insertFront(3));
    assertFalse(cd.insertFront(4));
    assertEquals(cd.getRear(), 2);
    assert(cd.isFull());
    assert(cd.deleteLast());
    assert(cd.insertFront(4));
    assertEquals(cd.getFront(), 4);
})

Deno.test('Test circular deque 2', () => {
    const cd = new CircularDequeu(2);
    assert(cd.insertFront(7));
    assert(cd.deleteLast());
    assertEquals(cd.getFront(), -1);
    assert(cd.insertLast(5));
    assert(cd.insertFront(0));
    assertEquals(cd.getFront(), 0);
    assertEquals(cd.getRear(), 5);
    assertEquals(cd.getFront(), 0);
    assertEquals(cd.getFront(), 0);
    assertEquals(cd.getRear(), 5);
    assertFalse(cd.insertLast(0));
})
