import { assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { Bitset } from './main.ts';

Deno.test('Test Bitset', () => {
    const bs = new Bitset(5);
    bs.fix(3);
    console.log(bs.toString());
    assertEquals(bs.toString(), '00010');
    bs.fix(1);
    assertEquals(bs.toString(), '01010');
    bs.flip();
    assertEquals(bs.toString(), '10101');
    assertFalse(bs.all());
    bs.unfix(0);
    assertEquals(bs.toString(), '00101');
    bs.flip();
    assertEquals(bs.toString(), '11010');
    assert(bs.one());
    bs.unfix(0);
    assertEquals(bs.toString(), '01010');
    assertEquals(bs.count(), 2);
    assertEquals(bs.toString(), '01010');

})
