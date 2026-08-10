import { assert, assertFalse } from 'jsr:@std/assert';
import {
    powerOfTwo,
    powerOfThree,
} from './main.ts';


Deno.test('Test powerOfTwo', () => {
    const n1 = 16;
    const n2 = 18;
    const n3 = 256;
    const n4 = 100;
    assert(powerOfTwo(n1));
    assert(powerOfTwo(n3));
    assertFalse(powerOfTwo(n2));
    assertFalse(powerOfTwo(n4));
})


Deno.test('Test powerOfThree', () => {
    const n1 = 9;
    const n2 = 81;
    const n3 = 39;
    const n4 = 66;
    assert(powerOfThree(n1));
    assert(powerOfThree(n2));
    assertFalse(powerOfThree(n3));
    assertFalse(powerOfThree(n4));
})
