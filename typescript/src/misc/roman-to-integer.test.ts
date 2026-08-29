import { assertEquals } from '@std/assert/equals';
import { romanToInt } from './roman-to-integer.ts';

Deno.test('Test romanToIntegar', () => {
  const roman1 = 'III';
  const roman2 = 'LVIII';
  const roman3 = 'MCMXCIV';
  assertEquals(romanToInt(roman1), 3);
  assertEquals(romanToInt(roman2), 58);
  assertEquals(romanToInt(roman3), 1994);
});
