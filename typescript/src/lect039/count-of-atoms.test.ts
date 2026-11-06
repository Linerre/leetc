import { assertEquals } from '@std/assert/equals';
import { countOfAtoms } from './count-of-atoms.ts';

Deno.test({
  name: 'Test countOfAtoms 1',
  timeout: 1000,
  fn: () => {
    const formula = 'H2O';
    const output = 'H2O';
    assertEquals(countOfAtoms(formula), output);
  }
});

Deno.test({
  name: 'Test countOfAtoms 2',
  timeout: 1000,
  fn: () => {
    const formula = 'Mg(OH)2';
    const output  = 'H2MgO2';
    assertEquals(countOfAtoms(formula), output);
  }
});

Deno.test({
  name: 'Test countOfAtoms 3',
  timeout: 1000,
  fn: () => {
    const formula = 'K4(ON(SO3)2)2';
    const output = 'K4N2O14S4';
    assertEquals(countOfAtoms(formula), output);
  }
});
