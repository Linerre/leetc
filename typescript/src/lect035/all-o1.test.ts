import { assertEquals } from '@std/assert/equals';
import { AllOne } from './all-o1.ts';

Deno.test({
  name: 'Test AllOne 1',
  timeout: 1000,
  fn: () => {
    const all1 = new AllOne();
    all1.inc('hello');
    all1.inc('hello');
    assertEquals(all1.getMaxKey(), 'hello');
    assertEquals(all1.getMinKey(), 'hello');
    all1.inc('leet');
    assertEquals(all1.getMaxKey(), 'hello');
    assertEquals(all1.getMinKey(), 'leet');
  }
});

Deno.test({
  name: 'Test AllOne 2',
  timeout: 1000,
  fn: () => {
    const all1 = new AllOne();
    all1.inc('a');
    all1.inc('b');
    all1.inc('c');
    all1.inc('d');
    all1.inc('e');
    all1.inc('f');
    all1.inc('g');
    all1.inc('h');
    all1.inc('i');
    all1.inc('j');
    all1.inc('k');
    all1.inc('l');
    
    all1.dec('a');
    all1.dec('b');
    all1.dec('c');
    all1.dec('d');
    all1.dec('e');
    all1.dec('f');

    all1.inc('g');
    all1.inc('h');
    all1.inc('i');
    all1.inc('j');
    
    assertEquals(all1.getMaxKey(), 'g', 'wrong max key');
    assertEquals(all1.getMinKey(), 'k', 'wrong min key');
    all1.inc('k');
    all1.inc('l');
    assertEquals(all1.getMaxKey(), 'g');
    assertEquals(all1.getMinKey(), 'g');
    all1.inc('a');
    all1.dec('j');
    assertEquals(all1.getMaxKey(), 'g');
    assertEquals(all1.getMinKey(), 'a');
  }
})
