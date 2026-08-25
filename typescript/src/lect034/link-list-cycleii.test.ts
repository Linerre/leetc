import { assertExists, assertEquals, assertFalse } from 'jsr:@std/assert';
import { detectCycle } from './link-list-cycleii.ts';
import { ListNode, makeListFromArrayWithRing } from './util.ts';

Deno.test({
    name: 'Test detectcycle 1',
    timeout: 3000,
    fn: () => {
        const list = [3,2,0,-4];
        const pos = 1;
        const head = makeListFromArrayWithRing(list, pos);
        const ring = detectCycle(head);
        assertExists(ring);
        assertEquals(ring.val, 2);
    }
});

Deno.test({
    name: 'Test detectcycle 2',
    timeout: 3000,
    fn: () => {
        const list = [1,2];
        const pos = 0;
        const head = makeListFromArrayWithRing(list, pos);
        const ring = detectCycle(head);
        assertExists(ring);
        assertEquals(ring.val, 1);
    }
});

Deno.test({
    name: 'Test detectcycle 2',
    timeout: 3000,
    fn: () => {
        const list = [1];
        const pos = -1;
        const head = makeListFromArrayWithRing(list, pos);
        const ring = detectCycle(head);
        assertFalse(ring);
    }
});
