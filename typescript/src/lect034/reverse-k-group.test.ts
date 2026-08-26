import { assert, assertExists, assertEquals } from 'jsr:@std/assert';
import { reverseKGroup } from './reverse-k-group.ts';
import { ListNode, makeListFromArray, listToArray } from './util.ts';

Deno.test({
    name: 'Test reverseKGroup 1',
    timeout: 5000,
    fn: () => {
        const input = [1,2,3,4,5];
        const output = [2,1,4,3,5];
        const k = 2;
        const head = makeListFromArray(input);
        assertEquals(listToArray(head), input);

        const newHead = reverseKGroup(head, k);
        assertEquals(listToArray(newHead), output);
    },
});

Deno.test({
    name: 'Test reverseKGroup 2',
    timeout: 5000,
    fn: () => {
        const input = [1,2,3,4,5];
        const output = [3,2,1,4,5];
        const k = 3;
        const head = makeListFromArray(input);
        assertEquals(listToArray(head), input);

        const newHead = reverseKGroup(head, k);
        assertEquals(listToArray(newHead), output);
    },
});

Deno.test({
    name: 'Test reverseKGroup 3',
    timeout: 5000,
    fn: () => {
        const input = [1,2,3,4,5,6,7];
        const output = [1,2,3,4,5,6,7];
        const k = 1;
        const head = makeListFromArray(input);
        assertEquals(listToArray(head), input);

        const newHead = reverseKGroup(head, k);
        assertEquals(listToArray(newHead), output);
    },
});
