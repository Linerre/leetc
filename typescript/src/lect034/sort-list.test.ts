import { assertExists, assertEquals, assertFalse } from 'jsr:@std/assert';
import { sortList } from './sort-list.ts';
import { ListNode, makeListFromArray, listToArray } from './util.ts';

Deno.test({
    name: 'Test sortList 1',
    timeout: 3000,
    fn: () => {
        const input = [-1,5,3,4,0];
        const output = [-1,0,3,4,5];
        const head = makeListFromArray(input);
        assertExists(head);

        const sortedHead = sortList(head);
        assertExists(sortedHead);
        assertEquals(listToArray(sortedHead), output);
    }
});

Deno.test({
    name: 'Test sortList 2',
    timeout: 3000,
    fn: () => {
        const input = [4,2,1,3];
        const output = [1,2,3,4];
        const head = makeListFromArray(input);
        assertExists(head);

        const sortedHead = sortList(head);
        assertExists(sortedHead);
        assertEquals(listToArray(sortedHead), output);
    }
});

Deno.test({
    name: 'Test sortList 2',
    timeout: 3000,
    fn: () => {
        const input: number[] = [];
        const output: number[] = [];
        const head = makeListFromArray(input);
        assertFalse(head);

        const sortedHead = sortList(head);
        assertFalse(sortedHead);
        assertEquals(listToArray(sortedHead), output);
    }
});
