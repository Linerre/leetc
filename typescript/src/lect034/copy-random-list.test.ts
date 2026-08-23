import { assert, assertEquals, assertExists } from 'jsr:@std/assert';
import { copyRandomList } from './copy-random-list.ts';
import {
    type NodePairs,
    _Node,
    makeListWithRandomFromArray,
    printListWithRandom,
} from './util.ts';

Deno.test({
    name: 'Test copyRandomList 1',
    timeout: 5000,
    fn: () => {
        const pairs: NodePairs = [[7,null],[13,0],[11,4],[10,2],[1,0]];
        const head = makeListWithRandomFromArray(pairs);
        const copyHead = copyRandomList(head);
        assertEquals(printListWithRandom(head), pairs);
        assertEquals(printListWithRandom(copyHead), pairs);
    }
});

Deno.test({
    name: 'Test copyRandomList 2',
    timeout: 5000,
    fn: () => {
        const pairs: NodePairs = [[1,1],[2,1]];
        const head = makeListWithRandomFromArray(pairs);
        const copyHead = copyRandomList(head);
        assertEquals(printListWithRandom(head), pairs);
        assertEquals(printListWithRandom(copyHead), pairs);
    }
});

Deno.test({
    name: 'Test copyRandomList 3',
    timeout: 5000,
    fn: () => {
        const pairs: NodePairs = [[3,null],[3,0],[3,null]];
        const head = makeListWithRandomFromArray(pairs);
        const copyHead = copyRandomList(head);
        assertEquals(printListWithRandom(head), pairs);
        assertEquals(printListWithRandom(copyHead), pairs);
    }
});
