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
        assertEquals(printListWithRandom(head), pairs);
    }
});
