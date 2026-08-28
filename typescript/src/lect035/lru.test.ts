import { assertEquals } from "@std/assert/equals";
import { LRUCache } from "./lru.ts";

Deno.test({
  name: "Test LRUCache",
  timeout: 3000,
  fn: () => {
    const lru = new LRUCache(2);
    lru.put(1, 1);
    lru.put(2, 2);
    assertEquals(lru.get(1), 1, "Expected 1 but got a different value");
    lru.put(3, 3);
    assertEquals(lru.get(2), -1, "Expected not found");
    lru.put(4, 4);
    assertEquals(lru.get(1), -1, "Expected not found");
    assertEquals(lru.get(3), 3, "Expected 3");
    assertEquals(lru.get(4), 4, "Expected 4");
  },
});
