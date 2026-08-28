import { assertFalse } from "@std/assert/false";
import { assertEquals } from "@std/assert";
import { assert } from "@std/assert";
import { RandomizedCollection } from "./randomized-collection.ts";

Deno.test({
  name: "Test Randomized collection",
  timeout: 1000,
  fn: () => {
    const rc = new RandomizedCollection();
    assert(rc.insert(1));
    assertFalse(rc.insert(1));
    assert(rc.remove(1));
    assertEquals(rc.getRandom(), 1);
  },
});
