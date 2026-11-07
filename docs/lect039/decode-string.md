# Decode String in Clojure

## Setup: stopping an infinite loop from CIDER

`cider-interrupt` (`C-c C-c`) failed with:

```
ERROR: Cannot stop thread on JDK21+ without -Djdk.attach.allowAttachSelf enabled,
see https://nrepl.org/nrepl/installation.html#jvmti.
```

On JDK 21+, nREPL needs the JVM flag `-Djdk.attach.allowAttachSelf` to stop a running thread.

### Fix with a `deps.edn` alias

```clojure
{:deps {org.clojure/clojure {:mvn/version "1.12.2"}}
 :paths ["src"]
 :aliases {:nrepl {:jvm-opts ["-Djdk.attach.allowAttachSelf"]}}}
```

Defining the alias isn't enough. `cider-jack-in` only enables its own `:cider/nrepl` alias, so CIDER has to be told to
include `:nrepl` too. Put this in the project's `.dir-locals.el`:

```elisp
((clojure-mode . ((cider-clojure-cli-aliases . ":nrepl"))))
```

Then quit the REPL, reload the buffer (`M-x revert-buffer`, because dir-locals are only read when a file is opened),
and jack in again. The start command should end in `-M:nrepl:cider/nrepl`. If it ends in `-M:cider/nrepl` only, the
alias wasn't picked up. That happened the first time because `.dir-locals.el` didn't exist yet.

To check from the REPL:

```clojure
(System/getProperty "jdk.attach.allowAttachSelf") ;=> "" (not nil) means the flag is set
```

### Alternative: let CIDER add the flag (CIDER 1.15+)

```elisp
(setq cider-enable-nrepl-jvmti-agent t)
```

This works for every project, with no alias or dir-locals needed.

## Bug: `parse` loops forever on `"3[a]"`

The TypeScript version (`typescript/src/lect039/decode-string.ts`) worked. The Clojure port hung.

### Buggy version

```clojure
(defn parse [s i]
  (let [n (count s)]
    (loop [ii i
           cnt  0
           p (str)
           c (get s ii)]
      (println c)
      (if (and (< ii n) c (not (re-matches #"\]" c)))
        (cond
          (re-matches #"[a-z][A-Z]" c)
          (recur (inc ii) cnt (str p c) (get s (inc ii)))

          (re-matches #"[0-9]" c)
          (recur (inc ii) (-> cnt (* 10) (+ (parse-long (re-matches #"[0-9]" c)))) p (get s (inc ii)))

          ;; left bracket
          :else
          (let [{:keys [pos, subp]} (parse s (inc ii))]
            (recur (inc pos) 0 (str p (string/join "" (repeat subp))) (get s (inc pos)))))
        {:pos ii, :path p}))))

(parse (string/split "3[a]" #"") 0)
```

### Trace of `(parse ["3" "[" "a" "]"] 0)`

| step | `ii` | `c`   | branch taken               | result                    |
|------|------|-------|----------------------------|---------------------------|
| 1    | 0    | `"3"` | digit                      | `cnt` = 3                 |
| 2    | 1    | `"["` | `:else`                    | recurse from 2            |
| 3    | 2    | `"a"` | **`:else`** (bug 1)        | recurse from 3            |
| 4    | 3    | `"]"` | stop                       | returns `{:pos 3 :path ""}` |
| 5    | back in step 3 |  | destructures `subp`, gets **`nil`** (bug 2) |          |
| 6    |      |       | `(string/join "" (repeat nil))` | **hangs** (bug 3)    |

### The three bugs

1. **`#"[a-z][A-Z]"` matches two characters**, a lowercase letter followed by an uppercase one. A single letter such as
   `"a"` doesn't match, so it falls into the `:else` (left bracket) branch. Fix: `#"[a-zA-Z]"`.
2. **Wrong destructuring key.** `parse` returns `{:pos … :path …}`, but the call site destructures
   `{:keys [pos subp]}`. There's no `:subp` key, so `subp` is always `nil`. Fix: `{:keys [pos path]}`.
3. **`(repeat x)` with one argument is an infinite lazy sequence.** `string/join` keeps reading it forever. This is
   the actual hang. The TS helper `repeat(cnt, str)` takes a count, so the Clojure call needs one too:
   `(repeat cnt path)`.

Clojure's `repeat` has a one-argument form and the TS helper doesn't, so the bug only exists in the Clojure port.
Bug 1 makes even the simplest input reach it.

### Fixed version

```clojure
(defn parse [s i]
  (let [n (count s)]
    (loop [ii i
           cnt  0
           p (str)
           c (get s ii)]
      (if (and (< ii n) c (not (re-matches #"\]" c)))
        (cond
          (re-matches #"[a-zA-Z]" c)                                   ; fix 1
          (recur (inc ii) cnt (str p c) (get s (inc ii)))

          (re-matches #"[0-9]" c)
          (recur (inc ii) (-> cnt (* 10) (+ (parse-long c))) p (get s (inc ii)))

          ;; left bracket
          :else
          (let [{:keys [pos path]} (parse s (inc ii))]                 ; fix 2
            (recur (inc pos) 0 (str p (string/join (repeat cnt path))) ; fix 3
                   (get s (inc pos)))))
        {:pos ii, :path p}))))

(defn decode-string [s]
  (-> (string/split s #"") (parse 0) :path))

(decode-string "3[a]")          ;=> "aaa"
(decode-string "3[a2[c]]")      ;=> "accaccacc"
(decode-string "2[abc]3[cd]ef") ;=> "abcabccdcdcdef"
```

## Question: too many `loop` bindings hurt readability. Would a map help?

Yes, but first check whether some bindings shouldn't exist. In order of preference:

### 1. Remove bindings you can derive

`c` is always `(get s ii)`, and `n` isn't needed either, because `get` on a vector returns `nil` past the end:

```clojure
(defn parse [s i]
  (loop [ii i, cnt 0, p ""]
    (let [c (get s ii)]
      (cond
        (or (nil? c) (= c "]"))     {:pos ii :path p}
        (re-matches #"[a-zA-Z]" c)  (recur (inc ii) cnt (str p c))
        (re-matches #"[0-9]" c)     (recur (inc ii) (+ (* 10 cnt) (parse-long c)) p)
        :else
        (let [{:keys [pos path]} (parse s (inc ii))]
          (recur (inc pos) 0 (str p (string/join (repeat cnt path)))))))))
```

The stop condition is now the first `cond` branch, so the outer `if` is gone and each `recur` is shorter.

### 2. A map of state, when the bindings really are many

```clojure
(defn parse [s i]
  (loop [{:keys [ii cnt p] :as st} {:ii i :cnt 0 :p ""}]
    (let [c (get s ii)]
      (cond
        (or (nil? c) (= c "]"))     {:pos ii :path p}
        (re-matches #"[a-zA-Z]" c)  (recur (-> st (update :ii inc) (update :p str c)))
        (re-matches #"[0-9]" c)     (recur (-> st (update :ii inc)
                                               (assoc :cnt (+ (* 10 cnt) (parse-long c)))))
        :else
        (let [{:keys [pos path]} (parse s (inc ii))]
          (recur (assoc st :ii (inc pos) :cnt 0
                           :p (str p (string/join (repeat cnt path))))))))))
```

- **Gains:** each `recur` names only the keys that change, so you can't mix up the order of positional arguments, and
  adding new state doesn't mean editing every `recur`.
- **Costs:** a small allocation on each iteration, and some noise from the `update`/`assoc` chains.
- **Rule of thumb:** with about four or more bindings where most `recur`s change only one or two of them, use a map.
  With three or fewer, positional bindings read better.

### 3. Change the representation (often the biggest win)

The index exists only because the code walks a vector by position. If you destructure a sequence of chars instead,
the index disappears, and the parser returns the decoded string plus the input that's left:

```clojure
(defn parse [cs]
  (loop [[c & more] cs, cnt 0, out ""]
    (cond
      (or (nil? c) (= c \]))  [out more]
      (Character/isLetter c)  (recur more cnt (str out c))
      (Character/isDigit c)   (recur more (+ (* 10 cnt) (Character/digit c 10)) out)
      :else
      (let [[sub remaining] (parse more)]
        (recur remaining 0 (str out (string/join (repeat cnt sub))))))))

(defn decode-string [s] (first (parse s)))   ; works directly on a string, no split needed
```

With no index, there's no `pos`/`inc pos` arithmetic to get wrong. This is the usual functional way to write a
recursive-descent parser.

### Other general habits

- Move each branch's logic into a small named helper so the `recur` lines stay one-liners.
- Use `reduce` instead of `loop` when you're folding over a collection in order with no early exit or back-tracking.
  This parser doesn't qualify because of the nested recursion.
- Pull values that never change out of the loop and into a surrounding `let`.
