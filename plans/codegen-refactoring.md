# Codegen Refactoring Plan

## Overview

`src/compiler/codegen.rs` is **10,309 lines** with **128 methods**. This plan captures refactoring opportunities ordered by impact.

---

## Phase 1: Low-level helper extraction (this PR)

### 1a. Struct field access helpers

Add `emit_struct_field_load()` and `emit_struct_field_store()` to reduce ~60 instances of GEP+load and ~30 instances of GEP+store for struct fields.

**Pattern being replaced:**
```rust
let gep = self.fresh_temp();
self.emit_line(&format!(
    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
    gep, struct_name, ptr, field_idx
));
let val = self.fresh_temp();
self.emit_line(&format!("{} = load {}, ptr {}", val, field_ty, gep));
```

**New helper:**
```rust
fn emit_struct_field_load(&mut self, struct_name: &str, ptr: &str, field_idx: u32, field_ty: &str) -> String
fn emit_struct_field_store(&mut self, struct_name: &str, ptr: &str, field_idx: u32, field_ty: &str, val: &str)
```

### 1b. Split `emit_builtin_helpers()` (1,712 lines)

Break into thematic sub-methods called from `emit_builtin_helpers()`:
- `emit_builtin_string_helpers()` — string ops (split, replace, trim, etc.)
- `emit_builtin_math_helpers()` — math/bitwise
- `emit_builtin_io_helpers()` — print, input, file ops
- `emit_builtin_collection_helpers()` — array, push, pop, contains, etc.
- `emit_builtin_conversion_helpers()` — to_int, to_float, type conversions

---

## Phase 2: Additional low-level helpers (future)

| Pattern | Occurrences | Potential helper |
|---|---|---|
| Alloca + store | ~60 | `emit_alloca_store(ty, val) -> String` |
| Memcpy for aggregates | ~25 | `emit_aggregate_copy(dst, src, ty)` |
| Condition + branch | ~8 | `emit_cond_branch(cond, true_lbl, false_lbl)` |
| Load from ptr | ~50 | `emit_load(ty, ptr) -> String` |

---

## Phase 3: Module extraction (future)

| Candidate module | Lines | Methods |
|---|---|---|
| `codegen_builtins.rs` | ~2,000 | Built-in helper emission |
| `codegen_pipelines.rs` | ~1,900 | Pipeline extraction, steps, terminators |
| `codegen_statements.rs` | ~700 | let, assign, if, while, match, return |
| `codegen_types.rs` | ~560 | Type queries, llvm_type, expr predicates |

---

## Phase 4: Pipeline terminator dedup (future)

The 5 pipeline terminators (`collect`, `fold`, `any_all`, `find`, `reduce`) each reimplement a similar loop structure. A shared loop-builder abstraction could consolidate them.

---

## Already well-factored

- Fat pointer helpers (`emit_fat_ptr_*`) — recently extracted, clean
- Scope management — minimal and clear
- Output/label utilities — clean

---

## Constraints

- All refactoring must be mechanical (no behavior changes)
- `cargo test` must pass after each phase
- `cargo clippy` and `cargo fmt` must stay clean
