## Release v0.17.0

- Can now find text that has been moved with the `find_moves` parameter
- `Range` now can have a `Move_id` which identifies which move the current `Range` belongs to.
  Multiple ranges can belong to the same move.

## Release v0.16.0

* Added `?max_slide` and `?score` arguments to `get_matching_blocks` and `get_hunks`.

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## 113.24.00

- Switch to PPX.

## 112.24.00

Update references to `Core.Std.Dequeue` to refer to `Core.Std.Deque`

## 111.25.00

- refactoring and more unit tests

## 111.21.00

- Added plain differ `Plain_diff` and use it in some cases for
  improved results.
- Move modules under `Patience_diff_lib.Std`.

## 111.17.00

- Exposed `Patience_diff.matches`.

## 111.13.00

- Moved `Patience_diff` out of `Core_extended` into its own library
  depending only on `Core_kernel`.

