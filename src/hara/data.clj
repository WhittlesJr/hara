(ns hara.data
  (:require [hara.module :as module]))

(module/link

 (hara.data.base.combine   combine
                           decombine)

 (hara.data.base.complex   assocs
                           dissocs
                           gets
                           merges
                           merges-nested
                           gets-in
                           dissocs-in)

 (hara.data.base.diff      diff
                           changed
                           patch
                           unpatch)

 (hara.data.base.map       dissoc-in
                           unique
                           assoc-some
                           assoc-in-some
                           update-in-some
                           merge-some
                           into-some
                           select-keys-some
                           merge-over-nil
                           assoc-in-over-nil
                           transform-in
                           retract-in
                           map-keys
                           map-vals
                           map-entries
                           transpose)
                          
 (hara.data.base.nested    keys-nested
                           key-paths
                           update-keys-in
                           update-vals-in
                           merge-nested
                           merge-over-nil-nested
                           dissoc-nested
                           unique-nested
                           clean-nested)
                          
 (hara.data.base.seq       positions
                           remove-index
                           index-of
                           element-of
                           flatten-all))
