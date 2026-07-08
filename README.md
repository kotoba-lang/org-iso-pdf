# kotoba-lang/org-iso-pdf

Zero-dep-beyond-`org-ietf-deflate` portable `.cljc` PDF COS object model
reader (ISO 32000). Named `org-iso-pdf` — PDF was formally standardized as
ISO 32000-1/2 (same reasoning `org-ieee-verilog` applies to a
vendor-originated-then-standardized spec: use the numbered ISO/IEC
designation as the canonical reference once one exists).

Extracted from `kotoba-lang/kasane` (kasane.cos, ADR-2606272100). PDF is not
a linear struct grammar (COS object graph + xref + trailer), so this is a
hand-written recursive-descent reader (dict/array/string/name/number/ref/
stream). R0 collects top-level objects by scan (robust to classic or
rebuilt xref), resolves the page tree, decodes FlateDecode content streams
via `org-ietf-deflate`, and extracts BT/ET shown text. Objects packed in
`/ObjStm` object streams are not resolved (R0). Image XObjects: FlateDecode
→ raw samples; DCTDecode → opaque JPEG bytes (feed to `org-iso-jpeg`'s
`decode-rgb`, kept decoupled); other filters → opaque.

## Usage

```clojure
(require '[pdf.core :as pdf])

(def parsed (pdf/parse pdf-bytes))       ; => {:objects :trailer :root}
(def pages  (pdf/pages parsed))          ; leaf page dicts, MediaBox/Resources inherited
(pdf/page-text (:objects parsed) (first pages))     ; => vector of shown text strings
(pdf/page-images (:objects parsed) (first pages))   ; => image XObjects
```

## Test

```sh
clojure -M:test
```
