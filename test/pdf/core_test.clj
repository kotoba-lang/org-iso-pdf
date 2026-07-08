(ns pdf.core-test
  "PDF image XObject extraction via a synthetic minimal PDF."
  (:require [clojure.test :refer [deftest is]]
            [pdf.core :as pdf]))

(defn- zlib [^bytes in]
  (let [d (java.util.zip.Deflater.) out (java.io.ByteArrayOutputStream.) buf (byte-array 4096)]
    (.setInput d in) (.finish d)
    (while (not (.finished d)) (let [k (.deflate d buf)] (.write out buf 0 k)))
    (.end d) (mapv #(bit-and (int %) 0xff) (.toByteArray out))))

(deftest pdf-image-xobject
  (let [samples [10 20 30 40]                                            ; 2x2 gray, 8bpc
        comp    (zlib (byte-array (map unchecked-byte samples)))
        ascii*  (fn [s] (mapv #(bit-and (int %) 0xff) (.getBytes ^String s "ISO-8859-1")))
        head (ascii* (str "%PDF-1.5\n"
                          "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n"
                          "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >> endobj\n"
                          "3 0 obj << /Type /Page /Parent 2 0 R /Resources << /XObject << /Im0 4 0 R >> >> >> endobj\n"
                          "4 0 obj << /Type /XObject /Subtype /Image /Width 2 /Height 2 /BitsPerComponent 8 "
                          "/ColorSpace /DeviceGray /Filter /FlateDecode /Length " (count comp) " >> stream\n"))
        tail (ascii* "\nendstream endobj\ntrailer << /Root 1 0 R >>\n%%EOF\n")
        bytes (vec (concat head comp tail))
        parsed (pdf/parse bytes)
        page (first (pdf/pages parsed))
        imgs (pdf/page-images (:objects parsed) page)]
    (is (= 1 (count imgs)))
    (let [img (first imgs)]
      (is (= "Im0" (:name img)))
      (is (= [2 2] [(:w img) (:h img)]))
      (let [dec (pdf/decode-image (:objects parsed) img)]
        (is (= :raw (:fmt dec)))
        (is (= samples (:samples dec)))))))                              ; FlateDecode → original samples

(deftest pdf-text-extraction
  (let [ascii* (fn [s] (mapv #(bit-and (int %) 0xff) (.getBytes ^String s "ISO-8859-1")))
        pdf-bytes (ascii* (str "%PDF-1.5\n"
                               "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n"
                               "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 200] >> endobj\n"
                               "3 0 obj << /Type /Page /Parent 2 0 R /Contents 4 0 R >> endobj\n"
                               "4 0 obj << /Length 33 >> stream\n"
                               "BT (Hello) Tj (World) Tj ET\n"
                               "endstream endobj\n"
                               "trailer << /Root 1 0 R >>\n%%EOF\n"))
        parsed (pdf/parse pdf-bytes)
        page (first (pdf/pages parsed))]
    (is (= [0 0 200 200] (mapv int (:MediaBox page))))
    (is (= ["Hello" "World"] (pdf/page-text (:objects parsed) page)))))
