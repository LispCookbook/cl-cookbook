;; Put the cursor on the "(format" below and press "F1" to get the
;; Hyperspec page for "format". Put the cursor on the "d" in the
;; format string and press "C-u F1" to get the Hyperspec page
;; describing the "Tilde D: Decimal" format character.

(format t "One = ~d" 1)

;; Note: Depending on the documentation packages that have been
;; loaded, and the browser that you wish to use, the following keys
;; may be used:
;;            Default    W3
;; Package    Browser  Browser  Format-Dft  Format-W3  Info
;; =========  =======  =======  ==========  =========  ====
;; Hyperspec    F1      S-F1    C-u F1      C-u S-F1
;; CLtL2       M-F1    M-S-F1   
;; ACL docs    C-F1    C-S-F1
;; Info docs                                           C-M-F1