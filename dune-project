(lang dune 3.17)
(using menhir 3.0)

(name variable_analysis)

(generate_opam_files true)

(source
 (github HyeongseoYoo/Variable-Analysis))

(authors "Hyeongseo Yoo <yhs0918@snu.ac.kr>")

(maintainers "Hyeongseo Yoo <yhs0918@snu.ac.kr>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name variable_analysis)
 (synopsis "Variable Analysis Tool")
 (description "Interrupt-Aware Variable Analysis Tool for SSD Firmware")
 (depends ocaml
         (ocaml-base-compiler
           (= 5.3.0))
         menhir)
 (tags
  ("abstract interpretation" "interrupt" "variable analysis")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html