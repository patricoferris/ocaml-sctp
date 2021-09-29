(lang dune 2.9)
(name sctp)

(generate_opam_files true)

(license ISC)
(authors "Patrick Ferris")
(maintainers "pf341@patricoferris.com")

(package
 (name sctp)
 (synopsis "Pure OCaml SCTP")
 (description "Pure OCaml Session Control Transmission Protocol")
 (depends
  (alcotest :with-test)
  cstruct))