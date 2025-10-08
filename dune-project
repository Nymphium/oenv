(lang dune 3.18)

(name oenv)

(source
 (github Nymphium/oenv))

(documentation "https://nymphium.github.io/oenv")

(license MIT)

(authors "Nymphium <s1311350@gmail.com>")

(maintainers "Nymphium <s1311350@gmail.com>")

(homepage "https://github.com/Nymphium/oenv")

(bug_reports "https://github.com/Nymphium/oenv/issues")

(generate_opam_files)

(package
 (name oenv)
 (synopsis "A composable environment variable reader")
 (description
  "oenv is a library for reading environment variables in a composable way, with support for default values, required variables, custom types, and logging via logs.")
 (depends
  (ocaml
   (>= 4.14))
  (dune :dev)
  logs
  ppx_deriving
  (eio_main :with-test)
  (ppx_assert :with-test)
  (ppx_inline_test :with-test)
  (odoc :with-doc)))
