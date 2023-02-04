# default, list all just Recipe
default: 
  @just -q --list

alias t := test

# cargo test with all features enabled
test:
  cargo test --all-features

# code coverage
coverage:
  cargo tarpaulin --out html
  rm default_*

# cargo watch for test with given filter
watch filter="":
  @cargo watch -q -c -x 'test {{filter}}'     
