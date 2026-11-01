# Tree-sitter hides supertype rules in its output trees, but gen-mod
# treats only rules whose names start with an underscore as hidden.
# Stripping aliases can also invalidate supertypes (an alternative may
# no longer be a single visible node), which makes `tree-sitter generate`
# reject the grammar. Dropping them makes those rules ordinary visible
# nodes, matching what gen-mod generates.
del(.supertypes)
