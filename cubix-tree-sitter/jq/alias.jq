walk(
  if type != "object"
    then .
  elif .type == "ALIAS"
    then .content
  else .
  end
)