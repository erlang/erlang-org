%% Find even numbers
-spec even(list(integer())) ->
  list({integer(), boolean()}).
even(Numbers) ->
  [{Number, Number rem 2 == 0}
    || Number <- Numbers].