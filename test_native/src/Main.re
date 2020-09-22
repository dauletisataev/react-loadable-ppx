let fourtyTwo = string_of_int([%gimme]);

print_endline(fourtyTwo);

type a = {text: string};

[@react.loadable]
let make = () => "lorem";

print_endline(Loadable.text_string);