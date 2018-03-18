let str = ReasonReact.stringToElement;

let rec length = list =>
  switch (list) {
  | [] => 0
  | [_, ...t] => 1 + length(t)
  };

let renderFooter = numItems =>
  switch (numItems) {
  | 1 => " item"
  | _ => " items"
  };

let rec map = (f, l) =>
  switch (l) {
  | [] => []
  | [h, ...t] => [f(h), ...map(f, t)]
  };

type item = {
  id: int,
  title: string,
  completed: bool,
};

type state = {items: list(item)};

type action =
  | AddItem(string)
  | ToggleItem(int)
  | DeleteItem(int);

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, ~onDelete, _children) => {
    ...component,
    render: _self =>
      <div className="item">
        <span onClick=(_evt => onToggle())>
          <input
            _type="checkbox"
            checked=(Js.Boolean.to_js_boolean(item.completed))
            /* TODO make interactive */
          />
          (str(item.title))
        </span>
        <button onClick=(_evt => onDelete())> (str("Delete")) </button>
      </div>,
  };
};

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) =>
      <input
        _type="text"
        value=text
        placeholder="Write something to do"
        onChange=(reduce(evt => valueFromEvent(evt)))
        onKeyDown=(
          evt =>
            if (ReactEventRe.Keyboard.key(evt) == "Enter") {
              onSubmit(text);
              reduce(() => "", ());
            }
        )
      />,
  };
};

let component = ReasonReact.reducerComponent("TodoApp");

let lastId = ref(0);

let newItem = text => {
  lastId := lastId^ + 1;
  {id: lastId^, title: text, completed: false};
};

let make = _children => {
  ...component,
  initialState: () => {
    items: [{id: 0, title: "Write some things to do", completed: false}],
  },
  reducer: (action, {items}) =>
    switch (action) {
    | AddItem(text) =>
      ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) =>
      let items =
        map(
          item =>
            item.id === id ? {...item, completed: ! item.completed} : item,
          items,
        );
      ReasonReact.Update({items: items});
    | DeleteItem(id) =>
      let items = List.filter(x => x.id != id, items);
      ReasonReact.Update({items: items});
    },
  render: ({state: {items}, reduce}) => {
    let numItems = length(items);
    <div className="app">
      <div className="title">
        (str("What to do"))
        <Input onSubmit=(reduce(text => AddItem(text))) />
      </div>
      <div className="items">
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              map(
                item =>
                  <TodoItem
                    item
                    key=(string_of_int(item.id))
                    onToggle=(reduce(() => ToggleItem(item.id)))
                    onDelete=(reduce(() => DeleteItem(item.id)))
                  />,
                items,
              ),
            ),
          )
        )
      </div>
      <div className="footer">
        (str(string_of_int(numItems) ++ renderFooter(numItems)))
      </div>
    </div>;
  },
};