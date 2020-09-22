
open Migrate_parsetree;
open Ast_409;
open Ast_mapper;
open Ast_helper;
open Ast_convenience_409;
open Location;
open Parsetree;
open Longident;

let nllid = name => mknoloc(Lident(name));
let mkref = name => Exp.mk(Pexp_ident(mknoloc(Lident(name))));
let mklet = (name, expr) => Vb.mk(pvar(name), expr);

let updatedRecord = (~singleField=false, record, field, value) =>
  Exp.mk(
    Pexp_record(
      [(nllid(field), mkref(value))],
      singleField ? None : Some(mkref(record)),
    ),
  );

let findAttr = find_attr;

let hasAttr = (s, attrs) => findAttr(s, attrs) != None;

let refractiveAnnotated = ty =>
  hasAttr("react.loadable", ty.pvb_attributes);

let anyRefractiveAnnotation = List.exists(refractiveAnnotated);

let lensDefinition = (~singleField=false, name) => {
  let getter = [%expr x => [%e Exp.field(evar("x"), nllid(name))]];
  let setter = [%expr
    (newVal, x) => [%e updatedRecord(~singleField, "x", name, "newVal")]
  ];
  let mklens = [%expr
    Refractive.Lens.make(~get=[%e getter], ~set=[%e setter])
  ];
  mklet(name, mklens);
};

let selectorDefinition = (lensesModule, name) => {
  let mkselector = [%expr
    Refractive.Selector.make(
      ~lens=[%e evar(lensesModule ++ "." ++ name)],
      ~path=[|[%e str(name)]|],
    )
  ];
  mklet(name, mkselector);
};

let qualifiedModuleName = (typeName, baseName) =>
  switch (typeName) {
  | "t" => baseName
  | other => String.capitalize_ascii(other) ++ baseName
  };

let defineModule = (loc) => {
  let content =
    [%str
      let text_string = "i am your text string";
    ];
  {
    pstr_desc:
      Pstr_module({
        pmb_name: {
          txt: "Loadable",
          loc,
        },
        pmb_expr: {
          pmod_desc: Pmod_structure(content),
          pmod_loc: loc,
          pmod_attributes: [],
        },
        pmb_loc: loc,
        pmb_attributes: [],
      }),
    pstr_loc: loc,
  };
};

let itemWithLoadableModule = (_typ_decls, pstr_loc, item) => {
  
  let loadableModule =
    defineModule(
      pstr_loc
    );
  
  [item] @ [loadableModule];
};

let mapper = (_, _) =>
        {
          ...default_mapper,
          structure: (mapper, items) => {
            switch (items) {
              | [
                  {pstr_desc: Pstr_value(_recFlag, value_bindings), pstr_loc} as item,
                  ...rest,
                ] when anyRefractiveAnnotation(value_bindings) =>
                let derived =
                  Ast_helper.with_default_loc(pstr_loc, () =>
                    itemWithLoadableModule([], pstr_loc, item)
                  );
                derived @ mapper.structure(mapper, rest);
              | [item, ...rest] =>
                let derived = mapper.structure_item(mapper, item);
                [derived, ...mapper.structure(mapper, rest)];
              | [] => []
              };
          },
          expr: (mapper, e) => {
            switch (e.pexp_desc) {
            | Pexp_let(Nonrecursive, _, _) => 
                %expr
                42
            /* If the expression is [%gimme] */
            | Pexp_extension(({txt: "gimme", _}, _payload)) =>
              /* Then replace by 42 */
              %expr
              42
            | _ => default_mapper.expr(mapper, e)
            };
          },
          
        };

let () =
  Migrate_parsetree.(
    Driver.register(~name="ppx_42", Versions.ocaml_409, mapper)
  );
