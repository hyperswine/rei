#*
    Utility Display tools
*#

export Debug: annotation (derive) (&self, formatter: &mut Formatter) -> () | FmtError {}

// export Display: trait (&self, formatter: &mut Formatter) -> () | FmtError

# for @derive(Display)
export Display: annotation (derive) (tokens: Tokens) {
    match tokens.parse() {
        Ok(vis, ident, fields...) => {
            add_to_namespace(
                // NOTE: the $operator means expr.embed() which can be impl'd through the Embed trait fn
                impl $ident: Display(&self, formatter: &mut Formatter) -> () | FmtError {
                    @rei {
                        fields.map(f => f.display())!?
                    }
                }
            )
        }
        Err(e) => {
            panic!("Could not derive display, did you do it right?")
        }
    }
}