use crate::parser::Ast;

pub enum TypeInfo {}

pub enum AnalyzerError {}

pub struct Analyzer {
	ast: Ast,
}

impl Analyzer {
	pub fn analyze(ast: Ast) -> Result<(), AnalyzerError> {
		let mut analyzer = Analyzer { ast };

		// Check to make sure only certain statements are allowed in global scope

		Ok(())
	}
}
