use crate::parser::Ast;

pub enum AnalyzerError {}

pub struct Analyzer {
	ast: Ast,
}

impl Analyzer {
	pub fn analyze(ast: Ast) -> Result<(), AnalyzerError> {
		Ok(())
	}
}
