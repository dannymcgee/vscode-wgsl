use std::fmt;

use parser::ast::Token;

pub type Result<T> = std::result::Result<T, TranspileError>;

#[derive(Clone, Debug)]
pub enum TranspileError {
	UnknownNamespace(Token),
	UnresolvedModule(Token),
	Multiple(Vec<TranspileError>),
}

impl fmt::Display for TranspileError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use TranspileError::*;

		match self {
			UnknownNamespace(token) => write!(f, "Unknown namespace `{}`", token),
			UnresolvedModule(token) => write!(f, "Unresolved module `{}`", token),
			Multiple(errors) => {
				for err in errors.iter() {
					writeln!(f, "{}", err)?;
				}
				Ok(())
			}
		}
	}
}

pub(crate) trait CumulativeResult {
	type Error: fmt::Display;

	fn add_error(&mut self, err: Self::Error);
}

impl CumulativeResult for Result<()> {
	type Error = TranspileError;

	fn add_error(&mut self, err: Self::Error) {
		use TranspileError::*;

		match self {
			Ok(_) => {
				*self = Err(err);
			}
			Err(Multiple(ref mut errs)) => {
				errs.push(err);
			}
			Err(single) => {
				*self = Err(Multiple(vec![single.to_owned()]));
			}
		}
	}
}

pub(crate) trait FoldErrors<T> {
	fn fold_errors(self, init: T) -> Result<T>;
}

impl<T, Iter> FoldErrors<T> for Iter
where Iter: Iterator<Item = TranspileError>
{
	fn fold_errors(self, init: T) -> Result<T> {
		self.fold(Ok(init), |mut accum, current| {
			use TranspileError::*;

			match accum {
				Ok(_) => Err(current),
				Err(Multiple(ref mut accum_errs)) => {
					match current {
						Multiple(cur_errs) => accum_errs.extend(cur_errs),
						single => accum_errs.push(single),
					}
					accum
				}
				Err(accum_single) => match current {
					Multiple(cur_errs) => {
						let mut combined = vec![accum_single];
						combined.extend(cur_errs);

						Err(Multiple(combined))
					}
					cur_single => Err(Multiple(vec![accum_single, cur_single])),
				},
			}
		})
	}
}
