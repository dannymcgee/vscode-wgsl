use gramatika::Spanned;
use parser_v2::Token;

#[derive(Debug)]
pub struct TokenDelta {
	pub delta_line: u32,
	pub delta_start_char: u32,
}

pub trait Delta {
	fn delta(&self, prev: &Self) -> TokenDelta;
}

impl<'a> Delta for Token<'a> {
	fn delta(&self, prev: &Self) -> TokenDelta {
		let cur_span = self.span();
		let prev_span = prev.span();

		let delta_line = (cur_span.start.line - prev_span.start.line) as u32;
		let delta_start_char =
			if delta_line == 0 && cur_span.start.character >= prev_span.start.character {
				cur_span.start.character - prev_span.start.character
			} else {
				cur_span.start.character
			} as u32;

		TokenDelta {
			delta_line,
			delta_start_char,
		}
	}
}
