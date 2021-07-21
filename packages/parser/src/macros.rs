macro_rules! fold_children {
	($source:ident, $accum:ident, $current:ident {
		$($($pattern:pat)|+ $(if $condition:expr)? => $expression:expr),+
		,
	}) => {
		$source
			.into_inner()
			.fold($accum, |$accum, $current| match $current.as_rule() {
				$($($pattern)|+ $(if $condition)? => $expression,)+
			})
	};
}
