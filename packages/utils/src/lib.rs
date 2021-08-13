#[macro_export]
macro_rules! hashmap {
	($($key:expr => $value:expr),*$(,)?) => {{
		let mut hashmap = ::std::collections::HashMap::new();
		$(hashmap.insert($key, $value);)*

		hashmap
	}}
}

#[cfg(test)]
mod tests;
