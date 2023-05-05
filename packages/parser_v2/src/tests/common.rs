use crate::common::AttributeList;

#[test]
fn attributes() {
	super::parse::<AttributeList>("@fragment");
	super::parse::<AttributeList>("@builtin(position)");
	super::parse::<AttributeList>("@group(0) @binding(0)");
}
