use crate::stmt::Stmt;

#[test]
fn if_stmt() {
	super::parse::<Stmt>(
		r#"
if (true) {
	discard;
} else {}
		"#,
	);

	super::parse::<Stmt>(
		r#"
if (0 != 1) {}
		"#,
	);

	super::parse::<Stmt>(
		r#"
if (false) {
	return;
} elseif (true) {
	return;
} elseif (2 + 2 == 5) {
	return;
} else {}
		"#,
	);
}

#[test]
fn switch_stmt() {
	super::parse::<Stmt>(
		r#"
switch (3) {
	case 0, 1: {
		pos = 0.0;
	}
	case 2: {
		pos = 1.0;
		fallthrough;
	}
	case 3: {}
	default: {
		pos = 3.0;
	}
}
		"#,
	)
}

#[test]
fn loop_stmt() {
	super::parse::<Stmt>(
		r#"
loop {
	if (i == 1) {
		break;
	}
	continuing {
		i = 1;
	}
}
		"#,
	);
}

#[test]
fn for_stmt() {
	super::parse::<Stmt>(
		r#"
for (var i: i32 = 0; i < 4; i = i + 1) {
	a = a + 2;
}
		"#,
	);

	super::parse::<Stmt>(
		r#"
for(;;) {
	break;
}
		"#,
	);
}

#[test]
fn var_stmt() {
	super::parse::<Stmt>("var i: i32 = 0;");
}
