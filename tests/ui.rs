#[test]
fn long_sequences_fail_with_a_regrouping_hint() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/long_sequence.rs");
}
