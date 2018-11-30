extern crate rstv;
use rstv::STV;

#[test]
fn main() {
    let mut election = STV::with_candidates(&[1]);
    let r = election.run_election(1).unwrap();
    assert_eq!(r, vec![1]);
}
