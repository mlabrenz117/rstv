use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Display};
use std::hash::Hash;

pub struct STV<T> {
    candidates: Vec<T>,
    ballots: HashMap<Vec<T>, usize>,
    total_voters: usize,
    closed_election: bool,
    max_iterations: u64,
}

impl<T: Clone + Eq + Hash + std::fmt::Debug> STV<T> {
    pub fn new() -> Self {
        STV {
            candidates: Vec::new(),
            ballots: HashMap::new(),
            total_voters: 0,
            closed_election: false,
            max_iterations: 500,
        }
    }

    pub fn with_candidates(candidates: &[T]) -> Self {
        let candidates = candidates.to_vec();
        STV {
            candidates,
            ballots: HashMap::new(),
            total_voters: 0,
            closed_election: true,
            max_iterations: 500,
        }
    }

    pub fn add_ballots(&mut self, ballots: impl IntoIterator<Item = impl IntoIterator<Item = T>>) {
        for ballot in ballots {
            let ballot: Vec<T> = ballot
                .into_iter()
                .inspect(|candidate| {
                    // Using inspect like this seems like a side affect;
                    // not sure if this is idiomatic?
                    if !self.closed_election && !self.candidates.contains(candidate) {
                        self.candidates.push(candidate.clone());
                    }
                })
                .collect();
            *self.ballots.entry(ballot).or_insert(0) += 1;
            self.total_voters += 1;
        }
    }

    pub fn run_election(&mut self, seats: usize) -> Result<Vec<T>, ElectionErr> {
        if self.candidates.len() < seats {
            return Ok(self.candidates.clone());
        }

        let mut candidates: HashMap<T, Candidate> = self
            .candidates
            .iter()
            .map(|c| (c.clone(), Candidate::new()))
            .collect();
        let mut elected = Vec::new();
        'main: loop {
            'compute: loop {
                let new_electors = self.compute(&mut candidates, seats)?;
                for elector in new_electors.iter() {
                    elected.push(elector.clone());
                }
                if new_electors.is_empty() || elected.len() >= seats {
                    break 'compute;
                }
            }

            if elected.len() < seats {
                if let None = exclude_lowest(&mut candidates) {
                    break 'main; //No more candidates can be elected
                }
                let num_excluded = candidates
                    .iter()
                    .filter(|(_, v)| v.status.is_excluded())
                    .count();
                if self.candidates.len() - num_excluded == seats {
                    let mut hopefuls: Vec<_> = candidates
                        .iter_mut()
                        .filter(|(_, v)| v.status.is_hopeful())
                        .collect();
                    hopefuls
                        .sort_by(|(_, b), (_, d)| d.max_votes.partial_cmp(&b.max_votes).unwrap());
                    for (candidate, v) in hopefuls {
                        v.status = CandidateStatus::Elected(1.0);
                        elected.push(candidate.clone())
                    }
                }
            }

            if elected.len() == seats {
                candidates
                    .iter_mut()
                    .filter(|(_, v)| v.status.is_hopeful())
                    .for_each(|(_, v)| v.status = CandidateStatus::Excluded);
                break 'main;
            }
        }

        // Assert that the CandidateStatus invariants are upheld.
        debug_assert!(candidates.iter().all(|(c, v)| if elected.contains(c) {
            v.status.is_elected()
        } else {
            v.status.is_excluded()
        }));

        Ok(elected)
    }

    fn compute(
        &mut self,
        candidates: &mut HashMap<T, Candidate>,
        seats: usize,
    ) -> Result<Vec<T>, ElectionErr> {
        let mut iterations = 0;

        #[allow(unused_assignments)] // Closures don't count as reading?
        let mut quota = 0.;

        let num_elected = candidates
            .iter()
            .filter(|(_, c)| c.status.is_elected())
            .count();

        loop {
            let mut converged = true;
            let mut total_excess: f64 = 0.0;
            iterations += 1;

            for v in candidates.values_mut() {
                v.votes = 0.0;
            }

            for (ballot, n) in self.ballots.iter() {
                total_excess += ballot
                    .iter()
                    .try_fold(*n as f64, |excess, candidate| {
                        if let Some(cv) = candidates.get_mut(candidate) {
                            let votes = cv.weight() * (excess as f64);
                            cv.add_votes(votes);
                            let new_excess = (1.0 - cv.weight()) * (excess as f64);
                            if new_excess > 0.0 {
                                Some(new_excess)
                            } else {
                                None
                            }
                        } else {
                            // If the election is closed, the candidate may not be in the running
                            // pass those votes along
                            Some(excess as f64)
                        }
                    })
                    .unwrap_or(0.0);
            }

            quota = {
                let v = ((self.total_voters as f64) - total_excess) / (seats + 1) as f64;
                if v >= 0.0001 || self.total_voters == 0 {
                    v
                } else {
                    0.0001
                }
            };

            for (_, cand) in candidates.iter_mut() {
                if let CandidateStatus::Elected(weight0) = cand.status {
                    let tmp = quota / cand.votes;
                    if tmp > 1.00001 || tmp < 0.99999 {
                        converged = false;
                        let mut new_weight = weight0 * tmp;
                        if new_weight > 1.0 {
                            new_weight = 1.0 // Another safeguard
                        };
                        cand.status = CandidateStatus::Elected(new_weight);
                    }
                }
            }

            if iterations > self.max_iterations {
                return Err(ElectionErr::MaxIterations(self.max_iterations));
            }

            if converged {
                break;
            }
        }
        let mut almost = HashSet::new();
        candidates
            .iter()
            .filter(|(_, c)| c.status.is_hopeful() && c.votes >= quota)
            .for_each(|(c, _)| {
                almost.insert(c.clone());
            });

        while almost.len() + num_elected > seats {
            candidates
                .iter_mut()
                .filter(|(cand, c)| c.status.is_hopeful() && !almost.contains(cand))
                .for_each(|(_, c)| c.status = CandidateStatus::Excluded);
            let lowest = exclude_lowest(candidates);
            almost.remove(&lowest.unwrap());
        }

        let total_elected = num_elected + almost.len();
        almost.iter().for_each(|c| {
            if let Some(v) = candidates.get_mut(c) {
                v.status = CandidateStatus::Elected(1.0);
                if total_elected < seats {
                    let w = quota / v.votes;
                    v.status = CandidateStatus::Elected(w);
                }
            }
        });
        let mut almost: Vec<T> = almost.into_iter().collect();
        almost.sort_by(|a, b| {
            // Maybe redistribute votes only to these two?
            let val_a = (candidates.get(a).unwrap()).max_votes;
            let val_b = (candidates.get(b).unwrap()).max_votes;
            val_b.partial_cmp(&val_a).unwrap()
        });

        Ok(almost)
    }

    pub fn max_iterations(&mut self, n: u64) {
        self.max_iterations = n;
    }
}

impl<T: Clone + Eq + Hash + std::fmt::Debug> Default for STV<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct Candidate {
    votes: f64,
    status: CandidateStatus,
    max_votes: f64,
}

impl Candidate {
    fn new() -> Self {
        Candidate {
            votes: 0.0,
            status: CandidateStatus::Hopeful,
            max_votes: 0.0,
        }
    }

    fn weight(&self) -> f64 {
        self.status.weight()
    }

    fn add_votes(&mut self, v: f64) {
        self.votes += v;
        if self.votes > self.max_votes {
            self.max_votes = self.votes;
        }
    }
}

#[derive(Debug)]
enum CandidateStatus {
    Hopeful,
    Elected(f64),
    Excluded,
}

impl CandidateStatus {
    fn weight(&self) -> f64 {
        match self {
            CandidateStatus::Hopeful => 1.0,
            CandidateStatus::Excluded => 0.0,
            CandidateStatus::Elected(w) => *w,
        }
    }

    fn is_elected(&self) -> bool {
        match self {
            CandidateStatus::Elected(_) => true,
            _ => false,
        }
    }

    fn is_excluded(&self) -> bool {
        match self {
            CandidateStatus::Excluded => true,
            _ => false,
        }
    }

    fn is_hopeful(&self) -> bool {
        match self {
            CandidateStatus::Hopeful => true,
            _ => false,
        }
    }
}

fn exclude_lowest<T: Eq + Hash>(candidates: &mut HashMap<T, Candidate>) -> Option<&T> {
    if let Some(lowest) = candidates
        .iter_mut()
        .filter(|(_, v)| v.status.is_hopeful())
        .min_by(|(_, c1), (_, c2)| (*c1).votes.partial_cmp(&(c2).votes).unwrap())
    {
        lowest.1.status = CandidateStatus::Excluded;
        Some(lowest.0)
    } else {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElectionErr {
    MaxIterations(u64),
}

impl Display for ElectionErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = match self {
            ElectionErr::MaxIterations(n) => format!("Max Iterations reached - {}", n),
        };
        write!(f, "Error running Election: {}", desc)
    }
}

impl Error for ElectionErr {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ElectionErr::MaxIterations(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() -> Result<(), ElectionErr> {
        let a = vec!["Alice", "Bob", "Chris"];
        let b = vec!["Bob", "Alice", "Chris"];
        let c = vec!["Chris"];
        let d = vec!["Don"];
        let e = vec!["Eric"];
        let mut ballots = Vec::new();
        for _ in 0..28 {
            ballots.push(a.clone());
        }
        for _ in 0..26 {
            ballots.push(b.clone());
        }
        for _ in 0..3 {
            ballots.push(c.clone());
        }
        for _ in 0..2 {
            ballots.push(d.clone());
        }
        ballots.push(e);
        let mut election = STV::new();
        election.add_ballots(ballots);
        let results = election.run_election(3)?;
        assert_eq!(results, vec!["Alice", "Bob", "Chris"]);
        Ok(())
    }

    #[test]
    fn basics() -> Result<(), ElectionErr> {
        let a = vec![1, 2, 3];
        let b = vec![1, 3, 4];
        let c = vec![2, 3, 4];
        let ballots = vec![a, b, c];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let r = election.run_election(3)?;
        assert_eq!(r, vec![1, 2, 3]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let r = election.run_election(3)?;
        assert_eq!(r, vec![1, 2, 3]);
        Ok(())
    }

    #[test]
    fn candidates_basics() -> Result<(), ElectionErr> {
        let a = vec![1, 2, 3];
        let b = vec![1, 3, 4];
        let c = vec![2, 3, 4];
        let ballots = vec![a, b, c];
        let mut election = STV::with_candidates(&[1, 2, 3, 4]);
        election.add_ballots(ballots.clone());
        let r = election.run_election(3)?;
        assert_eq!(r, vec![1, 2, 3]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let r = election.run_election(3)?;
        assert_eq!(r, vec![1, 2, 3]);

        let mut election = STV::with_candidates(&[2, 3, 4]);
        election.add_ballots(ballots);
        let r = election.run_election(3)?;
        assert_eq!(r, vec![2, 3, 4]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let r = election.run_election(3)?;
        assert_eq!(r, vec![2, 3, 4]);
        Ok(())
    }

    #[test]
    fn can_elect_one() -> Result<(), ElectionErr> {
        let a = vec![1, 2, 3];
        let b = vec![1, 3, 4];
        let c = vec![2, 3, 4];
        let ballots = vec![a, b, c];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let r = election.run_election(1)?;
        assert_eq!(r, vec![1]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let r = election.run_election(1)?;
        assert_eq!(r, vec![1]);
        Ok(())
    }

    #[test]
    fn multi_way_tie_no_one_reaches_quota() -> Result<(), ElectionErr> {
        let a = vec![1];
        let b = vec![2];
        let c = vec![3];
        let ballots = vec![a, b, c];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let results = election.run_election(1)?;
        assert!(results.contains(&1) ^ results.contains(&2) ^ results.contains(&3));
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let results = election.run_election(1)?;
        assert!(results.contains(&1) ^ results.contains(&2) ^ results.contains(&3));
        Ok(())
    }

    #[test]
    fn elect_all() -> Result<(), ElectionErr> {
        let a = vec![1, 2];
        let b = vec![2, 1];
        let ballots = vec![a.clone(), b, a];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let results = election.run_election(3)?;
        assert_eq!(results, vec![1, 2]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let results = election.run_election(3)?;
        assert_eq!(results, vec![1, 2]);
        Ok(())
    }

    #[test]
    fn forced_random() -> Result<(), ElectionErr> {
        let a = vec![1, 2];
        let b = vec![2, 1];
        let c = vec![3];
        let d = vec![4];
        let ballots = vec![a.clone(), a.clone(), b.clone(), b.clone(), c, d];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let results = election.run_election(3)?;
        assert!(
            results.contains(&1)
                && results.contains(&2)
                && (results.contains(&3) ^ results.contains(&4))
        );
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let results = election.run_election(3)?;
        assert!(
            results.contains(&1)
                && results.contains(&2)
                && (results.contains(&3) ^ results.contains(&4))
        );
        Ok(())
    }

    #[test]
    fn elect_none() -> Result<(), ElectionErr> {
        let a = vec![1, 2, 3];
        let b = vec![1, 3, 4];
        let c = vec![2, 3, 4];
        let ballots = vec![a, b, c];
        let mut election = STV::new();
        election.add_ballots(ballots);
        let r = election.run_election(0)?;
        assert_eq!(r, vec![]);
        let d = vec![];
        election.add_ballots(vec![d.clone(), d.clone(), d.clone(), d]);
        let r = election.run_election(0)?;
        assert_eq!(r, vec![]);
        Ok(())
    }

    #[test]
    fn empty_votes() -> Result<(), ElectionErr> {
        let a: Vec<u8> = Vec::new();
        let mut election = STV::new();
        let r = election.run_election(1)?;
        assert!(r.is_empty());
        let ballots = vec![a.clone(), a.clone(), a];
        election.add_ballots(ballots);
        let r = election.run_election(1)?;
        assert!(r.is_empty());
        Ok(())
    }

    #[test]
    fn closed_election() -> Result<(), ElectionErr> {
        let v = vec![1, 2];
        let ballots = vec![v.clone(), v];
        let mut election = STV::with_candidates(&[2]);
        election.add_ballots(ballots);
        let r = election.run_election(2)?;
        assert_eq!(r, vec![2]);
        let r = election.run_election(1)?;
        assert_eq!(r, vec![2]);
        Ok(())
    }

    #[test]
    fn one_candidate_no_votes() -> Result<(), ElectionErr> {
        let mut election = STV::with_candidates(&[1]);
        let r = election.run_election(1)?;
        assert_eq!(r, vec![1]);
        Ok(())
    }

    #[test]
    fn two_candidates_no_votes() -> Result<(), ElectionErr> {
        let mut election = STV::with_candidates(&[1, 2]);
        let r = election.run_election(1)?;
        assert!(r.contains(&1) ^ r.contains(&2));
        Ok(())
    }

    #[test]
    fn deque_basics() -> Result<(), ElectionErr> {
        use std::collections::VecDeque;
        let mut voter_a = VecDeque::new();
        voter_a.push_front("sue");
        voter_a.push_front("bill");
        voter_a.push_front("bob");

        let mut voter_b = VecDeque::new();
        voter_b.push_front("bill");
        voter_b.push_front("bob");
        voter_b.push_front("sue");

        let mut voter_c = VecDeque::new();
        voter_c.push_front("bob");
        voter_c.push_front("sue");
        voter_c.push_front("bill");

        let mut voter_d = VecDeque::new();
        voter_d.push_front("sue");
        voter_d.push_front("bill");
        voter_d.push_front("bob");

        let mut voter_e = VecDeque::new();
        voter_e.push_front("bill");
        voter_e.push_front("bob");
        voter_e.push_front("sue");

        let vec = vec![voter_a, voter_b, voter_c, voter_d, voter_e];
        let mut election = STV::new();
        election.add_ballots(vec);
        let winner = election.run_election(1)?;
        assert_eq!(winner, vec!["sue"]);
        let winners = election.run_election(2)?;
        assert!(winners.contains(&"sue") & winners.contains(&"bob"));
        //assert_eq!(winners, vec!["sue", "bob"]);
        Ok(())
    }
}
