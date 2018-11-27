use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

pub struct STV<T> {
    candidates: Vec<T>,
    ballots: HashMap<Vec<T>, usize>,
    total_voters: usize,
    accept_all_candidates: bool,
}

impl<T: Clone + Eq + Hash> STV<T> {
    pub fn new() -> Self {
        STV {
            candidates: Vec::new(),
            ballots: HashMap::new(),
            total_voters: 0,
            accept_all_candidates: true,
        }
    }

    pub fn with_candidates(candidates: &[T]) -> Self {
        STV {
            candidates: candidates.to_vec(),
            ballots: HashMap::new(),
            total_voters: 0,
            accept_all_candidates: false,
        }
    }

    pub fn add_ballots(&mut self, ballots: impl IntoIterator<Item = impl IntoIterator<Item = T>>) {
        for ballot in ballots {
            let ballot: Vec<T> = ballot
                .into_iter()
                .inspect(|candidate| {
                    // This seems like a side affect, not sure if this is idiomatic
                    if self.accept_all_candidates && !self.candidates.contains(&candidate) {
                        self.candidates.push(candidate.clone())
                    }
                })
                .collect();
            *self.ballots.entry(ballot).or_insert(0) += 1;
            self.total_voters += 1;
        }
    }

    pub fn run_election(&self, seats: usize) -> Result<Vec<T>, ElectionErr> {
        // Setup
        let mut elected = Vec::new();
        let mut total_excess: f64 = 0.0;
        let mut candidates: HashMap<T, Candidate> = self
            .candidates
            .iter()
            .cloned()
            .map(|x| (x, Candidate::new()))
            .collect();

        if candidates.len() < seats {
            return Err(ElectionErr::A);
        }

        // Main Loop
        while elected.len() < seats {
            //2.5
            let quota = ((self.total_voters as f64) - total_excess) / (seats + 1) as f64;
            for (c, cv) in candidates.iter_mut() {
                if cv.status == CandidateStatus::Elected(0.0) {
                    // 2.9 Iterative Process here
                }
            }

            for (ballot, n) in self.ballots.iter() {
                total_excess += ballot.iter().fold(0.0, |_excess, candidate| {
                    let cv = candidates.get_mut(candidate).unwrap();
                    let a = cv.weight() * (*n as f64);
                    cv.votes += a;
                    (1.0 - cv.weight()) * (*n as f64)
                });
            }
        }

        Ok(elected)
    }
}

struct Candidate {
    votes: f64,
    status: CandidateStatus,
}

impl Candidate {
    fn new() -> Self {
        Candidate {
            votes: 0.0,
            status: CandidateStatus::Hopeful,
        }
    }

    fn weight(&self) -> f64 {
        self.status.weight()
    }
}

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
}

impl PartialEq<CandidateStatus> for CandidateStatus {
    fn eq(&self, other: &CandidateStatus) -> bool {
        use CandidateStatus::*;
        match (self, other) {
            (Hopeful, Hopeful) => true,
            (Excluded, Excluded) => true,
            (Elected(_), Elected(_)) => true,
            _ => false,
        }
    }
}

pub enum ElectionErr {
    A,
    B,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {}
}
