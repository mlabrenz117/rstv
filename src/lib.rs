use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;

pub struct STV<T> {
    candidates: Vec<T>,
    ballots: HashMap<Vec<T>, usize>,
    total_voters: usize,
    closed_election: bool,
    max_iterations: u64,
}

impl<T: Clone + Eq + Hash + Debug> STV<T> {
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
                    // This seems like a side affect, not sure if this is idiomatic
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
        let mut i = 0;
        loop {
            println!("{}", i);
            let new_electors = self.compute(&mut candidates, seats)?;
            for elector in new_electors.iter() {
                println!("Elected: {:?}", elector);
                elected.push(elector.clone());
            }
            if new_electors.len() == 0 || elected.len() >= seats {
                break;
            }
            i += 1;
        }

        if elected.len() < seats {}

        Ok(elected)
    }

    fn compute(
        &mut self,
        candidates: &mut HashMap<T, Candidate>,
        seats: usize,
    ) -> Result<Vec<T>, ElectionErr> {
        println!("{:?}", candidates);
        let mut iterations = 0;

        #[allow(unused_assignments)] // Closures don't count as reading?
        let mut quota = 0.;

        let num_elected = candidates
            .iter()
            .filter(|(_, c)| c.status == CandidateStatus::Elected(1.0))
            .count();

        loop {
            let mut converged = true;
            let mut total_excess: f64 = 0.0;

            for (ballot, n) in self.ballots.iter() {
                total_excess += ballot.iter().fold(0.0, |_excess, candidate| {
                    if let Some(cv) = candidates.get_mut(candidate) {
                        let a = cv.weight() * (*n as f64);
                        cv.add_votes(a);
                        (1.0 - cv.weight()) * (*n as f64)
                    } else {
                        *n as f64
                    }
                });
            }

            quota = {
                let v = ((self.total_voters as f64) - total_excess) / (seats + 1) as f64;
                if v >= 0.0001 {
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

            iterations += 1;
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
            .filter(|(_, c)| c.status == CandidateStatus::Hopeful && c.votes >= quota)
            .for_each(|(c, _)| {
                almost.insert(c.clone());
            });

        while almost.len() + num_elected > seats {
            candidates
                .iter_mut()
                .filter(|(_, c)| c.status == CandidateStatus::Hopeful) // LOOK_CLOSER
                .for_each(|(_, c)| c.status = CandidateStatus::Excluded);
            let lowest = candidates
                .iter_mut()
                // May need a filter here
                .min_by(|(_, c1), (_, c)| (*c).votes.partial_cmp(&(c1).votes).unwrap())
                .unwrap();
            lowest.1.status = CandidateStatus::Excluded;
            almost.remove(lowest.0);
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
            let val_a = (candidates.get(a).unwrap()).max_votes;
            let val_b = (candidates.get(b).unwrap()).max_votes;
            val_a.partial_cmp(&val_b).unwrap()
        });
        for (_, c) in candidates.iter_mut() {
            c.votes = 0.;
        }
        Ok(almost)
    }

    pub fn max_iterations(&mut self, n: u64) {
        self.max_iterations = n;
    }
}

impl<T: Clone + Eq + Hash + Debug> Default for STV<T> {
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
    fn it_works() {
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
        //let mut voting = STV::with_candidates(&["Mark", "Bob", "Tim"]);
        let mut voting = STV::new();
        voting.add_ballots(ballots);
        let results = voting.run_election(4);
        match results {
            Ok(results) => assert_eq!(results, vec!["Alice", "Bob", "Chris"]),
            Err(e) => {
                panic!("{}", e);
            }
        }
    }
}
