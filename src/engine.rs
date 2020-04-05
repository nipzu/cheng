use std::fmt;
use std::time::{Duration, Instant};

use crate::position::{Move, Position, Square, Square::*};

pub struct Engine {
    game_tree: Vec<Position>,
    nodes_searched: usize,
    root_node: usize,
    depth: usize,
}

pub struct SearchOutput {
    pub best_move: Option<Move>,
    pub nodes_searched: usize,
    pub search_time: Duration,
    pub search_depth: usize,
    pub evaluation: f64,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            game_tree: vec![Position::new()],
            depth: 0,
            nodes_searched: 0,
            root_node: 0,
        }
    }

    pub fn set_depth(&mut self, new_depth: usize) {
        self.depth = new_depth;
    }

    pub fn set_game_tree(&mut self, move_iter: &mut dyn Iterator<Item = Move>) {
        self.game_tree = vec![Position::new()];
        for next_move in move_iter {
            let mut next_position = self.game_tree.last().unwrap().clone();
            next_position.make_move(next_move);
            self.game_tree.push(next_position);
        }
        self.root_node = self.game_tree.len() - 1;
    }

    pub fn search_game_tree(&mut self) -> SearchOutput {
        self.nodes_searched = 0;

        let start_time = Instant::now();
        let (best_move, mut evaluation) = self.min_max_search();
        let end_time = Instant::now();

        if !self.game_tree[self.root_node].is_white_turn() {
            evaluation *= -1.0;
        }

        let search_time = end_time - start_time;
        SearchOutput {
            best_move,
            nodes_searched: self.nodes_searched,
            search_depth: self.depth,
            evaluation,
            search_time,
        }
    }

    fn min_max_search(&mut self) -> (Option<Move>, f64) {
        self.nodes_searched += 1;

        assert!(self.game_tree.len() <= self.root_node + self.depth + 1);

        if self.game_tree.len() == self.root_node + self.depth + 1 {
            return (
                None,
                Self::heuristic_evaluation(self.game_tree.last().unwrap().get_squares()),
            );
        }

        let mut num_same_boards = 1;
        for i in self
            .game_tree
            .iter()
            .enumerate()
            .rev()
            .take_while(|(_, pos)| !pos.resets_draw_counters())
            .map(|(i, _)| i)
        {
            if self.game_tree[i - 1] == *self.game_tree.last().unwrap() {
                num_same_boards += 1;
            }
            if self.game_tree.len() - i >= 100 {
                return (None, 0.0);
            }
            // cant be reached more than once
            if num_same_boards >= 3
                && (!self.game_tree[i - 1].can_en_passant()
                    || self.game_tree[i - 1].get_possible_moves().len()
                        == self.game_tree.last().unwrap().get_possible_moves().len())
            {
                return (None, 0.0);
            }
        }

        let possible_moves = self.game_tree.last().unwrap().get_possible_moves();

        if possible_moves.is_empty() {
            return if self.game_tree.last().unwrap().is_check() {
                if self.game_tree.last().unwrap().is_white_turn() {
                    (None, std::f64::NEG_INFINITY)
                } else {
                    (None, std::f64::INFINITY)
                }
            } else {
                (None, 0.0)
            };
        }

        if self.game_tree.last().unwrap().is_insufficient_material() {
            return (None, 0.0);
        }

        let mut possible_moves_evaluated = Vec::new();

        let cur_node = self.game_tree.last().unwrap().clone();
        self.game_tree.push(cur_node.clone());
        for possible_move in possible_moves {
            *self.game_tree.last_mut().unwrap() = cur_node.clone();
            self.game_tree.last_mut().unwrap().make_move(possible_move);
            let eval = self.min_max_search().1;
            possible_moves_evaluated.push((Some(possible_move), eval));
        }
        self.game_tree.pop();

        let best_move = if self.game_tree.last().unwrap().is_white_turn() {
            *possible_moves_evaluated
                .iter()
                .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
                .unwrap()
        } else {
            *possible_moves_evaluated
                .iter()
                .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
                .unwrap()
        };

        best_move
    }

    fn heuristic_evaluation(position: &[Square; 64]) -> f64 {
        let mut material_score = 0.0;

        for (i, piece) in position.iter().enumerate() {
            material_score += match piece {
                Empty => 0.0,
                WhitePawn => 1.0 + (i / 8 - 1) as f64 / 80.0,
                BlackPawn => -1.0 - (7 - i / 8) as f64 / 80.0,
                WhiteKnight => 3.0,
                BlackKnight => -3.0,
                WhiteBishop => 3.0,
                BlackBishop => -3.0,
                WhiteRook => 5.0,
                BlackRook => -5.0,
                WhiteKing => 0.0,
                BlackKing => 0.0,
                WhiteQueen => 9.0,
                BlackQueen => -9.0,
            }
        }
        material_score
    }
}

impl fmt::Debug for Engine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self.game_tree[self.root_node])?;
        writeln!(f, "possible moves:")?;
        for possible_move in self.game_tree[self.root_node].get_possible_moves() {
            writeln!(f, "{}", possible_move)?;
        }

        Ok(())
    }
}
