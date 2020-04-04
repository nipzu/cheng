use crate::position::{Position, Square, Square::*};
use std::time::{Duration, Instant};

pub struct Engine {
    root_node: Position,
    nodes_searched: usize,
    depth: usize,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            root_node: Position::new(),
            depth: 0,
            nodes_searched: 0,
        }
    }

    pub fn set_root_node(&mut self, new_root_node: &Position) {
        self.root_node = new_root_node.clone();
    }

    pub fn set_depth(&mut self, new_depth: usize) {
        self.depth = new_depth;
    }
}

pub struct SearchOutput {
    pub best_move: (i32, i32, i32, i32),
    pub nodes_searched: usize,
    pub search_time: Duration,
    pub search_depth: usize,
}

impl Engine {
    pub fn find_best_move(&mut self) -> SearchOutput {
        self.nodes_searched = 0;

        let start_time = Instant::now();
        let best_move = self.min_max_search(self.root_node.clone(), self.depth).0;
        let end_time = Instant::now();

        let search_time = end_time - start_time;
        SearchOutput {
            best_move,
            nodes_searched: self.nodes_searched,
            search_depth: self.depth,
            search_time,
        }
    }

    fn min_max_search(&mut self, position: Position, depth: usize) -> ((i32,i32,i32,i32), f64) {
        self.nodes_searched += 1;

        match position.is_draw_or_checkmate() {
            (true, false) => return ((-1,-1,-1,-1),0.0),
            (false, true) => {
                if position.is_white_turn() {
                    return ((-1,-1,-1,-1),std::f64::NEG_INFINITY);
                } else {
                    return ((-1,-1,-1,-1),std::f64::INFINITY);
                }
            }
            (false, false) => (),
            (true, true) => unreachable!(),
        }

        let mut possible_moves_evaluated = Vec::new();

        if depth == 1 {
            let next_positions = position.get_next_bare_boards();
            for (possible_move, position) in next_positions {
                self.nodes_searched += 1;
                let eval = Engine::heuristic_evaluation(position);
                possible_moves_evaluated.push((possible_move, eval));
            }
        } else {
            let possible_moves = position.get_possible_moves();
            for possible_move in possible_moves {
                let mut next_position = position.clone();
                next_position.make_move(*possible_move);
                let eval = self.min_max_search(next_position, depth - 1).1;
                possible_moves_evaluated.push((*possible_move, eval));
            }
        }

        possible_moves_evaluated
            .sort_by(|(_, eval_a), (_, eval_b)| eval_a.partial_cmp(&eval_b).unwrap());

        if position.is_white_turn() {
            *possible_moves_evaluated.last().unwrap()
        } else {
            *possible_moves_evaluated.first().unwrap()
        }
    }

    fn heuristic_evaluation(position: [Square; 64]) -> f64 {
        let mut material_score = 0.0;

        for piece in position.iter() {
            material_score += match piece {
                Empty => 0.0,
                WhitePawn => 1.0,
                BlackPawn => -1.0,
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
