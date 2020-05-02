use std::fmt;
use std::time::{Duration, Instant};

use crate::position::{Move, Position, Square, Square::*};

use async_std::task;

#[derive(Clone)]
pub struct Engine {
    game_tree: Vec<Position>,
    nodes_searched: usize,
    root_node: usize,
    depth: usize,
}

#[derive(PartialEq, Copy, Clone)]
pub enum Evaluation {
    Heuristic(f64),
    MateIn(i32),
}

impl Evaluation {
    pub fn increase_mate_dist(&mut self) {
        if let Evaluation::MateIn(dist) = self {
            *dist += dist.signum();
        }
    }
}

impl fmt::Display for Evaluation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Evaluation::Heuristic(eval) => write!(f, "cp {}", (eval * 100.0).round() as i32),
            Evaluation::MateIn(dist) => {
                let half_move_dist = dist - dist.signum();
                write!(f, "mate {}", half_move_dist / 2 + half_move_dist % 2)
            }
        }
    }
}

impl std::ops::Neg for Evaluation {
    type Output = Evaluation;
    fn neg(self) -> Self::Output {
        match self {
            Evaluation::Heuristic(eval) => Evaluation::Heuristic(-eval),
            Evaluation::MateIn(dist) => Evaluation::MateIn(-dist),
        }
    }
}

impl PartialOrd for Evaluation {
    fn partial_cmp(&self, other: &Evaluation) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Evaluation::MateIn(s), Evaluation::MateIn(o)) => {
                if s.signum() == o.signum() {
                    o.abs().partial_cmp(&s.abs())
                } else {
                    Some(s.cmp(&o))
                }
            }
            (Evaluation::Heuristic(s), Evaluation::Heuristic(o)) => s.partial_cmp(o),
            (Evaluation::MateIn(s), Evaluation::Heuristic(_)) => Some(s.cmp(&0)),
            (Evaluation::Heuristic(_), Evaluation::MateIn(o)) => Some(0.cmp(o)),
        }
    }
}

pub struct SearchOutput {
    pub best_move: Option<Move>,
    pub nodes_searched: usize,
    pub search_time: Duration,
    pub search_depth: usize,
    pub evaluation: Evaluation,
}

pub async fn search_game_tree(game_tree: Vec<Position>, depth: usize) -> SearchOutput {
    let mut nodes_searched = 0;

    let mut moves_to_evaluate = Vec::new();
    game_tree
        .last()
        .unwrap()
        .get_candidate_moves(&mut moves_to_evaluate);
    moves_to_evaluate = moves_to_evaluate
        .into_iter()
        .filter(|m| game_tree.last().unwrap().clone().make_move(*m))
        .collect();

    let start_time = Instant::now();

    let mut tasks = Vec::new();

    let search_to_move = game_tree.len() - 1 + depth;

    for m in moves_to_evaluate {
        let mut game_tree_cloned = game_tree.to_vec();
        let mut candidate_moves_buffer = Vec::with_capacity(depth);
        for _ in 0..depth {
            candidate_moves_buffer.push(Vec::with_capacity(256));
        }
        let mut next_position = game_tree.last().unwrap().clone();
        next_position.make_move(m);
        game_tree_cloned.push(next_position);
        tasks.push(task::spawn_blocking(move || {
            (
                min_max_search(
                    &mut game_tree_cloned,
                    &mut candidate_moves_buffer,
                    search_to_move,
                    Evaluation::MateIn(1),
                    Evaluation::MateIn(-1),
                ),
                m,
            )
        }));
    }

    let (mut best_move, mut evaluation) = (
        None,
        Evaluation::MateIn(if game_tree.last().unwrap().is_white_turn() {
            -2
        } else {
            2
        }),
    );

    for t in tasks {
        let ((_, eval, new_nodes_searched), best_move_candidate) = t.await;
        nodes_searched += new_nodes_searched;
        if game_tree.last().unwrap().is_white_turn() {
            if eval >= evaluation {
                best_move = Some(best_move_candidate);
                evaluation = eval;
            }
        } else if eval <= evaluation {
            best_move = Some(best_move_candidate);
            evaluation = eval;
        }

        let end_time = Instant::now();
        let search_time = end_time - start_time;
        let out = SearchOutput {
            best_move,
            nodes_searched,
            search_depth: depth,
            evaluation: if game_tree.last().unwrap().is_white_turn() {
                evaluation
            } else {
                -evaluation
            },
            search_time,
        };
        println!("{}", out);
    }

    if !game_tree.last().unwrap().is_white_turn() {
        evaluation = -evaluation;
    }

    let end_time = Instant::now();
    let search_time = end_time - start_time;
    SearchOutput {
        best_move,
        nodes_searched,
        search_depth: depth,
        evaluation,
        search_time,
    }
}

fn is_position_draw(game_tree: &mut [Position]) -> bool {
    let mut num_same_boards = 1;
    for i in game_tree
        .iter()
        .enumerate()
        .rev()
        .take_while(|(_, pos)| !pos.resets_draw_counters())
        .map(|(i, _)| i)
    {
        if game_tree[i - 1] == *game_tree.last().unwrap() {
            num_same_boards += 1;
        }
        if game_tree.len() - i >= 100 {
            return true;
        }
        // cant be reached more than once
        if num_same_boards >= 3 && !game_tree[i - 1].can_en_passant() {
            return true;
        }
    }

    if game_tree.last().unwrap().is_insufficient_material() {
        return true;
    }

    false
}

fn min_max_search(
    game_tree: &mut Vec<Position>,
    candidate_moves_buffer: &mut [Vec<Move>],
    search_to_move: usize,
    black_max_eval: Evaluation,
    white_min_eval: Evaluation,
) -> (Option<Move>, Evaluation, usize) {
    if game_tree.len() - 1 == search_to_move {
        return (
            None,
            heuristic_evaluation(game_tree.last().unwrap().get_squares()),
            1,
        );
    }

    if is_position_draw(game_tree) {
        return (None, Evaluation::Heuristic(0.0), 1);
    }
    let (move_buffer, candidate_moves_buffer) = candidate_moves_buffer.split_first_mut().unwrap();

    move_buffer.clear();
    game_tree.last().unwrap().get_candidate_moves(move_buffer);

    game_tree.push(game_tree.last().unwrap().clone());
    let mut best_move = min_max_search_moves(
        game_tree,
        move_buffer,
        candidate_moves_buffer,
        search_to_move,
        black_max_eval,
        white_min_eval,
    );
    game_tree.pop();

    best_move.1.increase_mate_dist();

    // checkmate or stalemate
    if let None = best_move.0 {
        return if game_tree.last().unwrap().is_check() {
            (
                None,
                Evaluation::MateIn(if game_tree.last().unwrap().is_white_turn() {
                    -1
                } else {
                    1
                }),
                1,
            )
        } else {
            (None, Evaluation::Heuristic(0.0), 1)
        };
    }

    best_move
}

fn min_max_search_moves(
    game_tree: &mut Vec<Position>,
    move_buffer: &[Move],
    candidate_moves_buffer: &mut [Vec<Move>],
    search_to_move: usize,
    black_max_eval: Evaluation,
    white_min_eval: Evaluation,
) -> (Option<Move>, Evaluation, usize) {
    let is_white_turn = game_tree.last().unwrap().is_white_turn();

    let mut best_move: (Option<Move>, Evaluation, usize) = (
        None,
        Evaluation::MateIn(if is_white_turn { -2 } else { 2 }),
        1,
    );

    for priority_level in 0..=3 {
        for m in move_buffer.iter() {
            *game_tree.last_mut().unwrap() = game_tree[game_tree.len() - 2].clone();
            if game_tree[game_tree.len() - 2].get_move_priority_level(*m) == priority_level
                && game_tree.last_mut().unwrap().make_move(*m)
            {
                if is_white_turn {
                    let (_, eval, new_nodes) = min_max_search(
                        game_tree,
                        candidate_moves_buffer,
                        search_to_move,
                        black_max_eval,
                        best_move.1,
                    );

                    if eval >= best_move.1 {
                        best_move.0 = Some(*m);
                        best_move.1 = eval;
                    }
                    best_move.2 += new_nodes;
                    if eval > black_max_eval {
                        return best_move;
                    }
                } else {
                    let (_, eval, new_nodes) = min_max_search(
                        game_tree,
                        candidate_moves_buffer,
                        search_to_move,
                        best_move.1,
                        white_min_eval,
                    );
                    if eval <= best_move.1 {
                        best_move.0 = Some(*m);
                        best_move.1 = eval;
                    }
                    best_move.2 += new_nodes;
                    if eval < white_min_eval {
                        return best_move;
                    }
                }
            }
        }
    }

    best_move
}

fn heuristic_evaluation(position: &[Square; 64]) -> Evaluation {
    let mut material_score = 0.0;
    for (i, piece) in position.iter().enumerate() {
        material_score += get_piece_value_at_position(*piece, i);
    }
    Evaluation::Heuristic(material_score)
}

pub fn get_piece_value_at_position(piece: Square, position: usize) -> f64 {
    match piece {
        WhitePawn => 1.0 + (position / 8 - 1) as f64 / 80.0,
        BlackPawn => -1.0 - (7 - position / 8) as f64 / 80.0,
        _ => piece.get_value(),
    }
}

impl fmt::Display for SearchOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SearchOutput {
            best_move,
            search_time,
            nodes_searched,
            search_depth,
            evaluation,
        } = *self;
        let best_move_or_none = if let Some(m) = best_move {
            format!("{}", m)
        } else {
            String::from("(none)")
        };

        let nps = (nodes_searched as f64 / search_time.as_secs_f64()) as usize;
        write!(
            f,
            "info depth {} nodes {} nps {} time {} score {} pv {}",
            search_depth,
            nodes_searched,
            nps,
            search_time.as_millis(),
            evaluation,
            best_move_or_none
        )
    }
}
