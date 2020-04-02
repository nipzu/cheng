use crate::board::{Board, Square::*};

pub fn find_best_move(board: &Board) -> (i32, i32, i32, i32) {
    let mut moves = board
        .get_possible_moves()
        .into_iter()
        .map(|m| {
            let mut next_position = board.clone();
            next_position.make_move(m);
            (m, min_max_search(next_position, 3))
        })
        .collect::<Vec<_>>();
    moves.sort_by(|(_, eval_a), (_, eval_b)| eval_a.partial_cmp(&eval_b).unwrap());

    if board.is_white_turn() {
        moves.last().unwrap().0
    } else {
        moves.first().unwrap().0
    }
}

pub fn min_max_search(board: Board, depth: usize) -> f64 {
    if depth == 0 {
        return heuristic_evaluation(board);
    }

    if board.is_checkmate() {
        if board.is_white_turn() {
            return std::f64::NEG_INFINITY;
        } else {
            return std::f64::INFINITY;
        }
    }

    let mut moves = board
        .get_possible_moves()
        .into_iter()
        .map(|m| {
            let mut next_position = board.clone();
            next_position.make_move(m);
            (m, min_max_search(next_position, depth - 1))
        })
        .collect::<Vec<_>>();

    moves.sort_by(|(_, eval_a), (_, eval_b)| eval_a.partial_cmp(&eval_b).unwrap());

    if board.is_white_turn() {
        moves.last().unwrap().1
    } else {
        moves.first().unwrap().1
    }
}

pub fn heuristic_evaluation(board: Board) -> f64 {
    let mut white_material_score = 0.0;
    for (piece, multiplier) in [
        (WhiteQueen, 9.0),
        (WhiteRook, 5.0),
        (WhiteBishop, 3.0),
        (WhiteKnight, 3.0),
        (WhitePawn, 1.0),
    ]
    .iter()
    {
        white_material_score += multiplier * board.find_pieces(*piece).len() as f64;
    }

    let mut black_material_score = 0.0;
    for (piece, multiplier) in [
        (BlackQueen, 9.0),
        (BlackRook, 5.0),
        (BlackBishop, 3.0),
        (BlackKnight, 3.0),
        (BlackPawn, 1.0),
    ]
    .iter()
    {
        black_material_score += multiplier * board.find_pieces(*piece).len() as f64;
    }

    white_material_score - black_material_score
}
