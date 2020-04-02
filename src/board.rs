use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub struct Board {
    squares: [[Square; 8]; 8],
    earlier_moves: Vec<(i32, i32, i32, i32)>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub enum Square {
    Empty,
    WhiteKing,
    WhiteQueen,
    WhiteRook,
    WhiteBishop,
    WhiteKnight,
    WhitePawn,
    BlackKing,
    BlackQueen,
    BlackRook,
    BlackBishop,
    BlackKnight,
    BlackPawn,
}

use Square::*;

impl Square {
    pub fn is_white(self) -> bool {
        match self {
            WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn => true,
            _ => false,
        }
    }
}

struct PositionIterator {
    current_move: usize,
    current_board: Board,
    moves: Vec<(i32, i32, i32, i32)>,
}

impl PositionIterator {
    pub fn new(moves: Vec<(i32, i32, i32, i32)>) -> PositionIterator {
        PositionIterator {
            moves,
            current_board: Board::new(),
            current_move: 0,
        }
    }
}

impl Iterator for PositionIterator {
    type Item = Board;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_move >= self.moves.len() {
            return None;
        }

        self.current_board.make_move(self.moves[self.current_move]);
        self.current_move += 1;

        Some(self.current_board.clone())
    }
}

impl Eq for Board {}

impl PartialEq for Board {
    fn eq(&self, other: &Board) -> bool {
        self.squares == other.squares
    }
}

impl Hash for Board {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.squares.hash(state);
    }
}

impl Board {
    pub fn new() -> Board {
        Board {
            squares: [
                [
                    WhiteRook,
                    WhiteKnight,
                    WhiteBishop,
                    WhiteQueen,
                    WhiteKing,
                    WhiteBishop,
                    WhiteKnight,
                    WhiteRook,
                ],
                [WhitePawn; 8],
                [Empty; 8],
                [Empty; 8],
                [Empty; 8],
                [Empty; 8],
                [BlackPawn; 8],
                [
                    BlackRook,
                    BlackKnight,
                    BlackBishop,
                    BlackQueen,
                    BlackKing,
                    BlackBishop,
                    BlackKnight,
                    BlackRook,
                ],
            ],
            earlier_moves: vec![],
        }
    }

    pub fn get_earlier_positions(&self) -> impl Iterator<Item = Board> {
        PositionIterator::new(self.earlier_moves.clone())
    }

    pub fn is_checkmate(&self) -> bool {
        self.is_in_check(self.is_white_turn()) && self.get_possible_moves().is_empty()
    }

    pub fn is_draw(&self) -> bool {
        if !self.is_in_check(self.is_white_turn()) && self.get_possible_moves().is_empty() {
            return true;
        }
        if self.is_insufficient_material()
            || self.is_threefold_repetition()
            || self.is_50_move_rule()
        {
            return true;
        }

        false
    }

    fn is_50_move_rule(&self) -> bool {
        if self.earlier_moves.len() <= 100 {
            return false;
        }
        for (i, position) in self
            .get_earlier_positions()
            .enumerate()
            .take(self.earlier_moves.len() - 1)
            .skip(self.earlier_moves.len() - 101)
        {
            let (x1, y1, x2, y2) = self.earlier_moves[i];

            if position.get_square(x1, y1) == Some(WhitePawn)
                || position.get_square(x1, y1) == Some(BlackPawn)
                || position.get_square(x2, y2) == Some(Empty)
            {
                return false;
            }
        }
        false
    }

    fn is_threefold_repetition(&self) -> bool {
        let mut previous_positions = HashMap::new();

        for position in self.get_earlier_positions() {
            if let Some(count) = previous_positions.get_mut(&position) {
                *count += 1;
            } else {
                previous_positions.insert(position, 1);
            }
        }

        previous_positions.iter().any(|(_, count)| *count >= 3)
    }

    fn is_insufficient_material(&self) -> bool {
        if self.find_pieces(WhiteQueen).is_empty()
            && self.find_pieces(BlackQueen).is_empty()
            && self.find_pieces(WhiteRook).is_empty()
            && self.find_pieces(BlackRook).is_empty()
            && self.find_pieces(WhitePawn).is_empty()
            && self.find_pieces(BlackPawn).is_empty()
        {
            let white_bishops = self.find_pieces(WhiteBishop);
            let black_bishops = self.find_pieces(BlackBishop);
            let white_knights = self.find_pieces(WhiteKnight);
            let black_knights = self.find_pieces(BlackKnight);

            match (
                white_bishops.len(),
                black_bishops.len(),
                white_knights.len(),
                black_knights.len(),
            ) {
                (0, 0, 0, 0) | (1, 0, 0, 0) | (0, 1, 0, 0) | (0, 0, 1, 0) | (0, 0, 0, 1) => {
                    return true
                }
                (1, 1, 0, 0) => {
                    let wb = white_bishops[0];
                    let bb = black_bishops[0];
                    if wb.0 + wb.1 % 2 == bb.0 + bb.1 % 2 {
                        return true;
                    }
                }
                _ => (),
            }
        }

        false
    }

    pub fn is_check(&self) -> bool {
        self.is_in_check(self.is_white_turn())
    }

    fn is_in_check(&self, is_white: bool) -> bool {
        /*let own_king = if is_white { WhiteKing } else { BlackKing };
                let own_kings = self.find_pieces(own_king);

                assert_eq!(own_kings.len(), 1);

                let (kx, ky) = own_kings[0];
                for (_, _, x, y) in self.get_candidate_moves() {
                    if kx == x && ky == y {
                        return true;
                    }
                }
                false
        */

        let own_king = if is_white { WhiteKing } else { BlackKing };
        let own_kings = self.find_pieces(own_king);

        assert_eq!(own_kings.len(), 1);

        let (x, y) = own_kings[0];

        let check_tests = [
            Board::is_in_check_by_pawn,
            Board::is_in_check_by_king,
            Board::is_in_check_by_knight,
            Board::is_in_check_by_rook_or_queen,
            Board::is_in_check_by_bishop_or_queen,
        ];

        for check_test in check_tests.iter() {
            if check_test(self, x, y, is_white) {
                return true;
            }
        }
        false
    }

    pub fn make_move(&mut self, coords: (i32, i32, i32, i32)) {
        self.earlier_moves.push(coords);
        let (x1, y1, x2, y2) = coords;
        // castling
        if x1 == 4
            && y1 % 7 == 0
            && y1 == y2
            && (x1 - x2).abs() == 2
            && !self.has_square_moved(x1, y1)
        {
            let (rx1, rx2) = match x2 {
                2 => (0, 3),
                6 => (7, 5),
                _ => unreachable!(),
            };
            self.make_move((rx1, y1, rx2, y2));
        }

        self.set_square(x2, y2, self.get_square(x1, y1).unwrap());

        // TODO can only promote to queen
        if y1 == 6 && self.get_square(x1, y1) == Some(WhitePawn) {
            self.set_square(x2, y2, WhiteQueen)
        }
        if y1 == 1 && self.get_square(x1, y1) == Some(BlackPawn) {
            self.set_square(x2, y2, BlackQueen)
        }

        self.set_square(x1, y1, Empty);
    }

    pub fn get_possible_moves(&self) -> Vec<(i32, i32, i32, i32)> {
        self.get_candidate_moves()
            .filter(|cand_move| {
                let mut next_board = self.clone();
                next_board.make_move(*cand_move);
                !next_board.is_in_check(self.is_white_turn())
            })
            .collect()
    }

    pub fn is_white_turn(&self) -> bool {
        self.earlier_moves.len() % 2 == 0
    }

    /// returns true if the piece has been moved or captured by some other piece
    pub fn has_square_moved(&self, x: i32, y: i32) -> bool {
        self.earlier_moves
            .iter()
            .filter(|(fx, fy, tx, ty)| (x == *fx && y == *fy) || (x == *tx && y == *ty))
            .count()
            > 0
    }

    fn get_candidate_moves(&self) -> impl Iterator<Item = (i32, i32, i32, i32)> + '_ {
        (0..8)
            .map(move |x| (0..8).map(move |y| self.get_candidate_moves_for_square(x, y)))
            .flatten()
            .flatten()
    }

    fn get_candidate_moves_for_square(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        if let Some(piece) = self.get_square(x, y) {
            if piece != Empty && piece.is_white() == self.is_white_turn() {
                match piece {
                    WhiteKing | BlackKing => return self.get_king_candidate_moves(x, y),
                    WhiteQueen | BlackQueen => return self.get_queen_candidate_moves(x, y),
                    WhiteRook | BlackRook => return self.get_rook_candidate_moves(x, y),
                    WhiteBishop | BlackBishop => return self.get_bishop_candidate_moves(x, y),
                    WhiteKnight | BlackKnight => return self.get_knight_candidate_moves(x, y),
                    WhitePawn | BlackPawn => return self.get_pawn_candidate_moves(x, y),
                    _ => unreachable!(),
                }
            }
        }
        Vec::new()
    }
    fn get_pawn_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        let (dy, enemy_pawn) = if self.is_white_turn() {
            (1, BlackPawn)
        } else {
            (-1, WhitePawn)
        };

        let mut candidate_moves = Vec::new();
        for dx in [1, -1].iter() {
            // basic capturing
            if let Some(piece) = self.get_square(x + dx, y + dy) {
                if piece != Empty && piece.is_white() != self.is_white_turn() {
                    candidate_moves.push((x, y, x + dx, y + dy));
                }
            }
            // en passant
            if let Some(piece) = self.get_square(x + dx, y) {
                if piece == enemy_pawn
                    && self.earlier_moves.last() == Some(&(x + dx, y - 2 * dy, x + dx, y))
                {
                    candidate_moves.push((x, y, x + dx, y + dy));
                }
            }
        }
        // Moving forward
        if let Some(piece) = self.get_square(x, y + dy) {
            if piece == Empty {
                candidate_moves.push((x, y, x, y + dy));
                if let Some(piece) = self.get_square(x, y + 2 * dy) {
                    if piece == Empty && y % 5 == 1 {
                        candidate_moves.push((x, y, x, y + 2 * dy));
                    }
                }
            }
        }

        candidate_moves
    }

    fn get_knight_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        let mut candidate_moves = Vec::new();
        for (dx, dy) in [
            (1, 2),
            (1, -2),
            (-1, 2),
            (-1, -2),
            (2, 1),
            (2, -1),
            (-2, 1),
            (-2, -1),
        ]
        .iter()
        {
            if let Some(piece) = self.get_square(x + dx, y + dy) {
                if piece == Empty || piece.is_white() != self.is_white_turn() {
                    candidate_moves.push((x, y, x + dx, y + dy));
                }
            }
        }

        candidate_moves
    }

    fn get_king_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        let mut candidate_moves = Vec::new();
        // basic moves
        for dx in -1..=1 {
            for dy in -1..=1 {
                if let Some(piece) = self.get_square(x + dx, y + dy) {
                    if piece == Empty || piece.is_white() != self.is_white_turn() {
                        candidate_moves.push((x, y, x + dx, y + dy))
                    }
                }
            }
        }
        let y = if self.is_white_turn() { 0 } else { 7 };

        // check if king moved
        if !self.has_square_moved(4, y) && !self.is_in_check(self.is_white_turn()) {
            // castling kingside
            if !self.has_square_moved(7, y)
                && self.get_square(5, y) == Some(Empty)
                && self.get_square(6, y) == Some(Empty)
            {
                let mut test_check_5 = self.clone();
                let mut test_check_6 = self.clone();
                test_check_5.make_move((4, y, 5, y));
                test_check_6.make_move((4, y, 6, y));
                if !test_check_5.is_in_check(self.is_white_turn())
                    && !test_check_6.is_in_check(self.is_white_turn())
                {
                    candidate_moves.push((x, y, x + 2, y));
                }
            }
            // castling queenside
            if !self.has_square_moved(0, y)
                && self.get_square(3, y) == Some(Empty)
                && self.get_square(2, y) == Some(Empty)
            {
                let mut test_check_3 = self.clone();
                let mut test_check_2 = self.clone();
                test_check_3.make_move((4, y, 3, y));
                test_check_2.make_move((4, y, 2, y));
                if !test_check_3.is_in_check(self.is_white_turn())
                    && !test_check_2.is_in_check(self.is_white_turn())
                {
                    candidate_moves.push((x, y, x - 2, y));
                }
            }
        }

        candidate_moves
    }

    fn get_queen_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        let mut candidate_moves = self.get_rook_candidate_moves(x, y);
        candidate_moves.append(&mut self.get_bishop_candidate_moves(x, y));
        candidate_moves
    }

    fn get_rook_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        use std::iter::repeat;

        let mut candidate_moves = Vec::new();
        for deltas in vec![
            &mut (x + 1..8).zip(repeat(y)) as &mut dyn Iterator<Item = (i32, i32)>,
            &mut (repeat(x)).zip(y + 1..8),
            &mut (0..x).rev().zip(repeat(y)),
            &mut (repeat(x)).zip((0..y).rev()),
        ] {
            for (dx, dy) in deltas {
                if let Some(piece) = self.get_square(dx, dy) {
                    if piece == Empty {
                        candidate_moves.push((x, y, dx, dy));
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves.push((x, y, dx, dy));
                        break;
                    } else {
                        break;
                    }
                }
            }
        }

        candidate_moves
    }

    fn get_bishop_candidate_moves(&self, x: i32, y: i32) -> Vec<(i32, i32, i32, i32)> {
        let mut candidate_moves = Vec::new();
        for deltas in vec![
            &mut (x + 1..8).zip(y + 1..8) as &mut dyn Iterator<Item = (i32, i32)>,
            &mut (x + 1..8).zip((0..y).rev()),
            &mut (0..x).rev().zip(y + 1..8),
            &mut (0..x).rev().zip((0..y).rev()),
        ] {
            for (dx, dy) in deltas {
                if let Some(piece) = self.get_square(dx, dy) {
                    if piece == Empty {
                        candidate_moves.push((x, y, dx, dy));
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves.push((x, y, dx, dy));
                        break;
                    } else {
                        break;
                    }
                }
            }
        }

        candidate_moves
    }

    pub fn set_square(&mut self, x: i32, y: i32, square: Square) {
        if 0 <= x && x <= 7 && 0 <= y && y <= 7 {
            self.squares[y as usize][x as usize] = square;
        }
    }

    pub fn get_square(&self, x: i32, y: i32) -> Option<Square> {
        if 0 <= x && x <= 7 && 0 <= y && y <= 7 {
            Some(self.squares[y as usize][x as usize])
        } else {
            None
        }
    }

    fn find_pieces(&self, piece: Square) -> Vec<(i32, i32)> {
        self.squares
            .iter()
            .flatten()
            .enumerate()
            .filter(|(_, s)| *s == &piece)
            .map(|(i, _)| ((i % 8) as i32, (i / 8) as i32))
            .collect()
    }

    fn is_in_check_by_pawn(&self, x: i32, y: i32, is_white: bool) -> bool {
        let (dy, enemy_pawn) = if is_white {
            (1, BlackPawn)
        } else {
            (-1, WhitePawn)
        };

        self.get_square(x - 1, y + dy) == Some(enemy_pawn)
            || self.get_square(x + 1, y + dy) == Some(enemy_pawn)
    }

    fn is_in_check_by_king(&self, x: i32, y: i32, is_white: bool) -> bool {
        let enemy_king = if is_white { BlackKing } else { WhiteKing };

        for dx in -1..=1 {
            for dy in -1..=1 {
                if self.get_square(x + dx, y + dy) == Some(enemy_king) {
                    return true;
                }
            }
        }
        false
    }

    fn is_in_check_by_knight(&self, x: i32, y: i32, is_white: bool) -> bool {
        let enemy_knight = if is_white { BlackKnight } else { WhiteKnight };

        if self.get_square(x + 1, y + 2) == Some(enemy_knight)
            || self.get_square(x + 1, y - 2) == Some(enemy_knight)
            || self.get_square(x - 1, y + 2) == Some(enemy_knight)
            || self.get_square(x - 1, y - 2) == Some(enemy_knight)
            || self.get_square(x + 2, y + 1) == Some(enemy_knight)
            || self.get_square(x + 2, y - 1) == Some(enemy_knight)
            || self.get_square(x - 2, y + 1) == Some(enemy_knight)
            || self.get_square(x - 2, y - 1) == Some(enemy_knight)
        {
            return true;
        }
        false
    }

    fn is_in_check_by_rook_or_queen(&self, x: i32, y: i32, is_white: bool) -> bool {
        use std::iter::repeat;

        let (enemy_rook, enemy_queen) = if is_white {
            (BlackRook, BlackQueen)
        } else {
            (WhiteRook, WhiteQueen)
        };

        for deltas in vec![
            &mut (x + 1..8).zip(repeat(y)) as &mut dyn Iterator<Item = (i32, i32)>,
            &mut (repeat(x)).zip(y + 1..8),
            &mut (0..x).rev().zip(repeat(y)),
            &mut (repeat(x)).zip((0..y).rev()),
        ] {
            for (dx, dy) in deltas {
                if self.get_square(dx, dy) == Some(enemy_rook)
                    || self.get_square(dx, dy) == Some(enemy_queen)
                {
                    return true;
                } else if self.get_square(dx, dy) != Some(Empty) {
                    break;
                }
            }
        }

        false
    }

    fn is_in_check_by_bishop_or_queen(&self, x: i32, y: i32, is_white: bool) -> bool {
        let (enemy_bishop, enemy_queen) = if is_white {
            (BlackBishop, BlackQueen)
        } else {
            (WhiteBishop, WhiteQueen)
        };

        for deltas in vec![
            &mut (x + 1..8).zip(y + 1..8) as &mut dyn Iterator<Item = (i32, i32)>,
            &mut (x + 1..8).zip((0..y).rev()),
            &mut (0..x).rev().zip(y + 1..8),
            &mut (0..x).rev().zip((0..y).rev()),
        ] {
            for (dx, dy) in deltas {
                if self.get_square(dx, dy) == Some(enemy_bishop)
                    || self.get_square(dx, dy) == Some(enemy_queen)
                {
                    return true;
                } else if self.get_square(dx, dy) != Some(Empty) {
                    break;
                }
            }
        }
        false
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();
        for row in self.squares.iter().rev() {
            for piece in row {
                text += &format!(
                    "{} ",
                    match piece {
                        Square::Empty => ".",
                        WhiteKing => "K",
                        WhiteQueen => "Q",
                        WhiteRook => "R",
                        WhiteBishop => "B",
                        WhiteKnight => "N",
                        WhitePawn => "P",
                        BlackKing => "k",
                        BlackQueen => "q",
                        BlackRook => "r",
                        BlackBishop => "b",
                        BlackKnight => "n",
                        BlackPawn => "p",
                    }
                );
            }
            text += &"\n";
        }
        // remove last newline
        text.pop();

        write!(f, "{}", text)
    }
}
