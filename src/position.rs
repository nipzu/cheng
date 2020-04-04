use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use bitflags::bitflags;

#[derive(Clone)]
pub struct Position {
    squares: [Square; 64],
    earlier_moves: Vec<Move>,
    //possible_moves: Vec<Move>,
    move_draw_counter: u8,
    position_flags: PositionFlags,
    //is_check: bool,
}

bitflags! {
    struct PositionFlags: u8 {
        //const IS_CHECK                 = 0b0000_0001;
        const CAN_WHITE_CASTLE_KINGSIDE  = 0b0000_0010;
        const CAN_BLACK_CASTLE_KINGSIDE  = 0b0000_0100;
        const CAN_WHITE_CASTLE_QUEENSIDE = 0b0000_1000;
        const CAN_BLACK_CASTLE_QUEENSIDE = 0b0001_0000;
        // TODO add en passant
        const STARTING_POS               = 0b0001_1110;
    }
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
    current_position: Position,
    moves: Vec<Move>,
}

impl PositionIterator {
    pub fn new(moves: Vec<Move>) -> PositionIterator {
        PositionIterator {
            moves,
            current_position: Position::new(),
            current_move: 0,
        }
    }
}

bitflags! {
    struct MoveFlags: u8 {
        const IS_PAWN_MOVE      = 0b0000_0001;
        const IS_CAPTURE        = 0b0000_0010;
        const PROMOTE_TO_QUEEN  = 0b0000_0101;
        const PROMOTE_TO_ROOK   = 0b0000_1001;
        const PROMOTE_TO_BISHOP = 0b0001_0001;
        const PROMOTE_TO_KNIGHT = 0b0010_0001;
        //const IS_CASTLING     = 0b0100_0000;
        const FLAGS_UNKNOWN     = 0b1000_0000;
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Move {
    from: u8,
    to: u8,
    flags: MoveFlags,
}

impl Move {
    pub fn from_notation(text: &str) -> Move {
        let mut bytes = text.bytes();
        let from_x = bytes.next().unwrap() - 97;
        let from_y = bytes.next().unwrap() - 49;
        let to_x = bytes.next().unwrap() - 97;
        let to_y = bytes.next().unwrap() - 49;

        let flags = match bytes.next() {
            Some(b'q') => MoveFlags::PROMOTE_TO_QUEEN,
            Some(b'r') => MoveFlags::PROMOTE_TO_ROOK,
            Some(b'b') => MoveFlags::PROMOTE_TO_BISHOP,
            Some(b'n') => MoveFlags::PROMOTE_TO_KNIGHT,
            _ => MoveFlags::FLAGS_UNKNOWN,
        };

        Move {
            from: 8 * from_y + from_x,
            to: 8 * to_y + to_x,
            flags,
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const FILES: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        let from_file = FILES[(self.from % 8) as usize];
        let from_rank = self.from / 8 + 1;
        let to_file = FILES[(self.to % 8) as usize];
        let to_rank = self.to / 8 + 1;
        let promote_to = match (self.flags.bits() >> 2).trailing_zeros() {
            0 => 'q',
            1 => 'r',
            2 => 'b',
            3 => 'n',
            _ => ' ',
        };
        write!(
            f,
            "{}{}{}{}{}",
            from_file, from_rank, to_file, to_rank, promote_to
        )
    }
}

impl Iterator for PositionIterator {
    type Item = Position;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_move >= self.moves.len() {
            return None;
        }

        self.current_position
            .make_move(self.moves[self.current_move]);
        self.current_move += 1;

        Some(self.current_position.clone())
    }
}

impl Eq for Position {}

impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {
        self.squares
            .iter()
            .zip(other.squares.iter())
            .all(|(s1, s2)| s1 == s2)
    }
}

impl Hash for Position {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.squares.hash(state);
    }
}

impl Position {
    pub fn new() -> Position {
        let mut squares = [Empty; 64];
        squares[0..8].copy_from_slice(&[
            WhiteRook,
            WhiteKnight,
            WhiteBishop,
            WhiteQueen,
            WhiteKing,
            WhiteBishop,
            WhiteKnight,
            WhiteRook,
        ]);
        squares[8..16].copy_from_slice(&[WhitePawn; 8]);
        squares[48..56].copy_from_slice(&[BlackPawn; 8]);
        squares[56..64].copy_from_slice(&[
            BlackRook,
            BlackKnight,
            BlackBishop,
            BlackQueen,
            BlackKing,
            BlackBishop,
            BlackKnight,
            BlackRook,
        ]);
        Position {
            squares,
            earlier_moves: vec![],
            //is_check: false,
            move_draw_counter: 0,
            //possible_moves: vec![],
            position_flags: PositionFlags::STARTING_POS,
        }
    }

    pub fn get_earlier_positions(&self) -> impl Iterator<Item = Position> {
        PositionIterator::new(self.earlier_moves.clone())
    }

    pub fn is_draw_or_checkmate(&self) -> (bool, bool) {
        if self.get_possible_moves().is_empty() {
            if self.is_in_check(self.is_white_turn()) {
                return (false, true);
            } else {
                return (true, false);
            }
        }
        if self.is_insufficient_material()
            || self.is_threefold_repetition()
            || self.is_50_move_rule()
        {
            return (true, false);
        }
        (false, false)
    }

    pub fn is_checkmate(&self) -> bool {
        self.is_draw_or_checkmate().1
    }

    pub fn is_draw(&self) -> bool {
        self.is_draw_or_checkmate().0
    }

    fn is_50_move_rule(&self) -> bool {
        self.move_draw_counter >= 100
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
        if self.find_pieces(WhitePawn).is_empty()
            && self.find_pieces(BlackPawn).is_empty()
            && self.find_pieces(WhiteRook).is_empty()
            && self.find_pieces(BlackRook).is_empty()
            && self.find_pieces(WhiteQueen).is_empty()
            && self.find_pieces(BlackQueen).is_empty()
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
            Position::is_in_check_by_pawn,
            Position::is_in_check_by_king,
            Position::is_in_check_by_knight,
            Position::is_in_check_by_rook_or_queen,
            Position::is_in_check_by_bishop_or_queen,
        ];

        for check_test in check_tests.iter() {
            if check_test(self, x, y, is_white) {
                return true;
            }
        }
        false
    }

    pub fn get_next_bare_boards(&self) -> Vec<(Move, [Square; 64])> {
        let mut next_boards = Vec::new();
        for possible_move in self.get_possible_moves() {
            let mut next_position = self.clone();
            next_position.make_move_bare(possible_move);
            next_boards.push((possible_move, next_position.squares));
        }
        next_boards
    }

    pub fn make_move(&mut self, m: Move) {
        // TODO unknown flags
        self.move_draw_counter += 1;

        if m.flags.contains(MoveFlags::IS_PAWN_MOVE) || m.flags.contains(MoveFlags::IS_CAPTURE) {
            self.move_draw_counter = 0;
        }

        self.earlier_moves.push(m);

        self.make_move_bare(m);

        //self.possible_moves = self.calculate_possible_moves();
        //self.is_check = self.is_in_check(self.is_white_turn());
    }

    fn make_move_bare(&mut self, m: Move) {
        // castling
        if (m.from == 4 || m.from == 60)
            && m.from / 8 == m.to / 8
            && match m.to {
                6 => self
                    .position_flags
                    .contains(PositionFlags::CAN_WHITE_CASTLE_KINGSIDE),
                62 => self
                    .position_flags
                    .contains(PositionFlags::CAN_BLACK_CASTLE_KINGSIDE),
                2 => self
                    .position_flags
                    .contains(PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE),
                58 => self
                    .position_flags
                    .contains(PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE),
                _ => false,
            }
        {
            let (rook_from, rook_to) = match m.to {
                6 => (7, 5),
                62 => (63, 61),
                2 => (0, 3),
                58 => (56, 59),
                _ => unreachable!(),
            };
            self.squares[rook_to] = self.squares[rook_from];
            self.squares[rook_from] = Empty;
        }

        if m.from == 4 {
            self.position_flags -= PositionFlags::CAN_WHITE_CASTLE_KINGSIDE;
            self.position_flags -= PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE;
        }

        if m.from == 60 {
            self.position_flags -= PositionFlags::CAN_BLACK_CASTLE_KINGSIDE;
            self.position_flags -= PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE;
        }

        if m.from == 0 || m.to == 0 {
            self.position_flags -= PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE;
        }

        if m.from == 7 || m.to == 7 {
            self.position_flags -= PositionFlags::CAN_WHITE_CASTLE_KINGSIDE;
        }

        if m.from == 56 || m.to == 56 {
            self.position_flags -= PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE;
        }

        if m.from == 63 || m.to == 63 {
            self.position_flags -= PositionFlags::CAN_BLACK_CASTLE_KINGSIDE;
        }

        self.squares[m.to as usize] = self.squares[m.from as usize];
        self.squares[m.from as usize] = Empty;

        // TODO  promotion
        /*if y1 == 6 && self.get_square(x1, y1) == Some(WhitePawn) {
            self.set_square(x2, y2, WhiteQueen)
        }
        if y1 == 1 && self.get_square(x1, y1) == Some(BlackPawn) {
            self.set_square(x2, y2, BlackQueen)
        }*/
    }

    pub fn get_possible_moves(&self) -> Vec<Move> {
        self.calculate_possible_moves()
    }

    fn calculate_possible_moves(&self) -> Vec<Move> {
        self.get_candidate_moves()
            .filter(|cand_move| {
                let mut next_position = self.clone();
                next_position.make_move(*cand_move);
                !next_position.is_in_check(self.is_white_turn())
            })
            .collect()
    }

    pub fn is_white_turn(&self) -> bool {
        self.earlier_moves.len() % 2 == 0
    }

    fn get_candidate_moves(&self) -> impl Iterator<Item = Move> + '_ {
        (0..8)
            .map(move |x| (0..8).map(move |y| self.get_candidate_moves_for_square(x, y)))
            .flatten()
            .flatten()
    }

    fn get_candidate_moves_for_square(&self, x: i32, y: i32) -> Vec<Move> {
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

    fn get_pawn_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
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
                    candidate_moves.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        flags: MoveFlags::IS_CAPTURE | MoveFlags::IS_PAWN_MOVE,
                    });
                }
            }
            // en passant
            if let (Some(piece),Some(m))  = (self.get_square(x + dx, y), self.earlier_moves.last()) {
                if piece == enemy_pawn
                && m.from as i32== 8 * (y - 2 * dy)+ x+ dx &&m.to as i32 == 8*y+x+dx
                {
                    candidate_moves.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        flags: MoveFlags::IS_CAPTURE | MoveFlags::IS_PAWN_MOVE,
                    });
                }
            }
        }
        // Moving forward
        if let Some(piece) = self.get_square(x, y + dy) {
            if piece == Empty {
                candidate_moves.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * (y + dy) + x) as u8,
                    flags: MoveFlags::IS_PAWN_MOVE,
                });
                if let Some(piece) = self.get_square(x, y + 2 * dy) {
                    if piece == Empty && y % 5 == 1 {
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + 2 * dy) + x) as u8,
                            flags: MoveFlags::IS_PAWN_MOVE,
                        });
                    }
                }
            }
        }

        candidate_moves
    }

    fn get_knight_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
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
                if piece == Empty {
                    candidate_moves.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        flags: MoveFlags::IS_CAPTURE,
                    });
                }
            }
        }

        candidate_moves
    }

    fn get_king_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
        let mut candidate_moves = Vec::new();
        // basic moves
        for dx in -1..=1 {
            for dy in -1..=1 {
                if let Some(piece) = self.get_square(x + dx, y + dy) {
                    if piece == Empty {
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x + dx) as u8,
                            flags: MoveFlags::empty(),
                        });
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x + dx) as u8,
                            flags: MoveFlags::IS_CAPTURE,
                        });
                    }
                }
            }
        }
        let y = if self.is_white_turn() { 0 } else { 7 };

        // castling kingside
        if ((self.is_white_turn()
            && self
                .position_flags
                .contains(PositionFlags::CAN_WHITE_CASTLE_KINGSIDE))
            || (!self.is_white_turn()
                && self
                    .position_flags
                    .contains(PositionFlags::CAN_BLACK_CASTLE_KINGSIDE)))
            && !self.is_in_check(self.is_white_turn())
        {
            let mut test_check_5 = self.clone();
            let mut test_check_6 = self.clone();
            test_check_5.squares[8 * y as usize + 5] = test_check_5.squares[8 * y as usize + 4];
            test_check_5.squares[8 * y as usize + 4] = Empty;
            test_check_6.squares[8 * y as usize + 6] = test_check_6.squares[8 * y as usize + 4];
            test_check_6.squares[8 * y as usize + 4] = Empty;
            if !test_check_5.is_in_check(self.is_white_turn())
                && !test_check_6.is_in_check(self.is_white_turn())
            {
                candidate_moves.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * y + x + 2) as u8,
                    flags: MoveFlags::empty(),
                });
            }
        }
        // castling queenside
        if ((self.is_white_turn()
            && self
                .position_flags
                .contains(PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE))
            || (!self.is_white_turn()
                && self
                    .position_flags
                    .contains(PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE)))
            && !self.is_in_check(self.is_white_turn())
        {
            let mut test_check_3 = self.clone();
            let mut test_check_2 = self.clone();
            test_check_3.squares[8 * y as usize + 3] = test_check_2.squares[8 * y as usize + 4];
            test_check_3.squares[8 * y as usize + 4] = Empty;
            test_check_2.squares[8 * y as usize + 2] = test_check_2.squares[8 * y as usize + 4];
            test_check_2.squares[8 * y as usize + 4] = Empty;
            if !test_check_3.is_in_check(self.is_white_turn())
                && !test_check_2.is_in_check(self.is_white_turn())
            {
                candidate_moves.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * y + x - 2) as u8,
                    flags: MoveFlags::empty(),
                });
            }
        }

        candidate_moves
    }

    fn get_queen_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
        let mut candidate_moves = self.get_rook_candidate_moves(x, y);
        candidate_moves.append(&mut self.get_bishop_candidate_moves(x, y));
        candidate_moves
    }

    fn get_rook_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
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
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * dy + dx) as u8,
                            flags: MoveFlags::empty(),
                        });
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * dy + dx) as u8,
                            flags: MoveFlags::IS_CAPTURE,
                        });
                        break;
                    } else {
                        break;
                    }
                }
            }
        }

        candidate_moves
    }

    fn get_bishop_candidate_moves(&self, x: i32, y: i32) -> Vec<Move> {
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
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * dy + dx) as u8,
                            flags: MoveFlags::empty(),
                        });
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * dy + dx) as u8,
                            flags: MoveFlags::IS_CAPTURE,
                        });
                        break;
                    } else {
                        break;
                    }
                }
            }
        }

        candidate_moves
    }

    /*pub fn set_square(&mut self, x: i32, y: i32, square: Square) {
        if 0 <= x && x <= 7 && 0 <= y && y <= 7 {
            self.squares[(8 * y + x) as usize] = square;
        }
    }*/

    pub fn get_square(&self, x: i32, y: i32) -> Option<Square> {
        if 0 <= x && x <= 7 && 0 <= y && y <= 7 {
            Some(self.squares[(8 * y + x) as usize])
        } else {
            None
        }
    }

    pub fn find_pieces(&self, piece: Square) -> Vec<(i32, i32)> {
        self.squares
            .iter()
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

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();
        for y in 0..8 {
            for x in 0..8 {
                text += &format!(
                    "{} ",
                    match self.get_square(x, y).unwrap() {
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
