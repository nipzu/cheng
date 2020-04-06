use std::fmt;

use bitflags::bitflags;

#[derive(Clone)]
pub struct Position {
    squares: [Square; 64],
    position_flags: PositionFlags,
}

impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {
        self.squares
            .iter()
            .zip(other.squares.iter())
            .all(|(a, b)| a == b)
            && self.position_flags & PositionFlags::COMPARE_FLAGS
                == other.position_flags & PositionFlags::COMPARE_FLAGS
    }
}

bitflags! {
    struct PositionFlags: u16 {
        const CAN_EN_PASSANT             = 0b0000_0000_0000_1000;
        const EN_PASSANT_FILE_MASK       = 0b0000_0000_0000_0111;
        const CAN_WHITE_CASTLE_KINGSIDE  = 0b0000_0000_0001_0000;
        const CAN_BLACK_CASTLE_KINGSIDE  = 0b0000_0000_0010_0000;
        const CAN_WHITE_CASTLE_QUEENSIDE = 0b0000_0000_0100_0000;
        const CAN_BLACK_CASTLE_QUEENSIDE = 0b0000_0000_1000_0000;
        const IS_WHITE_TURN              = 0b0000_0001_0000_0000;
        const RESETS_DRAW_COUNTERS       = 0b0000_0010_0000_0000;
        const IS_CHECK                   = 0b0000_0100_0000_0000;
        const NOT_EN_PASSANT_BITS        = 0b1111_1111_1111_0000;
        const COMPARE_FLAGS              = 0b0000_0101_1111_0000;
        const STARTING_POS               = 0b0000_0011_1111_0000;
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
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

bitflags! {
    pub struct MoveFlags: u8 {
        const IS_PAWN_MOVE         = 0b0000_0001;
        const IS_CAPTURE           = 0b0000_0010;
        const FLAGS_UNKNOWN        = 0b0000_0100;
        const PROMOTE_TO_QUEEN     = 0b0001_0000;
        const PROMOTE_TO_ROOK      = 0b0010_0000;
        const PROMOTE_TO_BISHOP    = 0b0100_0000;
        const PROMOTE_TO_KNIGHT    = 0b1000_0000;
        const PROMOTION_PIECE_MASK = 0b1111_0000;
    }
}

#[derive(Clone, Copy)]
pub struct Move {
    from: u8,
    to: u8,
    move_flags: MoveFlags,
}

impl Move {
    pub fn from_notation(text: &str) -> Move {
        let mut bytes = text.bytes();
        let from_x = bytes.next().unwrap() - 97;
        let from_y = bytes.next().unwrap() - 49;
        let to_x = bytes.next().unwrap() - 97;
        let to_y = bytes.next().unwrap() - 49;

        let mut move_flags = MoveFlags::FLAGS_UNKNOWN;
        if let Some(b) = bytes.next() {
            match b {
                b'q' => move_flags = MoveFlags::PROMOTE_TO_QUEEN,
                b'r' => move_flags = MoveFlags::PROMOTE_TO_ROOK,
                b'b' => move_flags = MoveFlags::PROMOTE_TO_BISHOP,
                b'n' => move_flags = MoveFlags::PROMOTE_TO_KNIGHT,
                _ => panic!("invalid promotion"),
            };
            move_flags |= MoveFlags::IS_PAWN_MOVE;
            move_flags |= if from_y != to_y {
                MoveFlags::IS_CAPTURE
            } else {
                MoveFlags::empty()
            }
        }

        Move {
            from: 8 * from_y + from_x,
            to: 8 * to_y + to_x,
            move_flags,
        }
    }

    pub fn resets_draw_counters(self) -> bool {
        self.move_flags.contains(MoveFlags::IS_PAWN_MOVE)
            || self.move_flags.contains(MoveFlags::IS_CAPTURE)
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const FILES: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        let from_file = FILES[(self.from % 8) as usize];
        let from_rank = self.from / 8 + 1;
        let to_file = FILES[(self.to % 8) as usize];
        let to_rank = self.to / 8 + 1;
        let promote_to = match self.move_flags & MoveFlags::PROMOTION_PIECE_MASK {
            MoveFlags::PROMOTE_TO_QUEEN => "q",
            MoveFlags::PROMOTE_TO_ROOK => "r",
            MoveFlags::PROMOTE_TO_BISHOP => "b",
            MoveFlags::PROMOTE_TO_KNIGHT => "n",
            _ => "",
        };

        write!(
            f,
            "{}{}{}{}{}",
            from_file, from_rank, to_file, to_rank, promote_to
        )
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
            position_flags: PositionFlags::STARTING_POS,
        }
    }

    pub fn get_squares(&self) -> &[Square; 64] {
        &self.squares
    }

    pub fn is_insufficient_material(&self) -> bool {
        let mut is_knight = false;
        let mut bishops = [(Empty, false), (Empty, false)];

        for (i, piece) in self.squares.iter().enumerate() {
            match piece {
                WhitePawn | BlackPawn | WhiteRook | BlackRook | WhiteQueen | BlackQueen => {
                    return false
                }
                Empty | WhiteKing | BlackKing => (),
                WhiteKnight | BlackKnight => {
                    if is_knight {
                        return false;
                    } else {
                        is_knight = true;
                    }
                }
                WhiteBishop | BlackBishop => {
                    if bishops[0].0 == Empty {
                        bishops[0] = (*piece, ((i / 8) + (i % 8)) % 2 == 0);
                    } else if bishops[1].0 == Empty {
                        bishops[1] = (*piece, ((i / 8) + (i % 8)) % 2 == 0);
                    } else {
                        return false;
                    }
                }
            }
        }

        bishops[0].0 != Empty && bishops[1].0 != Empty && bishops[0].1 != bishops[1].1
    }

    pub fn can_en_passant(&self) -> bool {
        if self.position_flags.contains(PositionFlags::CAN_EN_PASSANT) {
            let x = (self.position_flags & PositionFlags::EN_PASSANT_FILE_MASK).bits as i32;
            let (y, own_pawn) = if self.is_white_turn() {
                (4, WhitePawn)
            } else {
                (3, BlackPawn)
            };
            for dx in [-1, 1].iter() {
                if self.get_square(x + dx, y) == Some(own_pawn) {
                    return true;
                }
            }
        }
        false
    }

    pub fn is_check(&self) -> bool {
        self.is_in_check(self.is_white_turn())
    }

    fn find_king(&self, is_white: bool) -> u8 {
        let own_king = if is_white { WhiteKing } else { BlackKing };
        for (i, square) in self.squares.iter().enumerate() {
            if *square == own_king {
                return i as u8;
            }
        }
        unreachable!();
    }

    fn is_in_check(&self, is_white: bool) -> bool {
        let own_king_pos = self.find_king(is_white);
        let (x, y) = (own_king_pos as i32 % 8, own_king_pos as i32 / 8);

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

    fn get_unknown_flags(&self, m: &mut Move) {
        m.move_flags = MoveFlags::empty();

        // promotion is handled in from_notation
        // ignore en passant for now
        if self.squares[m.to as usize] != Empty {
            m.move_flags |= MoveFlags::IS_CAPTURE;
        }

        if self.squares[m.from as usize] == WhitePawn || self.squares[m.from as usize] == BlackPawn
        {
            m.move_flags |= MoveFlags::IS_PAWN_MOVE;
        }
    }

    pub fn make_move(&mut self, mut m: Move) -> bool{
        assert_ne!(m.from, m.to);

        if m.move_flags.contains(MoveFlags::FLAGS_UNKNOWN) {
            self.get_unknown_flags(&mut m);
        }

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
                    .contains(PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE),
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
            self.position_flags
                .remove(PositionFlags::CAN_WHITE_CASTLE_KINGSIDE);
            self.position_flags
                .remove(PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE);
        }

        if m.from == 60 {
            self.position_flags
                .remove(PositionFlags::CAN_BLACK_CASTLE_KINGSIDE);
            self.position_flags
                .remove(PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE);
        }

        if m.from == 0 || m.to == 0 {
            self.position_flags
                .remove(PositionFlags::CAN_WHITE_CASTLE_QUEENSIDE);
        }

        if m.from == 7 || m.to == 7 {
            self.position_flags
                .remove(PositionFlags::CAN_WHITE_CASTLE_KINGSIDE);
        }

        if m.from == 56 || m.to == 56 {
            self.position_flags
                .remove(PositionFlags::CAN_BLACK_CASTLE_QUEENSIDE);
        }

        if m.from == 63 || m.to == 63 {
            self.position_flags
                .remove(PositionFlags::CAN_BLACK_CASTLE_KINGSIDE);
        }

        assert!(
            !(self.squares[m.to as usize] == WhiteKing || self.squares[m.to as usize] == BlackKing)
        );

        // check en passant square
        if m.move_flags.contains(MoveFlags::IS_PAWN_MOVE)
            && !m.move_flags.contains(MoveFlags::IS_CAPTURE)
            && m.from % 8 != m.to % 8
        {
            self.squares[(m.to as i32 + if self.is_white_turn() { -8 } else { 8 }) as usize] =
                Empty;
        }

        // here we actually make the move
        self.squares[m.to as usize] = self.squares[m.from as usize];
        self.squares[m.from as usize] = Empty;
        // clear en passant bits
        self.position_flags &= PositionFlags::NOT_EN_PASSANT_BITS;

        if (self.squares[m.to as usize] == WhitePawn || self.squares[m.to as usize] == BlackPawn)
            && ((m.from as i32 / 8) - (m.to as i32 / 8)).abs() == 2
        {
            self.position_flags |= PositionFlags::CAN_EN_PASSANT;
            self.position_flags.bits |= (m.from % 8) as u16;
        }

        if m.move_flags.contains(MoveFlags::PROMOTE_TO_QUEEN) {
            self.squares[m.to as usize] = if self.is_white_turn() {
                WhiteQueen
            } else {
                BlackQueen
            };
        }

        if m.move_flags.contains(MoveFlags::PROMOTE_TO_ROOK) {
            self.squares[m.to as usize] = if self.is_white_turn() {
                WhiteRook
            } else {
                BlackRook
            };
        }
        if m.move_flags.contains(MoveFlags::PROMOTE_TO_BISHOP) {
            self.squares[m.to as usize] = if self.is_white_turn() {
                WhiteBishop
            } else {
                BlackBishop
            };
        }
        if m.move_flags.contains(MoveFlags::PROMOTE_TO_KNIGHT) {
            self.squares[m.to as usize] = if self.is_white_turn() {
                WhiteKnight
            } else {
                BlackKnight
            };
        }

        self.position_flags.set(
            PositionFlags::RESETS_DRAW_COUNTERS,
            m.resets_draw_counters(),
        );
        self.position_flags.toggle(PositionFlags::IS_WHITE_TURN);
        
        !self.is_in_check(!self.is_white_turn())
    }

    pub fn resets_draw_counters(&self) -> bool {
        self.position_flags
            .contains(PositionFlags::RESETS_DRAW_COUNTERS)
    }

    pub fn is_white_turn(&self) -> bool {
        self.position_flags.contains(PositionFlags::IS_WHITE_TURN)
    }

    pub fn get_candidate_moves(&self, candidate_moves_buffer: &mut Vec<Move>) {
        for x in 0..8 {
            for y in 0..8 {
                self.get_candidate_moves_for_square(x, y, candidate_moves_buffer);
            }
        }
    }

    fn get_candidate_moves_for_square(
        &self,
        x: i32,
        y: i32,
        candidate_moves_buffer: &mut Vec<Move>,
    ) {
        if let Some(piece) = self.get_square(x, y) {
            if piece != Empty && piece.is_white() == self.is_white_turn() {
                match piece {
                    WhiteKing | BlackKing => {
                        self.get_king_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    WhiteQueen | BlackQueen => {
                        self.get_queen_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    WhiteRook | BlackRook => {
                        self.get_rook_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    WhiteBishop | BlackBishop => {
                        self.get_bishop_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    WhiteKnight | BlackKnight => {
                        self.get_knight_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    WhitePawn | BlackPawn => {
                        self.get_pawn_candidate_moves(x, y, candidate_moves_buffer)
                    }
                    _ => unreachable!(),
                };
            }
        }
    }

    fn get_pawn_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
        let dy = if self.is_white_turn() { 1 } else { -1 };
        for dx in [1, -1].iter() {
            // basic capturing
            if let Some(piece) = self.get_square(x + dx, y + dy) {
                if piece != Empty && piece.is_white() != self.is_white_turn() {
                    if (y + dy) % 7 == 0 {
                        for p in [
                            MoveFlags::PROMOTE_TO_QUEEN,
                            MoveFlags::PROMOTE_TO_ROOK,
                            MoveFlags::PROMOTE_TO_BISHOP,
                            MoveFlags::PROMOTE_TO_KNIGHT,
                        ]
                        .iter()
                        {
                            candidate_moves_buffer.push(Move {
                                from: (8 * y + x) as u8,
                                to: (8 * (y + dy) + x + dx) as u8,
                                move_flags: MoveFlags::IS_PAWN_MOVE | MoveFlags::IS_CAPTURE | *p,
                            });
                        }
                    } else {
                        candidate_moves_buffer.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x + dx) as u8,
                            move_flags: MoveFlags::IS_PAWN_MOVE | MoveFlags::IS_CAPTURE,
                        });
                    }
                }
            }

            // en passant
            if self.position_flags.contains(PositionFlags::CAN_EN_PASSANT)
                && y == if self.is_white_turn() { 4 } else { 3 }
                && (PositionFlags::EN_PASSANT_FILE_MASK & self.position_flags).bits as i32 == x + dx
            {
                candidate_moves_buffer.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * (y + dy) + x + dx) as u8,
                    // do not set IS_CAPTURE
                    move_flags: MoveFlags::IS_PAWN_MOVE,
                });
            }
        }
        // moving forward
        if let Some(piece1) = self.get_square(x, y + dy) {
            if piece1 == Empty {
                if (y + dy) % 7 == 0 {
                    for p in [
                        MoveFlags::PROMOTE_TO_QUEEN,
                        MoveFlags::PROMOTE_TO_ROOK,
                        MoveFlags::PROMOTE_TO_BISHOP,
                        MoveFlags::PROMOTE_TO_KNIGHT,
                    ]
                    .iter()
                    {
                        candidate_moves_buffer.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x) as u8,
                            move_flags: MoveFlags::IS_PAWN_MOVE | *p,
                        });
                    }
                } else {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x) as u8,
                        move_flags: MoveFlags::IS_PAWN_MOVE,
                    });
                }
                if let Some(piece2) = self.get_square(x, y + 2 * dy) {
                    if piece2 == Empty && y % 5 == 1 {
                        candidate_moves_buffer.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + 2 * dy) + x) as u8,
                            move_flags: MoveFlags::IS_PAWN_MOVE,
                        });
                    }
                }
            }
        }
    }

    fn get_knight_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
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
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * (y + dy) + x + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                }
            }
        }
    }

    fn get_king_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
        // basic moves
        for dx in -1..=1 {
            for dy in -1..=1 {
                if let Some(piece) = self.get_square(x + dx, y + dy) {
                    if piece == Empty {
                        candidate_moves_buffer.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x + dx) as u8,
                            move_flags: MoveFlags::empty(),
                        });
                    } else if piece.is_white() != self.is_white_turn() {
                        candidate_moves_buffer.push(Move {
                            from: (8 * y + x) as u8,
                            to: (8 * (y + dy) + x + dx) as u8,
                            move_flags: MoveFlags::IS_CAPTURE,
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
            && !self.is_check()
            && self.squares[8 * y as usize + 5] == Empty
            && self.squares[8 * y as usize + 6] == Empty
        {
            let mut test_check_5 = self.clone();
            test_check_5.squares[8 * y as usize + 5] = test_check_5.squares[8 * y as usize + 4];
            test_check_5.squares[8 * y as usize + 4] = Empty;
            test_check_5
                .position_flags
                .toggle(PositionFlags::IS_WHITE_TURN);

            if !test_check_5.is_check() {
                candidate_moves_buffer.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * y + x + 2) as u8,
                    move_flags: MoveFlags::empty(),
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
            && !self.is_check()
            && self.squares[8 * y as usize + 1] == Empty
            && self.squares[8 * y as usize + 2] == Empty
            && self.squares[8 * y as usize + 3] == Empty
        {
            let mut test_check_3 = self.clone();
            let mut test_check_2 = self.clone();
            test_check_3.squares[8 * y as usize + 3] = test_check_2.squares[8 * y as usize + 4];
            test_check_3.squares[8 * y as usize + 4] = Empty;
            test_check_3
                .position_flags
                .toggle(PositionFlags::IS_WHITE_TURN);
            test_check_2.squares[8 * y as usize + 2] = test_check_2.squares[8 * y as usize + 4];
            test_check_2.squares[8 * y as usize + 4] = Empty;
            test_check_2
                .position_flags
                .toggle(PositionFlags::IS_WHITE_TURN);
            if !test_check_3.is_check() && !test_check_2.is_check() {
                candidate_moves_buffer.push(Move {
                    from: (8 * y + x) as u8,
                    to: (8 * y + x - 2) as u8,
                    move_flags: MoveFlags::empty(),
                });
            }
        }
    }

    fn get_queen_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
        self.get_rook_candidate_moves(x, y, candidate_moves_buffer);
        self.get_bishop_candidate_moves(x, y, candidate_moves_buffer);
    }

    fn get_rook_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
        use std::iter::repeat;

        for (dx, dy) in (x + 1..8).zip(repeat(y)) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (repeat(x)).zip(y + 1..8) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (0..x).rev().zip(repeat(y)) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (repeat(x)).zip((0..y).rev()) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
    }

    fn get_bishop_candidate_moves(&self, x: i32, y: i32, candidate_moves_buffer: &mut Vec<Move>) {
        for (dx, dy) in (x + 1..8).zip(y + 1..8) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (x + 1..8).zip((0..y).rev()) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (0..x).rev().zip(y + 1..8) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
        for (dx, dy) in (0..x).rev().zip((0..y).rev()) {
            if let Some(piece) = self.get_square(dx, dy) {
                if piece == Empty {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::empty(),
                    });
                } else if piece.is_white() != self.is_white_turn() {
                    candidate_moves_buffer.push(Move {
                        from: (8 * y + x) as u8,
                        to: (8 * dy + dx) as u8,
                        move_flags: MoveFlags::IS_CAPTURE,
                    });
                    break;
                } else {
                    break;
                }
            }
        }
    }

    pub fn get_square(&self, x: i32, y: i32) -> Option<Square> {
        if 0 <= x && x <= 7 && 0 <= y && y <= 7 {
            Some(self.squares[(8 * y + x) as usize])
        } else {
            None
        }
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

        for (dx, dy) in (x + 1..8).zip(repeat(y)) {
            if self.get_square(dx, dy) == Some(enemy_rook)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (repeat(x)).zip(y + 1..8) {
            if self.get_square(dx, dy) == Some(enemy_rook)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (0..x).rev().zip(repeat(y)) {
            if self.get_square(dx, dy) == Some(enemy_rook)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (repeat(x)).zip((0..y).rev()) {
            if self.get_square(dx, dy) == Some(enemy_rook)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
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

        for (dx, dy) in (x + 1..8).zip(y + 1..8) {
            if self.get_square(dx, dy) == Some(enemy_bishop)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (x + 1..8).zip((0..y).rev()) {
            if self.get_square(dx, dy) == Some(enemy_bishop)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (0..x).rev().zip(y + 1..8) {
            if self.get_square(dx, dy) == Some(enemy_bishop)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        for (dx, dy) in (0..x).rev().zip((0..y).rev()) {
            if self.get_square(dx, dy) == Some(enemy_bishop)
                || self.get_square(dx, dy) == Some(enemy_queen)
            {
                return true;
            } else if self.get_square(dx, dy) != Some(Empty) {
                break;
            }
        }
        false
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self)?;
        writeln!(f, "{:#b}", self.position_flags.bits)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();
        for y in (0..8).rev() {
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
