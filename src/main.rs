use std::io::stdin;

use rand::{thread_rng, Rng};

mod board;
use board::*;

fn main() {
    handle_uci();
}

fn handle_uci() {
    let mut rng = thread_rng();
    let mut board = Board::new();
    loop {
        let mut command = String::new();
        stdin().read_line(&mut command).unwrap();
        while command.ends_with('\n') || command.ends_with('\r') {
            command.pop();
        }

        let command_args: Vec<&str> = command.split_whitespace().collect();

        match command_args[0] {
            "uci" => {
                println!("id name cheng");
                println!("id author nipzu");
                println!("uciok");
            }
            "isready" => println!("readyok"),
            "ucinewgame" => board = Board::new(),
            "position" => {
                if command_args[1] != "startpos" {
                    panic!("no startpos after position")
                }
                board = Board::new();
                if command_args.get(2) == Some(&"moves") {
                    for notation in command_args.iter().skip(3) {
                        board.make_move(notation_to_move(notation));
                    }
                }
            }
            "quit" => return,
            "debug" => {
                println!("{}", board);
                println!("{:?}", board.get_possible_moves());
            }
            "go" => {
                let moves = board.get_possible_moves();
                let best_move = move_to_notation(moves[rng.gen_range(0, moves.len())]);
                println!("info depth 1");
                println!("bestmove {}", best_move);
            }
            _ => (), //panic!("unknown command {}", command),
        }
    }
}

fn move_to_notation(move_coords: (i32, i32, i32, i32)) -> String {
    let (x1, y1, x2, y2) = move_coords;
    let files = ["a", "b", "c", "d", "e", "f", "g", "h"];
    format!(
        "{}{}{}{}",
        files[x1 as usize],
        y1 + 1,
        files[x2 as usize],
        y2 + 1
    )
}

fn notation_to_move(notation: &str) -> (i32, i32, i32, i32) {
    let mut bytes = notation.bytes();
    (
        bytes.next().unwrap() as i32 - 97,
        bytes.next().unwrap() as i32 - 49,
        bytes.next().unwrap() as i32 - 97,
        bytes.next().unwrap() as i32 - 49,
    )
}
