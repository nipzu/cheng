use std::io::stdin;

mod engine;
mod position;

use engine::{Engine, SearchOutput};
use position::Position;

fn main() {
    handle_uci();
}

fn handle_uci() {
    let mut position = Position::new();
    let mut engine = Engine::new();
    engine.set_depth(3);
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
            "ucinewgame" => position = Position::new(),
            "position" => {
                if command_args[1] != "startpos" {
                    panic!("no startpos after position")
                }
                position = Position::new();
                if command_args.get(2) == Some(&"moves") {
                    for notation in command_args.iter().skip(3) {
                        position.make_move(notation_to_move(notation));
                    }
                }
            }
            "quit" => return,
            "debug" => {
                println!("{}", position);
                println!("draw? {}", position.is_draw());
                println!("check? {}", position.is_check());
                println!("checkmate? {}", position.is_checkmate());
                println!("{:?}", position.get_possible_moves());
            }
            "go" => {
                engine.set_root_node(&position);
                let SearchOutput {
                    best_move,
                    search_time,
                    nodes_searched,
                    search_depth,
                } = engine.find_best_move();
                let nps = (nodes_searched as f64 / search_time.as_secs_f64()) as usize;
                println!(
                    "info depth {} nodes {} nps {} time {} pv {}",
                    search_depth,
                    nodes_searched,
                    nps,
                    search_time.as_millis(),
                    move_to_notation(best_move)
                );
                println!("bestmove {}", move_to_notation(best_move));
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
