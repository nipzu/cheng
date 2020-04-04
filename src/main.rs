use std::io::stdin;

mod engine;
mod position;

use engine::{Engine, SearchOutput};
use position::{Move, Position};

fn main() {
    handle_uci();
}

fn handle_uci() {
    let mut position = Position::new();
    let mut engine = Engine::new();
    engine.set_depth(4);
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
                        position.make_move(Move::from_notation(notation));
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
                    best_move.expect("could not find a move (probably a checkmate or draw)")
                );
                println!("bestmove {}", best_move.unwrap());
            }
            _ => (), //panic!("unknown command {}", command),
        }
    }
}
