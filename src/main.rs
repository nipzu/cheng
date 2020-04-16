use std::io::stdin;

mod engine;
mod position;

use engine::{Engine, SearchOutput};
use position::Move;

fn main() {
    handle_uci();
}

fn handle_uci() {
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
                println!("id name sherlock");
                println!("id author nipzu");
                println!("uciok");
            }
            "isready" => println!("readyok"),
            "ucinewgame" => engine.set_game_tree(&mut std::iter::empty()),
            "position" => {
                if command_args[1] != "startpos" {
                    panic!("no startpos after position")
                }
                if command_args.get(2) == Some(&"moves") {
                    engine.set_game_tree(
                        &mut command_args.iter().skip(3).map(|n| Move::from_notation(n)),
                    );
                }
            }
            "quit" => return,
            "debug" => {
                println!("{:?}", engine);
            }
            "go" => {
                let SearchOutput {
                    best_move,
                    search_time,
                    nodes_searched,
                    search_depth,
                    evaluation,
                } = engine.search_game_tree();
                let best_move_or_none = if let Some(m) = best_move {
                    format!("{}", m)
                } else {
                    String::from("(none)")
                };
                let nps = (nodes_searched as f64 / search_time.as_secs_f64()) as usize;
                let real_evaluation = if engine.is_root_white_turn() {
                    evaluation
                } else {
                    -evaluation
                };
                println!(
                    "info depth {} nodes {} nps {} time {} score {} pv {}",
                    search_depth,
                    nodes_searched,
                    nps,
                    search_time.as_millis(),
                    real_evaluation,
                    best_move_or_none
                );
                println!("bestmove {}", best_move_or_none);
            }
            _ => (), //panic!("unknown command {}", command),
        }
    }
}
