use async_std::io::stdin;
use async_std::task;
use futures::future::FutureExt;
use futures::{pin_mut, select};

mod engine;
mod position;

use engine::{search_game_tree, Evaluation, SearchOutput};
use position::{Move, Position};

fn main() {
    task::block_on(run_engine());
}

async fn read_line() -> Result<String, std::io::Error> {
    let mut line = String::new();
    stdin().read_line(&mut line).await?;
    Ok(line)
}

async fn run_engine() {
    let depth = 6;
    let mut game_tree = vec![Position::new()];
    let mut search_handle = task::spawn(dummy_search()).fuse();
    let mut is_searching = false;
    let mut command;

    loop {
        let a = read_line().fuse();
        pin_mut!(a);

        if is_searching {
            select! {
                line = a => command = line.unwrap(),
                search_output = search_handle => {
                    println!("{}", search_output);
                    let best_move_or_none = if let Some(m) = search_output.best_move {
                        format!("{}", m)
                    }else {
                        String::from("(none)")
                    };
                    println!("bestmove {}", best_move_or_none);
                    is_searching = false;
                    command = a.await.unwrap();
                },
            }
        } else {
            command = read_line().await.unwrap();
        }

        while command.ends_with('\n') || command.ends_with('\r') {
            command.pop();
        }
        let command_args: Vec<&str> = command.split_whitespace().collect();

        match *command_args.get(0).unwrap_or(&"") {
            "uci" => {
                println!("id name sherlock");
                println!("id author nipzu");
                println!("uciok");
            }
            "isready" => println!("readyok"),
            "ucinewgame" => game_tree = vec![Position::new()],
            "position" => {
                if command_args.get(1) != Some(&"startpos") {
                    panic!("no startpos after position")
                }
                if command_args.get(2) == Some(&"moves") {
                    game_tree = vec![Position::new()];
                    for m in command_args.iter().skip(3) {
                        let mut next_position = game_tree.last().unwrap().clone();
                        next_position.make_move(Move::from_notation(m));
                        game_tree.push(next_position);
                    }
                }
            }
            "quit" => return,
            "debug" => {
                unimplemented!();
            }
            "stop" => {
                // TODO is_searching = false;
                unimplemented!();
            }
            "go" => {
                let game_tree_cloned = game_tree.clone();
                search_handle = task::spawn(search_game_tree(game_tree_cloned, depth)).fuse();
                is_searching = true;
            }
            _ => (),
        }
    }
}

async fn dummy_search() -> SearchOutput {
    SearchOutput {
        best_move: None,
        evaluation: Evaluation::MateIn(0),
        nodes_searched: 0,
        search_time: std::time::Duration::from_secs(0),
        search_depth: 0,
    }
}
