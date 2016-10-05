#[cfg(test)]
mod tests {
    #[test]
    fn initial_board_number_of_pins_correct() {
        let board = super::mk_initial_board();
        let num_blue_pins = 
            board.blue_even_rows.count_ones() +
            board.blue_odd_rows.count_ones();
        let num_red_pins = 
            board.red_even_rows.count_ones() +
            board.red_odd_rows.count_ones();
        assert_eq!(15, num_blue_pins);
        assert_eq!(15, num_red_pins);
    }
}

struct Board {
    blue_even_rows: u64,
    blue_odd_rows: u64,
    red_even_rows: u64,
    red_odd_rows: u64,
}

struct Pos {
    x: usize,
    y: usize,
}

fn set_blue(board: &mut Board, pos: Pos, val: bool) {
    if val {
        let i = (pos.y / 2) * 11 + pos.x;
        let b = 1 << i;
        if pos.y % 2 == 0 {
            board.blue_even_rows |= b;
        } else {
            board.blue_odd_rows |= b;
        }
    } else {
        // TODO
    }
}

fn set_red(board: &mut Board, pos: Pos, val: bool) {
    if val {
        let i = (pos.y / 2) * 11 + pos.x;
        let b = 1 << i;
        if pos.y % 2 == 0 {
            board.red_even_rows |= b;
        } else {
            board.red_odd_rows |= b;
        }
    } else {
        // TODO
    }
}

const EMPTY_BOARD: Board = Board {
    blue_even_rows: 0,
    blue_odd_rows: 0,
    red_even_rows: 0,
    red_odd_rows: 0,
};

#[derive(Debug, Copy, Clone)]
enum Field {
    BluePin,
    RedPin,
    Empty,
}

type FieldArray = [[Field; 11]; 11];

fn board_from_field_array(arr: &FieldArray) -> Board {
    let mut board = EMPTY_BOARD;
    for y in 0..11 {
        for x in 0..11 {
            match arr[y][x] {
                Field::BluePin => {
                    set_blue(&mut board, Pos { x: x, y: y }, true);
                },
                Field::RedPin => {
                    set_red(&mut board, Pos { x: x, y: y }, true);
                },
                Field::Empty => {},
            }
        }
    }
    board
}

fn mk_initial_field_array() -> FieldArray {
    let mut arr = [[Field::Empty; 11]; 11];
    
    for x in 1..6 {
        for y in 1..(7-x) {
            arr[y][x] = Field::RedPin;
        }
    }
    
    for x in 1..6 {
        for y in 1..(7-x) {
            arr[10-y][10-x] = Field::BluePin;
        }
    }
    
    arr
}

fn mk_initial_board() -> Board {
    board_from_field_array(&mk_initial_field_array())
}

// let legal_even_rows