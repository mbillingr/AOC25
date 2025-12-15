use std::collections::{HashMap, HashSet};

fn main() {
    use dancing_links::*;

    let mut h = Matrix::new();
    let a = h.add_exactly_once_column("A");
    let b = h.add_exactly_once_column("B");
    let c = h.add_exactly_once_column("C");
    let d = h.add_exactly_once_column("D");
    let e = h.add_exactly_once_column("E");
    let f = h.add_exactly_once_column("F");
    let g = h.add_exactly_once_column("G");

    h.add_row([c, e, f]);
    h.add_row([a, d, g]);
    h.add_row([b, c, f]);
    h.add_row([a, d]);
    h.add_row([b, g]);
    h.add_row([d, e, g]);

    println!("{:#?}", h);

    h.search(&mut |solution| {
        println!("SOLUTION: {:?}", solution);
        true
    });

    let mut lines = INPUT.lines();

    let mut shapes = vec![];
    for _ in 0..6 {
        let _ = lines.next().unwrap();
        let shape = Shape::new(vec![
            lines.next().unwrap().chars().collect(),
            lines.next().unwrap().chars().collect(),
            lines.next().unwrap().chars().collect(),
        ]);
        shapes.push(shape);
        let _ = lines.next().unwrap();
    }

    let mut regions = vec![];
    for line in lines {
        let (size, counts) = line.split_once(": ").unwrap();
        let (w, h) = size.split_once('x').unwrap();
        let width = w.parse().unwrap();
        let height = h.parse().unwrap();

        let counts = counts
            .split_whitespace()
            .map(|c| c.parse().unwrap())
            .collect();

        regions.push(Region {
            width,
            height,
            counts,
        });
    }

    println!("Day 12, part 1: {}", regions.iter().enumerate().filter(|(i, r)| {
        println!("Checking Region {}/{}", i + 1, regions.len());
        r.check(&shapes)
    } ).count());
}

#[derive(Debug)]
struct Region {
    width: usize,
    height: usize,
    counts: Vec<usize>,
}

impl Region {
    fn check(&self, shapes: &[Shape]) -> bool {
        use dancing_links::*;

        let mut h = Matrix::new();

        let mut columns = HashMap::new();

        // add one "optional" column for each position in the region
        for i in 0..self.height {
            for j in 0..self.width {
                let name = region_position_name(i, j);
                let col = h.add_atmost_once_column(name.clone());
                columns.insert(name, col);
            }
        }

        // add one "required" column for each shape instance
        for (c, &n) in self.counts.iter().enumerate() {
            for k in 0..n {
                let name = shape_instance_name(c, k);
                let col = h.add_exactly_once_column(name.clone());
                columns.insert(name, col);
            }
        }

        // add one row for each shape placement
        for (c, shp0) in shapes.iter().enumerate() {
            for shp in shp0.variations() {
                for row in 0..self.height - 2 {
                    for col in 0..self.width - 2 {
                        for k in 0..self.counts[c] {
                            let mut rcols = Vec::with_capacity(10);
                            rcols.push(columns[&shape_instance_name(c, k)]);
                            for (i, j) in shp.displace(row, col) {
                                rcols.push(columns[&region_position_name(i, j)]);
                            }
                            h.add_row(rcols);
                        }
                    }
                }
            }
        }

        let solution = &mut false;
        h.search(&mut |_| {
            *solution = true;
            true
        });

        *solution

    }
}

fn shape_instance_name(shape_nr: usize, instance_nr: usize) -> String {
    format!("S{}-{}", shape_nr, instance_nr)
}

fn region_position_name(region_nr: usize, instance_nr: usize) -> String {
    format!("R{}/{}", region_nr, instance_nr)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Shape(Vec<Vec<char>>);

impl Shape {
    fn new(data: Vec<Vec<char>>) -> Self {
        Shape(data)
    }

    fn displace(&self, r: usize, c: usize) -> impl Iterator<Item = (usize, usize)> {
        self.0
            .iter()
            .enumerate()
            .flat_map(move |(i, row)| row.iter().enumerate().map(move |(j, ch)| (i, j, ch)))
            .filter(|(_, _, ch)| **ch == '#')
            .map(move |(i, j, _)| (i + r, j + c))
    }

    fn flip(&self) -> Self {
        let mut shape = self.clone();
        shape.0.reverse();
        shape
    }

    fn rot(&self) -> Self {
        Shape(
            (0..3)
                .map(|i| (0..3).map(|j| self.0[j][2 - i]).collect())
                .collect(),
        )
    }

    fn variations(&self) -> HashSet<Self> {
        let a = self.clone();
        let b = a.rot();
        let c = b.rot();
        let d = c.rot();
        let e = a.flip();
        let f = b.flip();
        let g = c.flip();
        let h = d.flip();
        let mut out = HashSet::new();
        out.insert(a);
        out.insert(b);
        out.insert(c);
        out.insert(d);
        out.insert(e);
        out.insert(f);
        out.insert(g);
        out.insert(h);
        out
    }
}

mod dancing_links {
    type Ptr = u32;
    const UNUSED: Ptr = Ptr::MAX;
    const NOTSET: Ptr = Ptr::MAX;

    #[derive(Clone, Copy, Debug)]
    pub struct Col(Ptr);

    #[derive(Debug)]
    pub struct Node {
        lt: Ptr,
        rt: Ptr,
        up: Ptr,
        dn: Ptr,
        col: Col,
    }

    #[derive(Debug)]
    struct Column {
        size: Ptr,
        name: String,
        head: Ptr,
    }

    #[derive(Debug)]
    pub struct Matrix {
        pub nodes: Vec<Node>,
        columns: Vec<Column>,
        rows: usize,
    }

    macro_rules! node_shuffle {
        ($self:ident $d:tt $($x:ident)+ <- $($rhs:tt)+) => {{
            let tmp = node_shuffle!($self $($x)+);
            node_shuffle!($self, $d, $self.nodes[tmp as usize]) = node_shuffle!($self $($rhs)+);
        }};

        ($self:ident $d:tt $($x:ident)+) => { node_shuffle!($self, $d, $self.nodes[node_shuffle!($self $($x)+) as usize]) };

        ($self:ident -- S C $x:ident) => { $self.columns[$self.nodes[$x as usize].col.0 as usize].size -= 1; };
        ($self:ident ++ S C $x:ident) => { $self.columns[$self.nodes[$x as usize].col.0 as usize].size += 1; };

        ($self:ident $x:expr) => { $x };

        ($self:ident, L, $x:expr) => {$x.lt};
        ($self:ident, R, $x:expr) => {$x.rt};
        ($self:ident, U, $x:expr) => {$x.up};
        ($self:ident, D, $x:expr) => {$x.dn};
        ($self:ident, C, $x:expr) => {$self.columns[$x.col.0 as usize].head};
    }

    macro_rules! dance {
        ($self:ident : $(($($t:tt)+))*) => {{
            $(
                dance!(@line $self : $($t)+);
            )*
        }};

        (@line $self:ident : for $i:ident <- $d:tt $start:ident .. {$($body:tt)*}) => {{
            let mut $i = node_shuffle!($self $d $start);
            while $i != $start {
               {
                   dance!($self : $($body)*);
               }
               $i = node_shuffle!($self $d $i);
           }
        }};

        (@line $self:ident : $($t:tt)+) => {
            node_shuffle!($self $($t)+);
        };
    }

    impl Matrix {
        pub fn new() -> Self {
            let h = Node {
                lt: 0,
                rt: 0,
                up: UNUSED,
                dn: UNUSED,
                col: Col(UNUSED),
            };

            Matrix {
                nodes: vec![h],
                columns: vec![],
                rows: 0,
            }
        }

        pub fn add_atmost_once_column(&mut self, name: impl ToString) -> Col {
            let col = Col(self.columns.len() as Ptr);
            let n = self.nodes.len() as Ptr;
            self.columns.push(Column {
                name: name.to_string(),
                size: 0,
                head: n,
            });
            self.nodes.push(Node {
                lt: n,
                rt: n,
                up: n,
                dn: n,
                col,
            });
            col
        }

        pub fn add_exactly_once_column(&mut self, name: impl ToString) -> Col {
            let h = self.root();

            let col = Col(self.columns.len() as Ptr);
            let n = self.nodes.len() as Ptr;
            self.columns.push(Column {
                name: name.to_string(),
                size: 0,
                head: n,
            });
            self.nodes.push(Node {
                lt: self.lt(h),
                rt: h,
                up: n,
                dn: n,
                col,
            });
            dance! {
                self:
                (R L h <- n)
                (L h <- n)
            }

            col
        }

        pub fn add_row(&mut self, nonzero_cols: impl IntoIterator<Item = Col>) {
            self.rows += 1;
            let xs: Vec<Ptr> = nonzero_cols
                .into_iter()
                .map(|c| {
                    self.get_col_mut(c).size += 1;
                    self.add_node(Node {
                        lt: NOTSET,
                        rt: NOTSET,
                        up: NOTSET,
                        dn: NOTSET,
                        col: c,
                    })
                })
                .collect();
            let n = xs.len();
            for (i, x) in xs.iter().copied().enumerate() {
                dance! {
                    self:
                    (L x <- xs[(n + i - 1) % n])
                    (R x <- xs[(i + 1) % n])
                    (U x <- U C x)
                    (D x <- C x)
                    (U C x <- x)
                    (D U x <- x)
                }
            }
        }

        pub fn search(&mut self, solution_callback: &mut impl FnMut(Vec<Vec<&str>>) -> bool) {
            self.recursive_search(vec![], solution_callback);
        }

        fn recursive_search(
            &mut self,
            out: Vec<Ptr>,
            solution_callback: &mut impl FnMut(Vec<Vec<&str>>) -> bool,
        ) -> bool {
            let h = self.root();
            if self.rt(h) == h {
                let tmp = out
                    .iter()
                    .map(|r| {
                        std::iter::once(*r)
                            .chain(self.iter_rt(*r))
                            .map(|x| self.column(x).name.as_str())
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>();
                return solution_callback(tmp);
            }

            let c = self.select_column();
            self.cover_column(c);

            let done = self.loop_dn(self.head(c), |this, r| {
                this.loop_rt(r, |this, j| this.cover_column_of(j));
                let mut out_ = out.clone();
                out_.push(r);
                let done = this.recursive_search(out_, solution_callback);
                this.loop_lt(r, |this, j| this.uncover_column_of(j));
                done
            });

            self.uncover_column(c);

            done
        }

        fn select_column(&mut self) -> Col {
            let head = self
                .iter_rt(self.root())
                .min_by_key(|&c| self.column(c).size)
                .unwrap();
            self.col(head)
        }

        fn cover_column_of(&mut self, j: Ptr) -> bool {
            let c = self.col(j);
            self.cover_column(c)
        }

        fn uncover_column_of(&mut self, j: Ptr) -> bool {
            let c = self.col(j);
            self.uncover_column(c)
        }

        fn cover_column(&mut self, c: Col) -> bool {
            let c = self.head(c);
            dance! {
                self:
                (L R c <- L c)
                (R L c <- R c)
                (for i <- D c .. {
                    (for j <- R i .. {
                        (U D j <- U j)
                        (D U j <- D j)
                        (-- S C j)
                    })
                })
            }
            false
        }

        fn uncover_column(&mut self, c: Col) -> bool {
            let c = self.head(c);
            dance! {
                self:
                (for i <- U c .. {
                    (for j <- L i .. {
                        (++ S C j)
                        (U D j <- j)
                        (D U j <- j)
                    })
                })
                (L R c <- c)
                (R L c <- c)
            }
            false
        }

        fn add_node(&mut self, node: Node) -> Ptr {
            let n = self.nodes.len() as Ptr;
            self.nodes.push(node);
            n
        }

        #[inline(always)]
        fn root(&self) -> Ptr {
            0
        }

        #[inline(always)]
        fn lt(&self, n: Ptr) -> Ptr {
            self.nodes[n as usize].lt
        }

        #[inline(always)]
        fn rt(&self, n: Ptr) -> Ptr {
            self.nodes[n as usize].rt
        }

        #[inline(always)]
        fn dn(&self, n: Ptr) -> Ptr {
            self.nodes[n as usize].dn
        }

        #[inline(always)]
        fn head(&self, c: Col) -> Ptr {
            self.columns[c.0 as usize].head
        }

        #[inline(always)]
        fn col(&self, n: Ptr) -> Col {
            self.nodes[n as usize].col
        }

        #[inline(always)]
        fn column(&self, n: Ptr) -> &Column {
            self.get_col(self.col(n))
        }

        #[inline(always)]
        fn get_col(&self, c: Col) -> &Column {
            &self.columns[c.0 as usize]
        }

        #[inline(always)]
        fn get_col_mut(&mut self, c: Col) -> &mut Column {
            &mut self.columns[c.0 as usize]
        }

        fn loop_dn(&mut self, start: Ptr, f: impl FnMut(&mut Self, Ptr) -> bool) -> bool {
            self.loop_over::<StepDown>(start, f)
        }

        fn loop_rt(&mut self, start: Ptr, f: impl FnMut(&mut Self, Ptr) -> bool) -> bool {
            self.loop_over::<StepRight>(start, f)
        }

        fn loop_lt(&mut self, start: Ptr, f: impl FnMut(&mut Self, Ptr) -> bool) -> bool {
            self.loop_over::<StepLeft>(start, f)
        }

        fn loop_over<S: ListStepper>(
            &mut self,
            start: Ptr,
            mut f: impl FnMut(&mut Self, Ptr) -> bool,
        ) -> bool {
            let mut n = S::step(self, start);
            while n != start {
                if f(self, n) {
                    return true;
                }
                n = S::step(self, n);
            }
            false
        }

        fn iter_rt(&self, start: Ptr) -> MatrixIter<'_, StepRight> {
            MatrixIter {
                matrix: self,
                start,
                current: self.rt(start),
                _marker: std::marker::PhantomData,
            }
        }
    }

    struct MatrixIter<'a, F> {
        matrix: &'a Matrix,
        start: Ptr,
        current: Ptr,
        _marker: std::marker::PhantomData<F>,
    }

    impl<'a, F: ListStepper> Iterator for MatrixIter<'a, F> {
        type Item = Ptr;

        fn next(&mut self) -> Option<Self::Item> {
            if self.current == self.start {
                return None;
            }
            let n = self.current;
            self.current = F::step(self.matrix, n);
            Some(n)
        }
    }

    trait ListStepper {
        fn step(matrix: &Matrix, n: Ptr) -> Ptr;
    }

    struct StepRight;

    impl ListStepper for StepRight {
        fn step(matrix: &Matrix, n: Ptr) -> Ptr {
            matrix.rt(n)
        }
    }

    struct StepLeft;

    impl ListStepper for StepLeft {
        fn step(matrix: &Matrix, n: Ptr) -> Ptr {
            matrix.lt(n)
        }
    }

    struct StepDown;

    impl ListStepper for StepDown {
        fn step(matrix: &Matrix, n: Ptr) -> Ptr {
            matrix.dn(n)
        }
    }
}

const INPUT: &str = "COPY INPUT HERE";