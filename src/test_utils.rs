use core::fmt::Debug;

pub fn assert_vec_eq<T: Eq + Debug>(vec1: &Vec<T>, vec2: &Vec<T>) {
    assert!(vec1.len() == vec2.len());

    for i in 0..vec1.len() {
        assert_eq!(vec1[i], vec2[i]);
    }
}
