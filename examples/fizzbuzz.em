fn fizz_buzz(n) {
	for (let i = 1; i <= n; i = i + 1;) {
		if i % 3 == 0 && i % 5 == 0 {
			print!("FizzBuzz");
		}
		else {
			if (i % 3 == 0) {
				print!("Fizz");
			} else {
				if (i % 5 == 0) {
					print!("Buzz");
				} else {
					print! i;
				}
			}
		}
	}
}

fizz_buzz(15);