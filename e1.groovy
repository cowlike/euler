println ( (1..999).inject(0) {t,v -> t += ((v % 3 == 0 || v % 5 == 0)? v: 0)} )
