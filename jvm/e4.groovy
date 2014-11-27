def range = (100..999)
[range, range].combinations().collect { it.first() * it.last() }.findAll { def s = it as String; s == s.reverse() }.max()
