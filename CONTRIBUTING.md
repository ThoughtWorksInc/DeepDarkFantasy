#The project use label to simulate issue dependency

label "#n!" mean issue #n must be solved before solving this issue

label "#n" mean if issue #n is solved, solving this issue will be easier

label #d mean other issue depend on this issue (#n or #n!)

##To Collaborator:

when creating label "#n", add #d to issue #n

when closing issue "#n" that has label #d, delete #n and #n! from all label

#The project has a lot of unique coding standard to follow. Following them fully is too difficult, so it is not required for contributing to the code base. However, a refactoring moving toward such standard is always appreciated

1. For every function F: Repr[A => B => C], define F\_: Repr[A] => Repr[B => C], and F\_\_: Repr[A] => Repr[B] => Repr[C], and so on. Unnecessary info should be removed.
