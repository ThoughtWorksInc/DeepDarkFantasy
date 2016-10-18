#The project use label to simulate issue dependency

label "#n!" mean issue #n must be solved before solving this issue

label "#n" mean if issue #n is solved, solving this issue will be easier

label #d mean other issue depend on this issue (#n or #n!)

##To Collaborator:

when creating label "#n", add #d to issue #n

when closing issue "#n" that has label #d, delete #n and #n! from all label
