main:
	corebuild -pkgs async,str Chat.byte
	corebuild -pkgs async,str Client.byte
	ocamlbuild BPlusTree.byte

test:
	corebuild -pkgs async,oUnit,str test.byte && ./test.byte

clean: 
	corebuild -clean
	ocamlbuild -clean