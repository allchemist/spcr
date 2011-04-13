(in-package :spcr)

(defparameter *groups-table*
  '((1 . "[OH]")
    (2 . "[#6][OH]")
    (3 . "[#6][CH2][OH]")
    (4 . "c[CH2][OH]")
    (5 . "C[CH](C)[OH]")
    (6 . "C[CH](C)[OH]")
    (7 . "[CH3][CH]([CH2])[OH]")
    (8 . "[CH2][CH]([CH2])[OH]")
    (9 . "[#6]C([#6])[CH]([#6])([#6])[OH]")
    (10 . "[C;r6][OH]")
    (11 . "[#6]C([#6])([#6])[OH]")
    (12 . "C([OH])C([OH])")
    (13 . "[c;r6][OH]")
    (14 . "[Nh]")
    (15 . "[NH2]")
    (16 . "C[NH2]")
    (17 . "[CH2][NH2]")
    (18 . "[CH](C)[NH2]")
    (19 . "[c][NH2]")
    (20 . "[NH]")
    (21 . "C[NH](C)")
    (22 . "C[NH][CH3]")
    (23 . "[#6]N")
    (24 . "[#6]N([#6])[#6]")
    (25 . "CN(C)C")
    (26 . "CN([CH3])C")
    (27 . "[Sh]")
    (28 . "[#6][SH]")
    (29 . "[C,N]#")
    (30 . "C#C")
    (31 . "[#6]C#[CH]")
    (32 . "C#N")
    (33 . "CC#N")
    (34 . "cC#N")
    (35 . "*(=C)(=*)")
    (36 . "N(=C)(=O)")))

(defparameter *OH-groups-tree*
  '(1
    (2 ((3 (4))
	(5 ((6 (7)) 8 9 10))
	11
	12))
    13))

(defparameter *NH-groups-tree*
  '(14
    ((15
      (16 (17 18))
      19)
     (20 (21 22)))))

(defparameter *CN-groups-tree*
  '(23
    (24
     (25 (26)))))

(defparameter *SH-groups-tree*
  '(27 (28)))

(defparameter *C/Nsp-groups-tree*
  '(29
    (30 (31))
    (32 (33 34))
    (35 (36))))

(defparameter *groups-tree*
  (list *OH-groups-tree*
	*NH-groups-tree*
	*CN-groups-tree*
	*SH-groups-tree*
	*C/Nsp-groups-tree*))

(defun tree-smarts-idx (idx)
  (cdr (assoc idx *groups-table*)))
