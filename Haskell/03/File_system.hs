
type Name = String
type Content = String

data File = (Name, Content)
data Directory = Empty |  (Name, [(Directory || File)])


fileSys :: Directory
fileSys = (("root",
                [("dir1", [("file1", "cont file1")
                          ("file2", "cont file2")])
                 ("dir2", [("dir3", [("file4", "nishto")])
                            ("file4", "nishto2")])]))

pwdHelper :: Directory -> String -> String 
pwdHelper (root, dirs) wd 
    | root == wd = root
    | otherwise = root ++ [(pwdHelper childs wd) | childs <- (map fst dirs)]
    

pwd :: Directory -> String
pwd 