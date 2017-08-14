module Chapter14_2

-- exercise 1
data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect

PwdCheckAccess : PwdCheck -> Access
PwdCheckAccess Correct = LoggedIn
PwdCheckAccess Incorrect = LoggedOut

data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
  Password : String -> ShellCmd PwdCheck LoggedOut PwdCheckAccess
  Logout : ShellCmd () LoggedIn (const LoggedOut)
  GetSecret : ShellCmd String LoggedIn (const LoggedIn)

  PutStr : String -> ShellCmd () state (const state)
  Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
  (>>=) : ShellCmd a state1 state2_fn ->
          ((res : a) -> ShellCmd b (state2_fn res) state3_fn) ->
          ShellCmd b state1 state3_fn

session : ShellCmd () LoggedOut (const LoggedOut)
session = do Correct <- Password "wurzel"
               | Incorrect => PutStr "Wrong password"
             msg <- GetSecret
             PutStr ("Secret code: " ++ show msg ++ "\n")
             Logout

-- sessionBad : ShellCmd () LoggedOut (const LoggedOut)
-- sessionBad = do Password "wurzel"
--                 msg <- GetSecret
--                 PutStr ("Secret code: " ++ show msg ++ "\n")
--                 Logout

-- noLogout : ShellCmd () LoggedOut (const LoggedOut)
-- noLogout = do Correct <- Password "wurzel"
--                 | Incorrect => PutStr "Wrong password"
--               msg <- GetSecret
--               PutStr ("Secret code: " ++ show msg ++ "\n")
