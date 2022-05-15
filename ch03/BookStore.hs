-- file: ch03/BookStore.hs
type ID = Int
type Author = String
type BookName = String
data BookInfo = Book ID BookName [Author]
                deriving (Show)

bookID (Book id _ _ ) = id


data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

cusInfo = Customer 12345 "Bob" ["216 SpringField Rd"]
