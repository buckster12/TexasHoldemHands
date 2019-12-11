# TexasHoldemHands

To run the program, type a command in your terminal (you should have java installed on your computer):

    java -jar out/artifacts/TexasHoldemHands_jar/TexasHoldemHands.jar

Write lines with cards data and press Ctrl+C to process them

The program gets a input lines with a board and hands cards, process them one by one and writes a result with a lower hand at first and the higher one in  the end of the line.

For example, if you type the next block of text in the input:


    4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
    2h3h4h5d8d KdKs 9hJh

you will get:

    Output:
    Ad4s=Ac4d 5d6d As9s KhKd
    KdKs 9hJh

* If any board or hand card string is incorrect, you will receive an error

----

A few examples with a short explanation to test the program:

* Each pair has one pair, `5dTd` is higher:

    4cJs5h7cKc 4dTs 5dTd

* Each hand has two pairs, but `2dAd` is higher because of a higher pair:

    TcAs2h7cKc TdKs 2dAd

* Each hand has the same pairs, but `TdAs` is higher because of a higher kicker:

    KcKs2h7cTh TdAs QdTd

* Each hand has a full house with different pairs. `AdTd` is higher because has set of Aces:

    AcAsJcJsTs AdTd JdTc
