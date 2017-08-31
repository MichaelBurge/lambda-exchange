#!/bin/sh

echo "\n1"

# cat <<'EOF' | curl localhost:2345/listBalances -XPOST -d @-
# {
#   "singleEntry":{
#     "amount":15000,
#     "account":1,
#     "currency":"USD"
#   }
# }
# EOF

echo "\n2"

cat <<'EOF' | curl localhost:2345/createMoney -XPOST -d @-
{
  "singleEntry":{
    "amount":15000,
    "account":1,
    "currency":"USD"
  }
}
EOF

echo "\n3"

cat <<'EOF' | curl localhost:2345/addOrder -XPOST -d @-
{
  "bid":{
    "toAmount":1,
    "user":1,
    "fromAmount":4600
  },
  "tag":"Request_AddBid"
}
EOF

echo "\n4"

cat <<'EOF' | curl localhost:2345/createMoney -XPOST -d @-
{
  "singleEntry":{
    "amount":15000,
    "account":2,
    "currency":"BTC"
  }
}
EOF

echo "\n5"

cat <<'EOF' | curl localhost:2345/listOrders -XPOST -d @-
{"user":null}
EOF
echo "\n5.5"

cat <<'EOF' | curl localhost:2345/addOrder -XPOST -d @-
{
  "ask":{
    "toAmount":1,
    "user":2,
    "fromAmount":-4500
  },
  "tag":"Request_AddAsk"
}
EOF

echo "\n6"

cat <<'EOF' | curl localhost:2345/listOrders -XPOST -d @-
{"user":null}
EOF


echo "\n7"

cat <<'EOF' | curl localhost:2345/listBalances -XPOST -d @-
{
  "singleEntry":{
    "amount":15000,
    "account":1,
    "currency":"USD"
  }
}
EOF

echo "\n8"

cat <<'EOF' | curl localhost:2345/listOrders -XPOST -d @-
{"user":null}
EOF

echo "\n9"
