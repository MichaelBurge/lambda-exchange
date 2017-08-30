#!/bin/sh

cat <<'EOF' | curl localhost:2345/addOrder -XPOST -d @-
{
  "bid":{
    "toAmount":1,
    "user":0,
    "fromAmount":4600
  },
  "tag":"Request_AddBid"
}
EOF

echo

cat <<'EOF' | curl localhost:2345/listOrders -XPOST -d @-
{"user":null}
EOF

echo

cat <<'EOF' | curl localhost:2345/addOrder -XPOST -d @-
{
  "bid":{
    "toAmount":1,
    "user":0,
    "fromAmount":4600
  },
  "tag":"Request_AddBid"
}
EOF

echo

cat <<'EOF' | curl localhost:2345/listOrders -XPOST -d @-
{"user":null}
EOF
