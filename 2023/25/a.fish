begin
    echo 'digraph {'
    while read first rest
        set first (string sub --end -1 $first)
        for word in (string split ' ' $rest)
            echo "$first -> $word"
        end
    end
    echo '}'
end | dot -Tsvg >graph.svg # Requires the `graphviz` package.

# 1. Run this script.
# 2. Open graph.svg in a browser.
# 3. Zoom out and scroll to the right until you find the gap between the two clusters, with 3 lines connecting them.
# 4. Inspect the right-most node of the left cluster.
# 5. Run this in the console:
#    b = $0.getBoundingClientRect(); nodes = document.querySelectorAll(".node"); left = Array.from(nodes, node => node.getBoundingClientRect()).filter(rect => rect.x <= b.x); left.length * (nodes.length - left.length)
