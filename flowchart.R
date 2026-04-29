library(DiagrammeR)

grViz("
digraph activity_pipeline {

  graph [layout = neato, splines = ortho, outputorder = edgesfirst]

  node [fontname = 'Helvetica', fontsize = 13, margin = '0.2,0.12', penwidth = 1.5]
  edge [fontname = 'Helvetica', fontsize = 11, penwidth = 1.5, color = '#555555']

  # ── Row 1 ──────────────────────────────────────────────
  node [shape = oval, style = filled, fillcolor = '#1a5ea8', fontcolor = 'white']
  START [label = 'Raw sensor logs\n(door states + weight readings)', pos = '0,6!', width = 2.2]

  node [shape = rectangle, style = filled, fillcolor = '#dceefb', fontcolor = '#1a1a1a']
  B [label = 'Pair each door opening\nwith its nearest closing', pos = '3.5,6!', width = 2.2]

  node [shape = diamond, style = filled, fillcolor = '#fff3cd', fontcolor = '#1a1a1a']
  C [label = 'Event duration\nunrealistically long?', pos = '7,6!', width = 2.0]

  # ── Row 2 ──────────────────────────────────────────────
  node [shape = rectangle, style = filled, fillcolor = '#dceefb', fontcolor = '#1a1a1a']
  D [label = 'Repair event using\nnearby sensor clues',         pos = '0,3!', width = 2.2]
  E [label = 'Merge door 1 and door 2\ninto a single timeline', pos = '3.5,3!', width = 2.2]

  node [shape = diamond, style = filled, fillcolor = '#fff3cd', fontcolor = '#1a1a1a']
  F [label = 'Gap between events\n≤ 1.5 minutes?', pos = '7,3!', width = 2.0]

  # ── Row 3 ──────────────────────────────────────────────
  node [shape = rectangle, style = filled, fillcolor = '#dceefb', fontcolor = '#1a1a1a']
  G [label = 'Group into\none activity',      pos = '1.5,0.6!', width = 1.8]
  H [label = 'Treat as\nseparate activities', pos = '1.5,-0.6!', width = 1.8]
  I [label = 'Attach weight reading\njust before and after', pos = '5,0!', width = 2.2]

  node [shape = oval, style = filled, fillcolor = '#1a5ea8', fontcolor = 'white']
  END [label = 'Activity record\n(time · duration · weight change)', pos = '8.5,0!', width = 2.4]

  # ── Edges ──────────────────────────────────────────────
  START -> B
  B     -> C
  C:s   -> D:w [label = ' Yes']
  C:s   -> E:w [label = ' No ']
  D     -> E
  E     -> F
  F:s   -> G:w [label = ' Yes']
  F:s   -> H:w [label = ' No ']
  G     -> I
  H     -> I
  I     -> END
}
")
