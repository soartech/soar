###
# Copyright 1995-2004 Soar Technology, Inc., University of Michigan. All 
# rights reserved.
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are 
# met:
# 
#    1.	Redistributions of source code must retain the above copyright 
#       notice, this list of conditions and the following disclaimer. 
# 
#    2.	Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in 
#       the documentation and/or other materials provided with the 
#       distribution. 
# 
# THIS SOFTWARE IS PROVIDED BY THE SOAR CONSORTIUM ``AS IS'' AND 
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE SOAR 
# CONSORTIUM  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH 
# DAMAGE.
# 
# The views and conclusions contained in the software and documentation 
# are those of the authors and should not be interpreted as representing 
# official policies, either expressed or implied, of Soar Technology, Inc., 
# the University of Michigan, or the Soar consortium.
### 

##
# Code to write a datamap in XML

namespace eval DmGenXmlOut {

# a simple hack to write out a string in XML with proper escapes
proc EscapeXmlString { s } {
   set s [join [split $s "<"] "&lt;"]
   set s [join [split $s ">"] "&gt;"]

   return $s
}

proc OpenXmlTag { fd tag attrs } {
   puts -nonewline $fd "<$tag "
   foreach { a v } $attrs {
      puts -nonewline $fd "$a=\"$v\" "
   }
   puts -nonewline $fd ">"
}
proc CloseXmlTag { fd tag } {
   puts $fd "</$tag>"
}

proc XmlTabs {  fd lvl { s  "  " } } {
   for { set i 0 } { $i < $lvl } { incr i } {
      puts -nonewline $fd $s
   }
}

proc getPsOrOpHref { type name } {
   return "$type-$name.xml"
}

proc writeXmlHeader { fd } {
   global dmGenVersion
   puts $fd "<?xml version=\"1.0\"?>\n"
   puts $fd "<!-- Generated by dmgen version [DmGenGetVersion]"
   puts $fd "     A Soar datamap generator by Soar Technology, Inc. -->"
}

proc WriteXmlDatamap { fn dm } {
   pushd $fn

   set pss [lsort [Datamap::GetProblemSpaces $dm]]
   set ops [lsort [Datamap::GetOperators $dm]]

   set dmfd [open index.xml w]
   writeXmlHeader $dmfd
   puts $dmfd "<dm name=\"[Datamap::GetName $dm]\">"
   foreach h [concat $pss $ops] {
      set name [Datamap::GetPsOrOpName $h]
      if [Datamap::IsProblemSpace $h] {
         set type S
      } else {
         set type O
      }
      set href [getPsOrOpHref $type $name]
      XmlTabs $dmfd 1
      puts $dmfd "<$type name=\"$name\" href=\"$href\"/>"
      set hfd [open $href w]
      writeXmlHeader $hfd
      WriteXmlPsOrOp $hfd $h 0
      close $hfd
   }
   puts $dmfd "</dm>"

   close $dmfd

   popd ;# pop back to original directory
}

proc WriteXmlPsOrOp { fd h depth } {
   global XmlVisited
   if [info exists XmlVisited] {
      unset XmlVisited
   }
   if [info exists XmlRefPaths] {
      unset XmlRefPaths
   }
   set graph [Datamap::GetGraph $h]
   set name [Datamap::GetPsOrOpName $h]
   if [Datamap::IsProblemSpace $h] {
      set typeName "Problem Space"
      set type S
   } else {
      set typeName "Operator"
      set type O
   }

   foreach v [$graph GetVertices] {
      set XmlVisited($v) 0
   }
   XmlTabs $fd $depth
   puts $fd "<$type name=\"[Datamap::GetPsOrOpName $h]\">"
   writeXmlDatamap_r $fd $graph [Datamap::GetStartVertex $h] [expr $depth + 1] {}
   XmlTabs $fd $depth
   puts $fd "</$type>"
}

proc writeXmlDatamap_r { fd graph vert depth path } {
   global XmlVisited XmlRefPaths
   set XmlVisited($vert) 1

   set out [$graph GetOutAdjacencies $vert]

   array set props [$graph Get $vert]

   set path [concat $path $props(name)]
   set XmlRefPaths($vert) $path

   XmlTabs $fd $depth
#   puts $fd "<vert id=\"$vert\" type=\"$props(type)\" spec=\"$props(special)\">"
   puts $fd "<vert type=\"$props(type)\" spec=\"$props(special)\" side=\"$props(side)\">"
   set nextDepth [expr $depth + 1]

   XmlTabs $fd $nextDepth
   puts $fd "<name>[EscapeXmlString $props(name)]</name>"

   XmlTabs $fd $nextDepth
   puts $fd "<vals>"
   set valDepth [expr $nextDepth + 1]
   foreach val $props(value) {
      XmlTabs $fd $valDepth
      puts $fd "<val>[EscapeXmlString $val]</val>"
   }
   XmlTabs $fd $nextDepth
   puts $fd "</vals>"

   if { $props(link) != {} } {
      set link $props(link)
      foreach { type root path } [join $link " "] {
         XmlTabs $fd $nextDepth
         puts $fd "<link type=\"$type\" name=\"$root\" path=\"$path\"/>"
      }
   }
   XmlTabs $fd $nextDepth
   puts $fd "<prods>"
   set prodDepth [expr $nextDepth + 1]
   foreach p $props(prods) {
      XmlTabs $fd $prodDepth
      puts $fd "<prod>[EscapeXmlString $p]</prod>"
   }
   XmlTabs $fd $nextDepth
   puts $fd "</prods>"

   foreach v $out {
      set name [$graph Get $v name]
      if { !$XmlVisited($v) } { ;# we haven't hit this vertex yet
         writeXmlDatamap_r $fd $graph $v $nextDepth $path
      } else {
         XmlTabs $fd $nextDepth
         set vpath [join $XmlRefPaths($v) "."]
#         puts $fd "<vertref id=\"$v\">[EscapeXmlString $vpath]</vertref>"
         puts $fd "<vertref>[EscapeXmlString $vpath]</vertref>"
      }
   }
   XmlTabs $fd $depth
   puts $fd "</vert>"
}

} ;# namespace eval DmgenXmlOut
