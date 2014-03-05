open Data
open Printf

let (//) = Filename.concat

let print_css print =
  let pf fs = ksprintf print fs in
    pf "
.edit {
background:#eeeeee ;
}

.editable-textarea {
background:#eeeeee ;
}

.done-hide-me {
visibility:hidden ;
display:none ;
}

.hide-me {
display:none ;
}

div.div-count {
}

p.div-title {
font-style:italic;font-family:Georgia,Times,serif;font-size:1.5em ;
color:#000000 ;
}

li.not_done {
font-style:italic;font-family:Georgia,Times,serif;font-size:1em ;
color:#bfe1f1 ;
} 


li.ok_generated {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#330033 ;
} 
li.ok_unchanged {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#888888 ;
} 

.error-msg {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#ff0000 ;
} 

li.ok_failed {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#ff0000 ;
} 

li.error {
background-color : #ffcccc ;
border: 1px solid #000000 ;
list-style: none ;
}

#chords td, #chords th 
{ 
font-size:1em;
border:1px solid #98bf21;
padding:2px 2px 2px 2px;
text-align:center;
height:50px ;
width:2cm ;
}

td.half-chords
{
width:1cm ;
}

#chords th 
{
font-size:1.1em;
text-align:left;
padding-top:5px;
padding-bottom:4px;
background-color:#A7C942;
color:#ffffff;
}
#chords tr.alt td 
{
color:#000000;
background-color:#EAF2D3;
}

span.note {
background-color:red
word-spacing:1px
}
div.title
{
padding:10px;
text-align:center ;
font-size:2em ;
}

div.auteur
{
padding:10px;
text-align:center ;
font-size:1.1em ;
}


#measure 
{
color:red
}

div#error-msg {
border:5px solid gray;
background-color:#FFEEEE ; 
}


div.sections
{
padding:10px;
margin:0px;
}

div#main-0 
{
padding:10px;
width:25cm ;
/* border:1px solid #98bf21; */
}

div#frame-title
{
padding:10px;
 background-color:#EEFFEE ; 
border:1px solid #98bf21;
}

div#main
{
/* border:5px solid pink ;*/
}

div#col_1
{
padding:10px;
float:left ;
/* border:5px solid #98bf21; */
/*background-color:#FFEEEE ; */
}

div#col_2
{
padding:10px;
float:right ;
/* top:2cm ; */
/* border:5px solid gray ;*/
/* background-color:#EEFFEE ; */

}

.lyrics-list {
padding : 10px ;
}

div.lyrics
{
padding:10px;
/* border:5px solid gray; */
margin:0px;
}

.lyrics-beat {
/* color:#ff0000 ; */
text-decoration:underline ;
font-weight:bold ;
}

div.structure
{
padding:10px;
border:5px solid gray;
margin:0px;
}

span.lyrics-section{
background-color:#eeaaee ;
}

span.note {
background-color:red
word-spacing:1px
}

ul.index-alphabet {
font-style:italic; font-family:Georgia, Times, serif; font-size:1em; 
color:#bfe1f1;
list-style-type:none ;
margin-left: 0cm ;
padding:0.1cm ;
border-left: 1px solid #999 ;
}

li.index-alphabet {
list-style-type : none ;
}

ul.index-auteur {
font-style:italic; font-family:Georgia, Times, serif; font-size:1em; 
color:#000000 ;
list-style-type:none ;
margin-left: 0.5cm ;
padding:0.1cm ;
border-left: 1px solid #999 ;
}

p.index-chanson {
margin-left: 2cm ;
}

ul.index-chanson {
font-style:italic; font-family:Georgia, Times, serif; font-size:0.5emx; 
color:#000000 ;
list-style-type:none ;
margin-left: 1cm ;
padding:0.1cm ;
} "
