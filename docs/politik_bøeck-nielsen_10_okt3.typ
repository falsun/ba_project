// File: typst-styling.typ

#show text: set text(
  font: ("IBM Plex Serif"),
  lang: "en"
)

#show raw: set text(font: "IBM Plex Mono")
#show math.equation: set text(font: "IBM Plex Math")
#show heading: set text(font: "IBM Plex Sans")

#set par(
  first-line-indent: 2em
)


#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#show link: set text(fill: rgb(0, 0, 255))
#show ref: set text(fill: rgb(0, 0, 255))

#set page(
  paper: "a4",
  margin: (bottom: 3.6cm,top: 3cm,x: 3cm,),
  numbering: "1",
)

#show: doc => article(
  title: [Untitled],
  authors: (
    ( name: [Frederik Bender Bøeck-Nielsen],
      affiliation: [Department of Political Science - University of Copenhagen],
      email: [lzq867\@alumni.ku.dk] ),
    ),
  date: [9. October, 2025],
  lang: "en",
  region: "GB",
  fontsize: 12pt,
  sectionnumbering: "1.1.1.",
  toc: true,
  toc_title: [Contents],
  toc_indent: 1.6em,
  toc_depth: 3,
  cols: 1,
  doc,
)

= Antagelsen om paralelle trends
<antagelsen-om-paralelle-trends>
#figure([
#box(image("output/figure_1_overview.pdf", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Trends in Military Spending by Group
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-overview>


#figure([
#box(image("output/figure_2_heterogeneity.pdf", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Exploring Heterogeneity Within the European NATO Sample
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-heterogeneity>





