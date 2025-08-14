//> using scala "3.4.2"
//> using dep "org.jsoup:jsoup:1.21.1"

import org.jsoup._
import org.jsoup.nodes._
import scala.jdk.CollectionConverters._

object HtmlToMd:
  def main(args: Array[String]): Unit =
    if args.length != 1 then
      Console.err.println("Usage: HtmlToMd <input.html>")
      sys.exit(1)
    val html = java.nio.file.Files.readString(java.nio.file.Paths.get(args(0)))
    val doc  = Jsoup.parse(html)
    // Remove scripts/styles
    doc.select("script,style").remove()
    val md = blocksToMd(doc.body().children().asScala.toList)
    println(md.trim + "\n")

  // Top-level block renderer
  private def blocksToMd(nodes: List[Node], indent: Int = 0): String =
    nodes.map(n => nodeToMd(n, indent, inInline = false)).mkString("\n").replaceAll("\n{3,}", "\n\n")

  // Render a node either in block or inline context
  private def nodeToMd(n: Node, indent: Int, inInline: Boolean): String = n match
    case t: TextNode =>
      val text = t.text().replaceAll("\\s+", if inInline then " " else " ").trim
      escapeInline(text)
    case e: Element =>
      e.normalName() match
        // Headings
        case "h1" | "h2" | "h3" | "h4" | "h5" | "h6" =>
          val level = e.normalName().drop(1).toInt
          s"${"#" * level} ${inlineChildren(e)}\n"
        // Paragraph
        case "p" =>
          s"${inlineChildren(e)}\n"
        // Strong / Bold
        case "strong" | "b" =>
          s"**${inlineChildren(e)}**"
        // Emphasis / Italic
        case "em" | "i" =>
          s"*${inlineChildren(e)}*"
        // Anchor
        case "a" =>
          val href = e.attr("href").trim
          val label = inlineChildren(e).trim match
            case "" => href
            case s  => s
          if href.nonEmpty then s"[$label]($href)" else label
        // Image
        case "img" =>
          val src = e.attr("src")
          val alt = e.attr("alt")
          if src.nonEmpty then s"![$alt]($src)" else ""
        // Line break / horizontal rule
        case "br" => "  \n"
        case "hr" => "\n---\n"
        // Lists
        case "ul" =>
          e.children().asScala.toList.map(li => listItemToMd(li, indent, ordered = false)).mkString
        case "ol" =>
          val items = e.children().asScala.toList.zipWithIndex
          items.map { case (li, idx) => listItemToMd(li, indent, ordered = true, idx + 1) }.mkString
        case "li" =>
          // Should be handled by parent ul/ol, but just in case:
          s"${"  " * indent}- ${inlineOrBlocks(e, indent + 1)}"
        // Code
        case "code" if e.parent() != null && e.parent().normalName() == "pre" =>
          // handled in <pre>
          e.text()
        case "code" =>
          s"`${e.text().replaceAll("`", "\\\\`")}`"
        case "pre" =>
          val code = e.text().replaceAll("\r\n", "\n")
          s"\n```\n$code\n```\n"
        // Blockquote
        case "blockquote" =>
          val inner = blocksToMd(e.children().asScala.toList, indent).split("\n").map(l => s"> $l").mkString("\n")
          s"$inner\n"
        // Div/Span and unknown tags: pass through children
        case _ =>
          if isInline(e) then inlineChildren(e)
          else blocksToMd(e.children().asScala.toList, indent)
    case _ => ""

  private def inlineChildren(e: Element): String =
    e.childNodes().asScala.map(ch => nodeToMd(ch, indent = 0, inInline = true)).mkString("").trim

  private def inlineOrBlocks(e: Element, indent: Int): String =
    // If child has block-y elements, render blocks on new lines after the bullet
    val hasBlock = e.children().asScala.exists(ch => !isInline(ch))
    if !hasBlock then inlineChildren(e)
    else
      val content = blocksToMd(e.childNodes().asScala.toList, indent)
      s"\n${indentLines(content, indent)}"

  private def listItemToMd(li: Element, indent: Int, ordered: Boolean, num: Int = 1): String =
    val bullet = if ordered then s"$num." else "-"
    val content = inlineOrBlocks(li, indent + 1)
    val head = content.linesIterator.toList match
      case Nil => ""
      case first :: rest =>
        val start = s"${"  " * indent}$bullet $first\n"
        val tail =
          if rest.nonEmpty then rest.map(l => s"${"  " * (indent + 1)}$l").mkString("\n") + "\n" else ""
        start + tail
    head

  private def isInline(e: Element): Boolean =
    Set("a","span","strong","b","em","i","img","code","br").contains(e.normalName())

  private def indentLines(s: String, indent: Int): String =
    s.linesIterator.map(l => s"${"  " * indent}$l").mkString("\n")

  private def escapeInline(s: String): String =
    // minimal escaping for Markdown special chars in inline text
    s.replaceAll("([*_`\\[\\]])", "\\\\$1")
