default: slides.html

slides.html: slides.md template-revealjs.html lib css plugin *.css
	pandoc --from=markdown+raw_html+markdown_in_html_blocks --to=html -s -o slides.html slides.md -V revealjs-url=https://revealjs.com --template=template-revealjs.html --standalone --section-divs -V theme=black -V transition=linear --no-highlight

