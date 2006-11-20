
BASE_URL	= http://217.13.200.204:8080/reports-xml
YEAR		= 
LOGIN		=
OUTPUT_DIR	= ../payment-website/images/statistics

YEARLY_GRAPHICS	= contracts-by-week.svg
TOTAL_GRAPHICS	= all-contracts.svg

all:
	$(MAKE) yearly-graphics YEAR=2005
	$(MAKE) yearly-graphics YEAR=2006
	$(MAKE) total-graphics

yearly-graphics: $(YEARLY_GRAPHICS)
total-graphics: $(TOTAL_GRAPHICS)

.SUFFIXES: .lxsl .xsl .svg

.lxsl.xsl:
	xsltproc -o $@ loop-compiler.xslt $*.lxsl

.xsl.svg:
	xsltproc -o $*.xml $*.xsl '$(BASE_URL)/$*/$(YEAR)$(LOGIN)'
	xsltproc -o $(OUTPUT_DIR)/$*$(YEAR).svg buildSVGLineChart.xsl $*.xml
	rm $*.xml
