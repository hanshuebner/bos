<xsl:stylesheet xmlns="http://www.w3.org/2000/svg"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/2000/xlink/namespace/"
                version="1.0">
  <xsl:output omit-xml-declaration="no"/>
  <xsl:template match="/graphData">
    <xsl:variable name="max">
      <xsl:value-of select="maxy"/>
    </xsl:variable>
    <xsl:variable name="min">
      <xsl:value-of select="miny"/>
    </xsl:variable>
    <xsl:variable name="maxy2">
      <xsl:value-of select="maxy2"/>
    </xsl:variable>
    <xsl:variable name="miny2">
      <xsl:value-of select="miny2"/>
    </xsl:variable>
    <xsl:variable name="maxx">
      <xsl:value-of select="maxx"/>
    </xsl:variable>
    <xsl:variable name="minx">
      <xsl:value-of select="minx"/>
    </xsl:variable>
    <svg width="800" height="600" onload="getSVGDoc(evt)" onzoom="ZoomControl()">
      <defs>
        <g id="star" transform="scale(0.21)">
          <polyline points="48,16,16,96,96,48,0,48,80,96">
          </polyline>
        </g>
        <g id="triangle" transform="scale(0.7)">
          <path id="Triangle" d="M 0 25 L 10 15 L 20 25 z" style="stroke:none"/> 
       </g>
        <g id="square" transform="scale(1)">
          <rect width="18" height="18">
          </rect>
        </g>
        <g id="rectangle" transform="scale(1)">
          <rect width="7" height="22">
          </rect>
        </g>
	<g id="none" transform="scale(1)">
	</g>
        <script type="text/javascript">
          <![CDATA[
          /* this code was largely reused from the excellent website SVG - Learning by Coding (http://svglbc.datenverdrahten.de/) */
                var svgdoc,svgroot;
                function getSVGDoc(load_evt)
                {
                  svgdoc=load_evt.target.ownerDocument;
                  svgroot=svgdoc.documentElement;
           
                  texte=svgdoc.getElementById("tooltip").getElementsByTagName("text");
                }
                function ShowTooltip(mousemove_event,txt)
                {
                  var ttrelem,tttelem,posx,posy,curtrans,ctx,cty,txt;
                  var sollbreite,maxbreite,ges,anz,tmp,txl,neu,i,k,l
                  ttrelem=svgdoc.getElementById("ttr");
                  tttelem=svgdoc.getElementById("ttt");
                  posx=mousemove_event.clientX;
                  posy=mousemove_event.clientY;
                  for(i=1;i<=5;i++)texte.item(i).firstChild.data="";
                  sollbreite=150;
                  tttelem.childNodes.item(0).data=txt;
                  ges=tttelem.getComputedTextLength();
                  tttelem.childNodes.item(0).data="";
                  anz=Math.ceil(ges/sollbreite);
                  tmp=txt.split(" ");
                  txl=new Array(tmp.length);
                  neu=new Array(anz);
                  for(i=0;i<tmp.length;i++)
                  {
          tttelem.childNodes.item(0).data=tmp[i];
          txl[i]=tttelem.getComputedTextLength();
                  }
                  k=0;
                  maxbreite=0;
                  for(i=0;i<anz;i++)
                  {
          l=0,neu[i]="";
          while(l+txl[k]<1.1*sollbreite && k<tmp.length)
          {
            l+=txl[k];
            neu[i]+=tmp[k]+" ";
            k++;
            if(maxbreite<l)maxbreite=l;
          }
                  }
                  curtrans=svgroot.currentTranslate;
                  ctx=curtrans.x;
                  cty=curtrans.y;
                  ttrelem.setAttribute("x",posx-ctx+10);
                  ttrelem.setAttribute("y",posy-cty-20+10);
                  ttrelem.setAttribute("width",maxbreite+2*(maxbreite-sollbreite)+40);
                  ttrelem.setAttribute("height",anz*15+3);
                  ttrelem.setAttribute("style","fill: #FFC; stroke: #000; stroke-width: 0.5px");
                  for(i=1;i<=anz;i++)
                  {
          texte.item(i).firstChild.data=neu[i-1];
          texte.item(i).setAttribute("x",posx-ctx+15);
          texte.item(i).setAttribute("y",parseInt(i-1)*15+posy-cty+3);
          texte.item(i).setAttribute("style","fill: #00C; font-size: 11px");
                  }
                  svgdoc.getElementById("tooltip").style.setProperty("visibility","visible");
                }
                function HideTooltip()
                {
                  svgdoc.getElementById("tooltip").style.setProperty("visibility","hidden");
                }
                function ZoomControl()
                {
                  var curzoom;
                  curzoom=svgroot.currentScale;
                  svgdoc.getElementById("tooltip").setAttribute("transform","scale("+1/curzoom+")");
                }
                ]]>
        </script>
      </defs>
      <g transform="translate(150,50) scale(0.5)">
        <!--Heading-->
        <text x="5" y="-40" text-anchor="left" font-weight="bolder"
              font-size="40" fill="maroon" text-decoration="underline">
          <xsl:value-of select="title"/>
        </text>
        <!--Caption (Vertical)-->
        <g transform="translate(-220, 80) rotate(270, 0, 0)">
          <text x="0" y="0" text-anchor="middle" font-weight="bolder"
                font-size="36" fill="black">
            <xsl:value-of select="ytitle"/>
          </text>
        </g>
        <g transform="translate(1220, 80) rotate(90, 0, 0)">
          <text x="0" y="0" text-anchor="middle" font-weight="bolder"
                font-size="36" fill="black">
            <xsl:value-of select="y2title"/>
          </text>
        </g>
        <!--Caption (Horizontal)-->
        <text x="1070" y="1000" font-size="36" font-weight="bolder" fill="black">
          <xsl:value-of select="xtitle"/>
        </text>
        <!-- Now Draw the main X and Y axis -->
        <g style="stroke-width:5; stroke:black">
          <!-- X Axis -->
          <path d="M 0 1000 L 1000 1000 Z"/>
          <!-- Y Axis -->
          <path d="M 0 0 L 0 1000 Z"/>
          <xsl:if test="y2axis='true'">
            <!-- Y2 Axis -->
            <path d="M 1000 0 L 1000 1000 Z"/>
          </xsl:if>
        </g>
        <xsl:for-each select="sets/set">
          <!-- display the x-axis labels -->
          <g style="stroke:red; stroke-width: 3; fill:black; stroke:none ;font-size:24; text-anchor:middle">
            <xsl:for-each select="measure[xlabel]">
              <!-- for all measures with an xlabel -->
              <xsl:call-template name="xlabel">
                <xsl:with-param name="xvalue" select="xvalue"/>
                <xsl:with-param name="xlabel" select="xlabel"/>
                <xsl:with-param name="minx" select="$minx"/>
                <xsl:with-param name="maxx" select="$maxx"/>
              </xsl:call-template>
            </xsl:for-each>
            <xsl:for-each select="../../xvalues/xvalue">
              <!-- for all (explicit) xvalues  -->
              <xsl:call-template name="xlabel">
                <xsl:with-param name="xvalue" select="value"/>
                <xsl:with-param name="xlabel" select="label"/>
                <xsl:with-param name="minx" select="$minx"/>
                <xsl:with-param name="maxx" select="$maxx"/>
                <xsl:with-param name="gridline" select="gridline"/>
              </xsl:call-template>
            </xsl:for-each>
          </g>
          <xsl:for-each select="../../xvalues/xmarkers">
            <!-- draw y-markers on the vertical axis -->
            <xsl:call-template name="xmarker">
              <xsl:with-param name="start" select="minvalue"/>
              <xsl:with-param name="n" select="steps"/>
              <xsl:with-param name="stepsize"
                              select="(maxvalue - minvalue) div steps"/>
              <xsl:with-param name="min" select="$minx"/>
              <xsl:with-param name="max" select="$maxx"/>
              <xsl:with-param name="gridline" select="gridline"/>
            </xsl:call-template>
          </xsl:for-each>
          <!-- end of x-axis markers, labels and vertical grid-lines -->
          <!-- create the y-axis markers -->
          <xsl:for-each select="measure[ylabel]">
            <!-- for all measures with an ylabel -->
            <xsl:call-template name="ylabel">
              <xsl:with-param name="yvalue" select="yvalue"/>
              <xsl:with-param name="ylabel" select="ylabel"/>
              <xsl:with-param name="min" select="$min"/>
              <xsl:with-param name="max" select="$max"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="../../yvalues/yvalue">
            <!-- for all (explicit) yvalues  -->
            <xsl:call-template name="ylabel">
              <xsl:with-param name="yvalue" select="value"/>
              <xsl:with-param name="ylabel" select="label"/>
              <xsl:with-param name="min" select="$min"/>
              <xsl:with-param name="max" select="$max"/>
              <xsl:with-param name="gridline" select="gridline"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="../../yvalues/ymarkers">
            <!-- draw y-markers on the vertical axis -->
            <xsl:call-template name="ymarker">
              <xsl:with-param name="start" select="minvalue"/>
              <xsl:with-param name="n" select="steps"/>
              <xsl:with-param name="stepsize"
                              select="(maxvalue - minvalue) div steps"/>
              <xsl:with-param name="min" select="$min"/>
              <xsl:with-param name="max" select="$max"/>
              <xsl:with-param name="gridline" select="gridline"/>
            </xsl:call-template>
          </xsl:for-each>
          <!-- end of y-axis markers, labels and horizontal grid-lines -->
          <!-- create the y2-axis markers -->
          <xsl:for-each select="measure[y2label]">
            <!-- for all measures with an ylabel -->
            <xsl:call-template name="ylabel">
              <xsl:with-param name="yvalue" select="y2value"/>
              <xsl:with-param name="ylabel" select="y2label"/>
              <xsl:with-param name="min" select="$miny2"/>
              <xsl:with-param name="max" select="$maxy2"/>
              <xsl:with-param name="axis" select="2"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="../../y2values/y2value">
            <!-- for all (explicit) yvalues  -->
            <xsl:call-template name="ylabel">
              <xsl:with-param name="yvalue" select="value"/>
              <xsl:with-param name="ylabel" select="label"/>
              <xsl:with-param name="min" select="$miny2"/>
              <xsl:with-param name="max" select="$maxy2"/>
              <xsl:with-param name="gridline" select="gridline"/>
              <xsl:with-param name="axis" select="2"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="../../y2values/y2markers">
            <!-- draw y-markers on the vertical axis -->
            <xsl:call-template name="ymarker">
              <xsl:with-param name="start" select="minvalue"/>
              <xsl:with-param name="n" select="steps"/>
              <xsl:with-param name="stepsize"
                              select="(maxvalue - minvalue) div steps"/>
              <xsl:with-param name="min" select="$miny2"/>
              <xsl:with-param name="max" select="$maxy2"/>
              <xsl:with-param name="gridline" select="gridline"/>
              <xsl:with-param name="axis" select="2"/>
            </xsl:call-template>
          </xsl:for-each>
          <!-- end of y2-axis markers, labels and horizontal grid-lines -->

          <!-- go and draw the line of the chart itself -->
          <g stylet="stroke:red; stroke-width: 3; fill : none;">
      <xsl:attribute name="style">
        stroke:<xsl:value-of select="@color"/>;stroke-width: 3; fill : none;
      </xsl:attribute>
            <!-- instead of a polyline, make a line from the previous to each new point -->
            <xsl:for-each select="measure">
              <xsl:variable name="x">
                <xsl:value-of select=" 1000* ((xvalue - ($minx)) div ($maxx - $minx))"/>
              </xsl:variable>
              <xsl:variable name="y">
                <xsl:choose>
                  <xsl:when test="yvalue">
                    <xsl:value-of select="1000 - 1000* ((yvalue - ($min)) div ($max - $min))"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="1000 - 1000* ((y2value - ($miny2)) div ($maxy2 - $miny2))"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:if test="not(../@showline='false')">
                <xsl:if test="(position() &gt; 1)">
                  <line>
                    <xsl:attribute name="x1">
                      <xsl:value-of select=" 1000* ((preceding-sibling::measure[position()=1]/xvalue - ($minx)) div ($maxx - $minx))"/>
                    </xsl:attribute>
                    <xsl:attribute name="y1">
                      <xsl:choose>
                        <xsl:when test="yvalue"> 
                          <xsl:value-of select="1000 - 1000* ((preceding-sibling::measure[position()=1]/yvalue - ($min)) div ($max - $min))"/>
                        </xsl:when>
                        <xsl:otherwise>
                          <xsl:value-of select="1000 - 1000* ((preceding-sibling::measure[position()=1]/y2value - ($miny2)) div ($maxy2 - $miny2))"/>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:attribute>
                    <xsl:attribute name="x2">
                      <xsl:value-of select="$x"/>
                    </xsl:attribute>
                    <xsl:attribute name="y2">
                      <xsl:value-of select="$y"/>
                    </xsl:attribute>
                  </line>
                </xsl:if>
              </xsl:if>
              <xsl:if test="xgrid = 'true'">
                <xsl:call-template name="gridline">
                  <xsl:with-param name="x1" select="$x"/>
                  <xsl:with-param name="y1" select="$y"/>
                  <xsl:with-param name="type">vertical</xsl:with-param>
                </xsl:call-template>
              </xsl:if>
              <xsl:if test="ygrid = 'true'">
                <xsl:call-template name="gridline">
                  <xsl:with-param name="x1" select="$x"/>
                  <xsl:with-param name="y1" select="$y"/>
                  <xsl:with-param name="type">horizontal</xsl:with-param>
                  <xsl:with-param name="yaxis">
                    <xsl:choose>
                      <xsl:when test="yvalue">1</xsl:when>
                      <xsl:otherwise>2</xsl:otherwise>
                    </xsl:choose>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:if>
            </xsl:for-each> <!-- measure -->
          </g>
        </xsl:for-each> <!-- sets -->
            <!-- now again traverse all measures to place markers and create annotations;
                 by doing this in a second go, we ensure (according to the 'painters algoritm' 
                 (see: http://wiki.svg.org/index.php/ChangingDrawingOrder)) that the annotations
                 and markers are on top of everything else.
                 -->
        <xsl:for-each select="sets/set">
            <xsl:for-each select="measure">
              <xsl:variable name="x">
                <xsl:value-of select=" 1000* ((xvalue - ($minx)) div ($maxx - $minx))"/>
              </xsl:variable>
              <xsl:variable name="y">
                <xsl:choose>
                  <xsl:when test="yvalue">
                    <xsl:value-of select="1000 - 1000* ((yvalue - ($min)) div ($max - $min))"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="1000 - 1000* ((y2value - ($miny2)) div ($maxy2 - $miny2))"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
             <g >
               <xsl:attribute name="style">stroke:<xsl:value-of select="../@color"/> stroke-width: 3; fill : none;</xsl:attribute>
              <!-- draw a marker -->
              <xsl:call-template name="marker">
                <xsl:with-param name="x" select="$x"/>
                <xsl:with-param name="y" select="$y"/>
                <xsl:with-param name="label" select="label"/>
                <xsl:with-param name="marker" select="../@marker-type"/>
                <xsl:with-param name="color" select="../@color"/>
              </xsl:call-template>
              <xsl:if test="annotation">
                <!-- currently the annotation is written before the line ; that means that the line will cover the annotation -->
                <xsl:call-template name="annotation">
                  <xsl:with-param name="x" select="$x"/>
                  <xsl:with-param name="y" select="$y"/>
                  <xsl:with-param name="annotation" select="annotation"/>
                </xsl:call-template>
              </xsl:if>
           </g>
            </xsl:for-each>  <!-- measures in set -->
        </xsl:for-each> <!-- sets -->
        <!-- legend per set -->
      <g id="legend" style="fill:lavenderblush;stroke-width:2;stroke:black" transform="translate(1080,500)">
        <!-- Tooltip - Beginn (ttr=Tooltip-Rechteck, ttt=Tooltip-Text) -->
        <rect id="legend" x="0" y="0" rx="5" ry="5" width="200" >
        <xsl:attribute name="height"><xsl:value-of select="29+ 20* count(sets/set)"/></xsl:attribute>
        </rect>
        <text x="10" y="15" style="text-decoration:underline;stroke-width:1;stroke:black;fill:black">Legend:</text>
        <xsl:for-each select="sets/set">
              <xsl:call-template name="marker">
                <xsl:with-param name="x">15</xsl:with-param>
                <xsl:with-param name="y" select="7+20*(position())"/>
                <xsl:with-param name="label" select="title"/>
                <xsl:with-param name="marker" select="@marker-type"/>
                <xsl:with-param name="color" select="@color"/>
              </xsl:call-template>
          <text x="27">
            <xsl:attribute name="y"><xsl:value-of select="15+20*(position())"/></xsl:attribute>
            <xsl:attribute name="style">stroke-width:1;stroke:<xsl:value-of select="@color" />;fill:<xsl:value-of select="@color" /></xsl:attribute>
            <xsl:value-of select="@title"/>
          </text>
          </xsl:for-each>
      </g>

      </g>
      <g id="tooltip" style="visibility: hidden">
        <!-- Tooltip - Beginn (ttr=Tooltip-Rechteck, ttt=Tooltip-Text) -->
        <rect id="ttr" x="0" y="0" rx="5" ry="5" width="100" height="16"/>
        <text id="ttt" x="0" y="0" style="visibility: hidden">dyn. Text</text>
        <text x="-10" y="-10">dyn. Text</text>
        <text x="-10" y="-10">dyn. Text</text>
        <text x="-10" y="-10">dyn. Text</text>
        <text x="-10" y="-10">dyn. Text</text>
        <text x="-10" y="-10">dyn. Text</text>
      </g>
      <!-- Tooltip - Ende -->
    </svg>
  </xsl:template>
  <xsl:template name="xlabel">
    <xsl:param name="xvalue"/>
    <xsl:param name="maxx"/>
    <xsl:param name="minx"/>
    <xsl:param name="xlabel"/>
    <xsl:param name="gridline"/>
    <g>
      <xsl:attribute name="transform">rotate(315,               
                  
        
        <xsl:value-of select="1000* (($xvalue - ($minx)) div ($maxx - $minx))"/>


,1014)</xsl:attribute>
      <text y="1014" style="text-anchor:end">
        <xsl:attribute name="x">
          <xsl:value-of select="1000* (($xvalue - ($minx)) div ($maxx - $minx))"/>
        </xsl:attribute>
        <xsl:value-of select="$xlabel"/>
      </text>
    </g>
    <xsl:if test="$gridline = 'true'">
      <xsl:call-template name="gridline">
        <xsl:with-param name="x1">
          <xsl:value-of select="1000* (($xvalue - ($minx)) div ($maxx - $minx))"/>
        </xsl:with-param>
        <xsl:with-param name="y1">0</xsl:with-param>
        <xsl:with-param name="type">vertical</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!-- xlabel -->
  <xsl:template name="ylabel">
    <xsl:param name="yvalue"/>
    <xsl:param name="max"/>
    <xsl:param name="min"/>
    <xsl:param name="ylabel"/>
    <xsl:param name="gridline"/>
    <xsl:param name="axis">1</xsl:param>
    <g id="ylabel">
      <xsl:attribute name="transform">rotate(<xsl:value-of select="325 + ($axis - 1) * -300 "/>, <xsl:value-of select="-10 + ($axis - 1) * 1020 "/>,
                    
          
        <xsl:value-of select="1000-1000* (($yvalue - ($min)) div ($max - $min))"/>

  )</xsl:attribute>
      <text x="-10" font-size="20pt">
        <xsl:attribute name="style">text-anchor:
          <xsl:choose>
            <xsl:when test="$axis=1">end</xsl:when>
            <xsl:otherwise>start</xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <xsl:attribute name="x">
          <xsl:value-of select="-10 + ($axis - 1) * 1020 "/>
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="1000 - 1000* (($yvalue - ($min)) div ($max - $min))"/>
        </xsl:attribute>
        <xsl:value-of select="$ylabel"/>
      </text>
    </g>
    <xsl:if test="$gridline = 'true'">
      <xsl:call-template name="gridline">
        <xsl:with-param name="y1">
          <xsl:value-of select="1000 - 1000* (($yvalue - ($min)) div ($max - $min))"/>
        </xsl:with-param>
        <xsl:with-param name="x1"><xsl:value-of select="1000- 1000*($axis -1 )" /></xsl:with-param>
        <xsl:with-param name="type">horizontal</xsl:with-param>
        <xsl:with-param name="yaxis"><xsl:value-of select="$axis" /></xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!-- ylabel -->
  <xsl:template name="annotation">
    <xsl:param name="x"/>
    <xsl:param name="y"/>
    <xsl:param name="annotation"/>
    <!-- display an annotation -->
    <g style="stroke-width:1;stroke:blue">
      <xsl:variable name="height">
        <xsl:value-of select="8+16*round(string-length($annotation) div 25)"/>
      </xsl:variable>
      <xsl:variable name="y_anno">
        <xsl:choose>
          <xsl:when test="$y &lt; 300">-150</xsl:when>
          <xsl:otherwise>150</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="line_y">
        <xsl:choose>
          <xsl:when test="$y &lt; 300"><xsl:value-of select="-40+ $y - $y_anno + 0.35* $height" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="-40+ $y - $y_anno + 0.65* $height"/></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <line>
        <!-- vertical line either upwards (default) or downwards (for the over the 70% of max values -->
        <xsl:attribute name="x1">
          <xsl:value-of select="$x"/>
        </xsl:attribute>
        <xsl:attribute name="y1">
          <xsl:value-of select="$y"/>
        </xsl:attribute>
        <xsl:attribute name="x2">
          <xsl:value-of select="$x"/>
        </xsl:attribute>
        <xsl:attribute name="y2">
          <xsl:value-of select="$line_y "/>
        </xsl:attribute>
      </line>
      <line>
        <!-- horizontal line to right (default) or left (for the over 80% of max x values -->
        <xsl:attribute name="x1">
          <xsl:value-of select="$x"/>
        </xsl:attribute>
        <xsl:attribute name="y1">
          <xsl:value-of select="$line_y "/>
        </xsl:attribute>
        <xsl:attribute name="x2">
        <xsl:choose>
          <xsl:when test="$x &gt; 750"><xsl:value-of select="-7+$x"/></xsl:when>
          <xsl:otherwise><xsl:value-of select="7+$x"/></xsl:otherwise>
        </xsl:choose>
        </xsl:attribute>
        <xsl:attribute name="y2">
          <xsl:value-of select="$line_y"/>
        </xsl:attribute>
      </line>
      <rect width="210"  style="fill:lightyellow"  >
        <!-- box to hold the annotation; starts at the horizontal line and extends to the right (default) or left (for over 80% of max x value -->
        <xsl:attribute name="height">
          <xsl:value-of select="$height"/>
        </xsl:attribute>
        <xsl:attribute name="x">
        <xsl:choose>
          <xsl:when test="$x &gt; 750"><xsl:value-of select="-7+$x -210"/></xsl:when>
          <xsl:otherwise><xsl:value-of select="7+$x"/></xsl:otherwise>
        </xsl:choose>
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="-40+$y - $y_anno"/>
        </xsl:attribute>
      </rect>
      <!-- we may need to split the text in multiple lines; our annotation textbox has a width of 40 which means around 17 characters??-->
      <xsl:call-template name="annotationLine">
        <xsl:with-param name="x" select="$x"/>
        <xsl:with-param name="y" select="$y - $y_anno"/>
        <xsl:with-param name="annotation" select="$annotation"/>
        <xsl:with-param name="line">1</xsl:with-param>
      </xsl:call-template>
    </g>
  </xsl:template>
  <xsl:template name="marker">
    <xsl:param name="x"/>
    <xsl:param name="y"/>
    <xsl:param name="label"/>
    <xsl:param name="marker">circle</xsl:param>
    <xsl:param name="color">red</xsl:param>
    <g  onmouseout="HideTooltip(evt)" transform="scale(1)">
      <xsl:attribute name="style">
        stroke:<xsl:value-of select="$color"/>;fill:<xsl:value-of select="$color"/>
      </xsl:attribute>
      <xsl:attribute name="onmouseover">
        ShowTooltip(evt,'<xsl:value-of select="$label"/>')
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="$marker='square'">
          <use xlink:href="#square">
            <xsl:attribute name="x">
              <xsl:value-of select="$x -9"/>
            </xsl:attribute>
            <xsl:attribute name="y">
              <xsl:value-of select="$y -9"/>
            </xsl:attribute>
          </use>
        </xsl:when>
        <xsl:when test="$marker='triangle'">
          <use xlink:href="#triangle">
            <xsl:attribute name="x">
              <xsl:value-of select="$x -9"/>
            </xsl:attribute>
            <xsl:attribute name="y">
              <xsl:value-of select="$y -9"/>
            </xsl:attribute>
          </use>
        </xsl:when>
        <xsl:when test="$marker='rectangle'">
          <use xlink:href="#rectangle">
            <xsl:attribute name="x">
              <xsl:value-of select="$x -4"/>
            </xsl:attribute>
            <xsl:attribute name="y">
              <xsl:value-of select="$y -4"/>
            </xsl:attribute>
          </use>
        </xsl:when>
        <xsl:when test="$marker='star'">
          <use xlink:href="#star">
            <xsl:attribute name="x">
              <xsl:value-of select="$x -9"/>
            </xsl:attribute>
            <xsl:attribute name="y">
              <xsl:value-of select="$y -9"/>
            </xsl:attribute>
          </use>
        </xsl:when>
        <xsl:when test="$marker='diamond'">
          <!-- diamond is just a square rotated about its own center for 45 degrees -->
          <use xlink:href="#square">
            <xsl:attribute name="x">
              <xsl:value-of select="$x -9"/>
            </xsl:attribute>
            <xsl:attribute name="y">
              <xsl:value-of select="$y -9"/>
            </xsl:attribute>
            <xsl:attribute name="transform">
               rotate(45,<xsl:value-of select="$x"/>,<xsl:value-of select="$y "/>)
            </xsl:attribute>
          </use>
        </xsl:when>
        <xsl:when test="$marker='circle'">
          <circle r="9">
            <xsl:attribute name="cx">
              <xsl:value-of select="$x"/>
            </xsl:attribute>
            <xsl:attribute name="cy">
              <xsl:value-of select="$y"/>
            </xsl:attribute>
          </circle>
        </xsl:when>
        <xsl:when test="$marker='smallcircle'">
          <circle r="4">
            <xsl:attribute name="cx">
              <xsl:value-of select="$x"/>
            </xsl:attribute>
            <xsl:attribute name="cy">
              <xsl:value-of select="$y"/>
            </xsl:attribute>
          </circle>
        </xsl:when>
        <xsl:otherwise>
        </xsl:otherwise>
      </xsl:choose>
    </g>
  </xsl:template>
  <xsl:template name="xmarker">
    <xsl:param name="n"/>
    <xsl:param name="i">0</xsl:param>
    <xsl:param name="stepsize"/>
    <xsl:param name="min"/>
    <xsl:param name="max"/>
    <xsl:param name="gridline">false</xsl:param>
    <xsl:param name="start">
      <xsl:value-of select="$min"/>
    </xsl:param>
    <xsl:variable name="x">
      <xsl:value-of select="1000* (($start + $i* $stepsize - $min) div ($max - $min))"/>
    </xsl:variable>
    <line y1="1000" y2="985">
      <xsl:attribute name="x1">
        <xsl:value-of select="$x"/>
      </xsl:attribute>
      <xsl:attribute name="x2">
        <xsl:value-of select="$x"/>
      </xsl:attribute>
    </line>
    <xsl:call-template name="xlabel">
      <xsl:with-param name="xvalue" select="$start + $i* $stepsize"/>
      <xsl:with-param name="xlabel">
        <xsl:value-of select="round($i * $stepsize +$start)"/>
"</xsl:with-param>
      <xsl:with-param name="minx" select="$min"/>
      <xsl:with-param name="maxx" select="$max"/>
    </xsl:call-template>
    <xsl:if test="$gridline = 'true'">
      <xsl:call-template name="gridline">
        <xsl:with-param name="y1">0</xsl:with-param>
        <xsl:with-param name="x1" select="$x"/>
        <xsl:with-param name="type">vertical</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="$i &lt; $n">
      <xsl:call-template name="xmarker">
        <xsl:with-param name="i" select="$i+1"/>
        <xsl:with-param name="n" select="$n"/>
        <xsl:with-param name="stepsize" select="$stepsize"/>
        <xsl:with-param name="min" select="$min"/>
        <xsl:with-param name="max" select="$max"/>
        <xsl:with-param name="start" select="$start"/>
        <xsl:with-param name="gridline" select="$gridline"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!-- end of xmarker -->
  <xsl:template name="ymarker">
    <xsl:param name="n"/>
    <xsl:param name="i">0</xsl:param>
    <xsl:param name="stepsize"/>
    <xsl:param name="min"/>
    <xsl:param name="max"/>
    <xsl:param name="gridline">false</xsl:param>
    <xsl:param name="start">
      <xsl:value-of select="$min"/>
    </xsl:param>
    <xsl:param name="axis">1</xsl:param>
    <xsl:variable name="y">
      <xsl:value-of select="1000 - 1000* (($start + $i* $stepsize - $min) div ($max - $min))"/>
    </xsl:variable>
    <xsl:value-of select="$i"/>
    <line x1="0" x2="18">
      <xsl:attribute name="y1">
        <xsl:value-of select="$y"/>
      </xsl:attribute>
      <xsl:attribute name="y2">
        <xsl:value-of select="$y"/>
      </xsl:attribute>
    </line>
    <text x="-20">
      <xsl:attribute name="x">
        <xsl:value-of select="-20 + 1030*($axis -1)"/>
      </xsl:attribute>
      <xsl:attribute name="style">text-anchor:
      <xsl:choose>
        <xsl:when test="$axis=1">end</xsl:when>
        <xsl:otherwise>start</xsl:otherwise>
      </xsl:choose>
      ;font-size:        
        <xsl:choose>
          <xsl:when test="$i=$n">40</xsl:when>
          <xsl:when test="$i=0">40</xsl:when>
          <xsl:otherwise>20</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="y">
        <xsl:value-of select="$y +5"/>
      </xsl:attribute>
      <xsl:value-of select="round($i * $stepsize +$start)"/>
    </text>
    <xsl:if test="$gridline = 'true'">
      <xsl:call-template name="gridline">
        <xsl:with-param name="x1"><xsl:value-of select="1000 - 1000*( $axis -1)"/></xsl:with-param>
        <xsl:with-param name="y1" select="$y"/>
        <xsl:with-param name="type">horizontal</xsl:with-param>
        <xsl:with-param name="yaxis"><xsl:value-of select="$axis"/></xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="$i &lt; $n">
      <xsl:call-template name="ymarker">
        <xsl:with-param name="i" select="$i+1"/>
        <xsl:with-param name="n" select="$n"/>
        <xsl:with-param name="stepsize" select="$stepsize"/>
        <xsl:with-param name="min" select="$min"/>
        <xsl:with-param name="max" select="$max"/>
        <xsl:with-param name="start" select="$start"/>
        <xsl:with-param name="gridline" select="$gridline"/>
        <xsl:with-param name="axis" select="$axis"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!-- ymarker -->
  <xsl:template name="gridline">
    <xsl:param name="x1"/>
    <xsl:param name="y1"/>
    <xsl:param name="type"/>
    <xsl:param name="yaxis">1</xsl:param>
    <line style="fill:none; stroke:#B0B0B0; stroke-width:2; stroke-dasharray:2 4">
      <xsl:attribute name="x1">
        <xsl:value-of select="$x1"/>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="$type='horizontal'">
          <xsl:attribute name="x2"><xsl:value-of select="($yaxis -1)* 1000 "/></xsl:attribute>
          <xsl:attribute name="y2">
            <xsl:value-of select="$y1"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="y2">1000</xsl:attribute>
          <xsl:attribute name="x2">
            <xsl:value-of select="$x1"/>
          </xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:attribute name="y1">
        <xsl:value-of select="$y1"/>
      </xsl:attribute>
    </line>
  </xsl:template>
  <!-- gridline -->
  
  <!-- write line in annotation box -->
  <xsl:template name="annotationLine" >
    <xsl:param name="x"/>
    <xsl:param name="y"/>
    <xsl:param name="annotation"/>
    <xsl:param name="line">1</xsl:param>
      <text font-size="16" fill="black">
        <xsl:attribute name="text-anchor">        
        <xsl:choose>
          <xsl:when test="$x &gt; 750">end</xsl:when>
          <xsl:otherwise>start</xsl:otherwise>
        </xsl:choose>
        </xsl:attribute>
        <xsl:attribute name="x">
        <xsl:choose>
          <xsl:when test="$x &gt; 750"><xsl:value-of select="-10+$x"/></xsl:when>
          <xsl:otherwise><xsl:value-of select="10+$x"/></xsl:otherwise>
        </xsl:choose>
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="-25+$y +16*($line -1)"/>
        </xsl:attribute>
        <xsl:value-of select="substring($annotation,1,25)"/>
      </text>
    <xsl:if test="string-length($annotation) > 25">
      <xsl:call-template name="annotationLine">
        <xsl:with-param name="x" select="$x"/>
        <xsl:with-param name="y" select="$y"/>
        <xsl:with-param name="annotation" select="substring($annotation,26)"/>
        <xsl:with-param name="line" select="$line+1"/>
      </xsl:call-template>
    </xsl:if>
</xsl:template>

</xsl:stylesheet>
