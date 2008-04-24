<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" doctype-system="http://www.w3.org/TR/html4/strict.dtd" doctype-public="-//W3C//DTD HTML 4.01//EN" indent="yes" />
  <xsl:template match="/">
    <html>
    <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <link href="/static/poi/bos_style.css" rel="stylesheet">
    </link>
    <link href="/static/poi/highslide.css" rel="stylesheet">
    </link>
    <link href="http://createrainforest.org/rss/news" rel="alternate" title="RSS Feed" type="application/rss+xml">
    </link>
    <script type="text/javascript" src="/static/poi/prototype.js"></script>
    <script type="text/javascript" src="/static/poi/effects.js"></script>
    <script type="text/javascript" src="/static/poi/accordion.js"></script>
    <script type="text/javascript" src="/static/poi/highslide.js"></script>
    <script type="text/javascript" src="/static/poi/panorama.js"></script>
    <script type="text/javascript" src="/static/poi/functions.js"></script>
    <title><xsl:value-of select="poi/@title"/></title>
    </head>
    <body>
    <div id="content">
      <div id="header">
        <div id="menue_banner">
          <div id="menue_header_main">
            <div class="menue">|</div>
            <xsl:apply-templates select="poi/menu/entry" />
            
          </div>
        </div>
      </div>
      <div id="content_main">
        <div class="description_content">
          <div class="description_header"><xsl:value-of select="poi/@title"/></div>
          <div class="description_subheader"><xsl:value-of select="poi/@subtitle"/></div>
          <xsl:value-of select="poi/description"/> </div>
        <xsl:apply-templates select="poi/media" />
        
      </div>
      <div class="clearer"></div>
    </div>
    </body>
    </html>
  </xsl:template>
  <!--Anfang der templates-->
  <xsl:template match="poi">
    <xsl:value-of select="@id"/><br />
    <!--<xsl:value-of select="@href"/><br />-->
  </xsl:template>
  <xsl:template match="media">
    <xsl:if test="@type='image_gallery'">
      <div class="accordion_toggle"><xsl:value-of select="@title"/>:<xsl:value-of select="@subtitle"/></div>
      <div class="accordion_content">
        <div>
          <xsl:apply-templates select="image" /></div>
      </div>
    </xsl:if>
    <xsl:if test="@type='airal'">
      <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
      <xsl:variable name="url"><xsl:value-of select="image/url"/></xsl:variable>
      <xsl:variable name="title"><xsl:value-of select="@title"/></xsl:variable>
      <xsl:variable name="width"><xsl:value-of select="image/width"/></xsl:variable>
      <xsl:variable name="height"><xsl:value-of select="image/height"/></xsl:variable>
      <div class="accordion_toggle"><xsl:value-of select="@title"/>:<xsl:value-of select="@subtitle"/></div>
      <div class="accordion_content"><img class="airal" src="{$url}" alt="{$title}" title="{$title}" height="{$height}" width="{$width}"></img></div>
    </xsl:if>
    <xsl:if test="@type='panorama'">
      <xsl:variable name="id"><xsl:value-of select="image/@id"/></xsl:variable>
      <xsl:variable name="url"><xsl:value-of select="image/url"/></xsl:variable>
      <xsl:variable name="width"><xsl:value-of select="image/width"/></xsl:variable>
      <xsl:variable name="height"><xsl:value-of select="image/height"/></xsl:variable>
      <div class="accordion_toggle"><xsl:value-of select="@title"/>:<xsl:value-of select="@subtitle"/></div>
      <div class="accordion_content">
        <div class="scrollpane" style="border: 1px solid black; width: 600px; height: {$height}px; background-image: url({$url}); background-position: 0px 0px; background-repeat:no-repeat" id="pano-{$id}"></div>
        <script type="text/javascript">scroll('<xsl:value-of select="image/url"/>', 600, 'pano-<xsl:value-of select="image/@id"/>');</script>
      </div>
    </xsl:if>
    <xsl:if test="@type='movie'">
      <xsl:variable name="url"><xsl:value-of select="url"/></xsl:variable>
      <div class="accordion_toggle"><xsl:value-of select="@title"/>:<xsl:value-of select="@subtitle"/></div>
      <div class="accordion_content">
        <div class="movie">
          <object width="360" height="340">
            <param value="{$url}" name="movie"/>
            <embed width="425" height="355" type="application/x-shockwave-flash" src="{$url}"/>
          </object>
        </div>
      </div>
    </xsl:if>
  </xsl:template>
  <xsl:template match="entry">
    <xsl:variable name="title"><xsl:value-of select="@title"/></xsl:variable>
    <xsl:variable name="href"><xsl:value-of select="@href"/></xsl:variable>
    <div class="menue"><a class="menuelink" href="{$href}" shape="rect" target="_blank" ><xsl:value-of select="$title"/></a></div>
    <div class="menue">|</div>
  </xsl:template>
  <xsl:template match="image">
    <xsl:variable name="id"><xsl:value-of select="@id"/></xsl:variable>
    <xsl:variable name="url"><xsl:value-of select="url"/></xsl:variable>
    <xsl:variable name="title"><xsl:value-of select="@title"/></xsl:variable>
    <a id="thumb{$id}" href="{$url}" class="highslide" onClick="return hs.expand(this)"><img src="{$url}" alt="{$title}" title="{$title}" height="75" width="75"></img></a>
    <div class='highslide-caption'>
      <div class="img-title"><xsl:value-of select="@title"/></div>
      <div class="img-subtitle"><xsl:value-of select="@subtitle"/></div>
      <xsl:value-of select="description"/></div>
  </xsl:template>
</xsl:stylesheet>
