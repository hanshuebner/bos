<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:kml="http://earth.google.com/kml/2.1"
                xmlns:bos="http://headcraft.de/bos"
                version="1.0">
  <xsl:output method="html"/>
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>        
        <title>Sambodja POI template</title></head>
      <body>
        <xsl:apply-templates select="//kml:Placemark">
          <xsl:sort select="kml:name"/>
        </xsl:apply-templates>
      </body>
    </html>
  </xsl:template>
  
  <!-- language setting -->
  <xsl:param name="lang" select="'de'"/>
  <!-- server -->
  <xsl:param name="server" select="'http://test.createrainforest.org'"/>
  <!-- image_width -->
  <xsl:param name="image_width" select="60"/>
  
  <xsl:template match="kml:Placemark">  
    <table style="background-color: rgb(186, 186, 186); width: 319px; height: 350px;" border="0" cellpadding="5" cellspacing="0">
      <tbody>
        <tr>
          <td colspan="3" style="width: 99px; text-align: left;">             
            <img src="{$server}/images/header_ganzneu.gif" alt="Create Rainforest Banner / BOS Logo" width="400"/>
          </td>
        </tr>        
        <tr>
          <td style="width: 100px;">
            <a href="{$server}/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Galerie" src="{$server}/infosystem/en/satellitenkarte.htm#"/>
              <br/> Galerie Slidwshow</a></td><td style="width: 33%;">
            <a href="{$server}/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Panorama-klein" src="{$server}/infosystem/en/satellitenkarte.htm#"/>
              <br/>Panorama-Mini</a></td><td style="width: 33%;"> <a href="{$server}/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Video" src="{$server}/infosystem/en/satellitenkarte.htm#"/>
              <br/>Video</a></td></tr><tr><td style="width: 33%;" colspan="3">
            <h1><xsl:value-of select="descendant::bos:title/bos:content[@lang=$lang]"/></h1>
            <h2><xsl:value-of select="descendant::bos:subtitle/bos:content[@lang=$lang]"/></h2>
            <div style="text-align: left;">
              <xsl:value-of select="descendant::bos:description/bos:content[@lang=$lang]"/>
              <a href="{$server}/en/index">learn more</a></div>
            <xsl:apply-templates select="descendant::bos:image"/>
        </td></tr><tr><td style="width: 99px;" colspan="3" align="center" valign="middle">
            <font color="#999999"><a href="{$server}/en/index">create rainforest</a>
              | copyright</font>
    </td></tr></tbody></table>
    <p/>    
  </xsl:template>
  
  <xsl:template match="bos:image">
    <xsl:param name="id" select="bos:id"/>
    <xsl:param name="aspect_ratio" select="bos:width div bos:height"/>
    <!-- <xsl:message>val:<xsl:value-of select="$aspect_ratio"/></xsl:message> -->
    <xsl:param name="w" select="$image_width"/>
    <xsl:param name="h" select="$aspect_ratio * $w"/>
    <img src="{$server}/image/{$id}" width="{$w}" height="{$h}"/>
    &#160;
  </xsl:template>
</xsl:stylesheet>

