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

  <xsl:template match="kml:Placemark">  
    <!-- images here <img/><img/><img/> -->        
    <table style="background-color: rgb(186, 186, 186); width: 319px; height: 350px;" border="0" cellpadding="5" cellspacing="0">
      <tbody>
        <tr>
          <td colspan="3" style="width: 99px; text-align: left;">
                Create Rainforest Banner / BOS Logo
          </td>
        </tr>
        <tr>
          <td style="width: 100px;">
            <a href="http://createrainforest.org/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Galerie" src="http://createrainforest.org/infosystem/en/satellitenkarte.htm#"/>
              <br/> Galerie Slidwshow</a></td><td style="width: 33%;">
            <a href="http://createrainforest.org/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Panorama-klein" src="http://createrainforest.org/infosystem/en/satellitenkarte.htm#"/>
              <br/>Panorama-Mini</a></td><td style="width: 33%;"> <a href="http://createrainforest.org/infosystem/en/satellitenkarte.htm#">
              <img style="border: 0px solid ;" alt="Video" src="http://createrainforest.org/infosystem/en/satellitenkarte.htm#"/>
              <br/>Video</a></td></tr><tr><td style="width: 33%;" colspan="3">
            <h1><xsl:value-of select="kml:name"/></h1>
            <div style="text-align: justify;">
              <xsl:value-of select="descendant::bos:description/bos:content[@lang='en']"/>
              <a href="http://createrainforest.org/en/index">learn more</a></div>
          </td></tr><tr><td style="width: 99px;" colspan="3" align="center" valign="middle">
            <font color="#999999"><a href="http://createrainforest.org/en/index">create rainforest</a>
                  | copyright</font>
          </td></tr></tbody></table>
    <p/>    
  </xsl:template>

</xsl:stylesheet>

