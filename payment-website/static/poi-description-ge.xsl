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
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>
  
  <!-- language setting - will be set from outside -->
  <!-- <xsl:param name="lang" select="'de'"/> -->
  <xsl:param name="lang" select="'de'"/>
  <!-- server -->
  <xsl:param name="server" select="'http://test.createrainforest.org'"/>
  <!-- image_width -->
  <xsl:param name="image_width" select="120"/>
  
  <xsl:template match="bos:poi">     
    <table width="500" style="background-color: rgb(186, 186, 186);"
           border="0" cellpadding="5" cellspacing="0">
      <tbody>
        <tr>
          <td colspan="3" style="width: 99px; text-align: left;">             
            <img src="{$server}/images/header_ganzneu.gif" alt="Create Rainforest Banner / BOS Logo" width="400"/>
          </td>
        </tr>        
        <tr>
          <td style="width: 100px;">
            <h1><xsl:value-of select="bos:title/bos:content[@lang=$lang]"/></h1>
            <h2><xsl:value-of select="bos:subtitle/bos:content[@lang=$lang]"/></h2>
            <!--             <div width="400" style="text-align: left;"> -->
            <!--               <xsl:value-of select="bos:description/bos:content[@lang=$lang]"/> -->
            <!--             </div> -->
            <table width="400">
              <tr><td>
                  <xsl:value-of select="bos:description/bos:content[@lang=$lang]"/>
              </td></tr>
            </table>        
            <xsl:choose>
              <xsl:when test="bos:name='selling-area'">
                <p><a href="http://test.createrainforest.org/de/bestellung">Machen Sie mit!</a></p>
              </xsl:when>
              <xsl:otherwise>
                <br/><br/>
              </xsl:otherwise>
            </xsl:choose>
            <table>
              <tbody>
                <tr>
                  <td><xsl:apply-templates select="descendant::bos:image[1]"/></td>
                  <td><xsl:apply-templates select="descendant::bos:image[2]"/></td>
                  <td><xsl:apply-templates select="descendant::bos:image[3]"/></td>
                </tr>
                <tr>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[1]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[2]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[3]" mode="title"/></td>
                </tr>
                <tr>
                  <td><xsl:apply-templates select="descendant::bos:image[4]"/></td>
                  <td><xsl:apply-templates select="descendant::bos:image[5]"/></td>
                  <td><xsl:apply-templates select="descendant::bos:image[6]"/></td>
                </tr>
                <tr>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[4]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[5]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::bos:image[6]" mode="title"/></td>
                </tr>
              </tbody>
            </table>
          </td>
        </tr>
        <!-- this will later become a link that goes directly to poi info -->
        <tr><td align="center" colspan="3"><a href="{$server}/infosystem/en/satellitenkarte.htm">learn more</a></td></tr>
        <tr>
          <td style="width: 99px;" colspan="3" align="center" valign="middle">
            <font color="#999999">
              <a href="{$server}/en/index">create rainforest</a> | copyright
            </font>
          </td>
        </tr>
      </tbody>
      </table>
      <p/>  
  </xsl:template>
  
  <xsl:template match="bos:image">
    <xsl:param name="id" select="bos:id"/>
    <xsl:param name="aspect_ratio" select="bos:width div bos:height"/>
    <!-- <xsl:message>val:<xsl:value-of select="$aspect_ratio"/></xsl:message> -->
    <xsl:param name="w" select="$image_width"/>
    <xsl:param name="h" select="$aspect_ratio * $w"/>
    <img src="{$server}/image/{$id}" width="{$w}" height="{$h}"/>
  </xsl:template>

  <xsl:template match="bos:image" mode="title">
    <span style="font-size: small;"><xsl:value-of select="bos:title/bos:content[@lang=$lang]"/></span>
  </xsl:template>

</xsl:stylesheet>

