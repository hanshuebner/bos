<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
  
  <!-- params passed in from outside -->
  <xsl:param name="host" select="localhost"/>
  <xsl:param name="lang" select="en"/>
  <!-- other params -->
  <xsl:param name="image_width" select="120"/>
  
  <xsl:template match="poi">
    <xsl:param name="poi_xml_link" select="concat('http://', $host, '/poi-xml/', @id, '?lang=', $lang)"/>
    <table width="500" style="background-color: rgb(186, 186, 186);"
           border="0" cellpadding="5" cellspacing="0">
      <tbody>
        <tr>
          <td colspan="3" style="width: 99px; text-align: left;">             
            <img src="http://{$host}/images/header_ganzneu.gif" alt="Create Rainforest Banner / BOS Logo" width="400"/>
          </td>
        </tr>        
        <tr>
          <td style="width: 100px;">
            <h1><xsl:value-of select="@title"/></h1>
            <h2><xsl:value-of select="@subtitle"/></h2>
            <table width="400">
              <tr><td>
                  <xsl:value-of select="description"/>
              </td></tr>
            </table>        
            <xsl:choose>
              <xsl:when test="@id='1983023'">
                <p><a href="http://{$host}/de/bestellung">Machen Sie mit!</a></p>
              </xsl:when>
              <xsl:otherwise>
                <br/><br/>
              </xsl:otherwise>
            </xsl:choose>
            <table>
              <tbody>
                <tr>
                  <td><xsl:apply-templates select="descendant::image[1]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                  <td><xsl:apply-templates select="descendant::image[2]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                  <td><xsl:apply-templates select="descendant::image[3]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                </tr>
                <tr>
                  <td valign="top"><xsl:apply-templates select="descendant::image[1]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::image[2]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::image[3]" mode="title"/></td>
                </tr>
                <tr>
                  <td><xsl:apply-templates select="descendant::image[4]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                  <td><xsl:apply-templates select="descendant::image[5]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                  <td><xsl:apply-templates select="descendant::image[6]">
                      <xsl:with-param name="link" select="$poi_xml_link"/>
                  </xsl:apply-templates></td>
                </tr>
                <tr>
                  <td valign="top"><xsl:apply-templates select="descendant::image[4]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::image[5]" mode="title"/></td>
                  <td valign="top"><xsl:apply-templates select="descendant::image[6]" mode="title"/></td>
                </tr>
              </tbody>
            </table>
          </td>
        </tr>
        <tr><td align="center" colspan="3"><a href="{$poi_xml_link}">learn more</a></td></tr>
        <tr>
          <td style="width: 99px;" colspan="3" align="center" valign="middle">
            <font color="#999999">
              <a href="http://{$host}/en/index">create rainforest</a> | copyright
            </font>
          </td>
        </tr>
      </tbody>
      </table>
      <p/>  
  </xsl:template>
  
  <xsl:template match="image">
    <xsl:param name="id" select="@id"/>
    <xsl:param name="aspect_ratio" select="width div height"/>
    <!-- <xsl:message>val:<xsl:value-of select="$aspect_ratio"/></xsl:message> -->
    <xsl:param name="w" select="$image_width"/>
    <xsl:param name="h" select="$aspect_ratio * $w"/>
    <a href="{$link}"><img src="http://{$host}/image/{$id}/thumbnail,,{$w},{$h}" width="{$w}" height="{$h}"/></a>
  </xsl:template>

  <xsl:template match="image" mode="title">
    <span style="font-size: small;"><xsl:value-of select="@title"/></span>
  </xsl:template>

</xsl:stylesheet>

