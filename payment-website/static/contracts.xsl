<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output method="html"/>
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>        
        <title>Samboja Lestari Donor List</title></head>
      <body>
        <table border="1">
          <thead>
            <tr><th>Name</th><th>ID</th><th>Country</th><th>Coordinate</th><th>qm</th></tr>
          </thead>
          <tbody>
            <xsl:apply-templates/>
          </tbody>
        </table>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="contract">
    <tr>
      <td><xsl:value-of select="@sponsor-name" /></td>
      <td align="right"><xsl:value-of select="@sponsor-id" /></td>
      <td align="right"><xsl:value-of select="@country" /></td>
      <td align="right"><xsl:value-of select="@coords" /></td>
      <td align="right"><xsl:value-of select="@sqm-count" /></td>
    </tr>
  </xsl:template>
</xsl:stylesheet>

      
