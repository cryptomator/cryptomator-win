<?xml version="1.0" encoding="utf-8"?>

<!-- TODO: It is not clear if the XSLT processor of heat supports version 3.0 of XSLT-->
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:wix="http://schemas.microsoft.com/wix/2006/wi">
  <xsl:output method="xml" indent="yes"/>
  <xsl:strip-space elements="*"/>

  <!-- filter executable (added in main script due to shortcut problem)-->
  <xsl:template match="wix:Component[descendant::wix:File[@Source[contains(.,'Cryptomator.exe')]]]"/>
  <!-- add include line to resolve variables-->
  <xsl:template match="wix:Wix/wix:Fragment[1]">
    <xsl:processing-instruction name="include">$(sys.CURRENTDIR)/properties.wxi</xsl:processing-instruction>
    <xsl:copy>  
        <xsl:apply-templates select="node()[not(wix:Wix)]"/>
    </xsl:copy>
  </xsl:template>    

  <!--identity template copies everything forward by default-->
  <xsl:template match="@*|node()">
      <xsl:copy>
          <xsl:apply-templates select="@*|node()"/>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>