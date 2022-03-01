<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/Update">
<html>
<head>
<style type="text/css">
table {
	font-family: sans-serif;
	font-size: 1rem;
	text-align: center;
	border-spacing: .5rem;
}
td {
	border: 2px solid #e6e6e6;
	border-radius: .5rem;
	padding: .75rem;
}
td:first-child {
	background-color: #e6e6e6;
}
</style>
<title>Version info: <xsl:value-of select="AppName"/></title>
</head>
<body>
<table>
	<tr>
		<td>Application name</td>
		<td><xsl:value-of select="AppName"/></td>
	</tr>
	<tr>
		<td>Application version</td>
		<td><xsl:value-of select="AppVersion/MajorVersion"/>.<xsl:value-of select="AppVersion/MinorVersion"/>.<xsl:value-of select="AppVersion/Release"/>.<xsl:value-of select="AppVersion/Build"/></td>
	</tr>
	<tr>
		<td>Download URL</td>
		<td><a><xsl:attribute name="href"><xsl:value-of select="DownloadURL"/></xsl:attribute><xsl:value-of select="DownloadURL"/></a></td>
	</tr>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>
