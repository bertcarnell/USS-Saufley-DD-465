<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2022 Robert Carnell -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes"/>
    <xsl:template match="/">
    <html>
        <head>
            <meta charset="utf-8"/>
            <meta name="description" content="USS Saufley War Reports"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <meta name="robots" content="noindex"/> <!-- Prevent page from being indexed -->
            <meta name="copyright" content="2022 Robert Carnell"/>
            
            <!-- US Navy Logo 2022 -->
            <link rel="shortcut icon" type="image/png" href="https://www.cnrc.navy.mil/eToolbox/assets/img/favicon.png"/>
            
            <!-- Latest compiled and minified CSS -->
            <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous"/>
            <!-- Latest compiled and minified JavaScript -->
            <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" integrity="sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" crossorigin="anonymous"></script>
            
            <!-- all paragraphs indented similarly -->
            <style>
              p {text-indent: 10pt;}
            </style>
            <title>USS Saufley War Reports</title>
        </head>
        <body>
            <!-- <div id="includeHeader"></div><script>$("#includeHeader").load("navbar1.html");</script> -->
        
            <!--<div class="container-fluid" style="padding:0">
            <img class="img-responsive" src="images/ReadingEagle19760329.PNG" alt="ReadingEagle" style="width:100%"/> 
            </div>-->
            
            <div class="jumbotron-fluid" style="padding-left:15px">
                <h1>USS Saufley DD-465</h1>
                <h3>War Reports</h3>
            </div>
            
            <div class="container-fluid">
                <!--<h4>Notes</h4>
                <ul>
                <li>Links to the <a href="https://news.google.com/newspapers" target="_blank" rel="noopener noreferrer">Google Newspaper Archive</a> sometimes take you to the wrong article. Please search the page near the highlighted article to find the correct one.  Not every article on a Google newspaper page can be linked.</li>
                <li>The articles on this web page are for my personal genealogical research.  Please follow all applicable copyright laws in your use of these articles.
                    <ul>
                    <li><a href="https://www.legalgenealogist.com/2012/09/12/copyright-and-the-obit/" target="_blank" rel="noopener noreferrer">Copyright and the obit</a></li>
                    <li><a href="https://www.legalgenealogist.com/2012/09/13/copyright-and-the-post-1963-obit/" target="_blank" rel="noopener noreferrer">Copyright and the Post 1963 obit</a></li>
                    <li><a href="http://blog.frontrunnerpro.com/obituary-pirates-what-you-should-know-about-your-obituaries-and-online-revenues/" target="_blank" rel="noopener noreferrer">Obituary Pirates</a></li>
                    <li><a href="https://www.archives.com/experts/macentee-thomas/copyright-and-genealogy.html" target="_blank" rel="noopener noreferrer">Copyright and Genealogy</a></li>
                    <li><a href="https://www.nolo.com/legal-encyclopedia/fair-use-rule-copyright-material-30100.html" target="_blank" rel="noopener noreferrer">Fair Use</a></li>
                    </ul>
                </li>
                </ul>
                <hr>-->
                <h4>Search</h4>
                <p>In order to search this page...</p>
                <ul>
                <li>On a desktop browser:  Use <code>[CTRL]-F</code> to open a text search on the page.</li>
                <li>Android Chrome browser:  Press the three vertical dots in the upper right, use <code>Find on page</code></li>
                </ul>
            </div>
            <hr/>
            <div id="report_zone">
                <xsl:for-each select="dataroot/source">
                    <!-- Sort the sources by date -->
                    <xsl:sort select="date"/>
                    <xsl:variable name="dateurl"><xsl:value-of select="url"/></xsl:variable>
                    <xsl:variable name="dateurlshort"><xsl:value-of select="substring($dateurl, 1, string-length($dateurl) - 8)"/></xsl:variable>
                    <h3><a href="{$dateurl}"><xsl:value-of select="date"/></a></h3>
                    <ul>
                        <xsl:for-each select="images/image">
                            <xsl:variable name="imagefilename"><xsl:value-of select="file"/></xsl:variable>
                            <!--<li><xsl:value-of select="concat($dateurlshort, $imagefilename)"/></li>-->
                            <xsl:variable name="fullfilename"><xsl:value-of select="concat($dateurlshort, $imagefilename)"/></xsl:variable>
                            <xsl:if test="items">
                                <xsl:for-each select="items/item">
                                    <li><a href="{$fullfilename}" target="_blank" rel="noopener noreferrer"><xsl:value-of select="date"/></a></li>
                                    <div><xsl:value-of disable-output-escaping="yes" select="description"/></div>
                                    <p></p>
                                </xsl:for-each>
                            </xsl:if>
                        </xsl:for-each>
                    </ul>
                    <!-- <div class="newspaper">
                        <xsl:value-of disable-output-escaping="yes" select="Transcription"/>
                    </div>
                    <xsl:if test="Translation">
                        <div class="newspaper">
                            <xsl:value-of disable-output-escaping="yes" select="Translation"/>
                        </div>
                    </xsl:if> -->
                </xsl:for-each>
            </div>
        </body>
    </html>
    </xsl:template>
</xsl:stylesheet>
