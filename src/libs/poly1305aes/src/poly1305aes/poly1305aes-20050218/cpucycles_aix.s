.toc
.csect cpucycles_aix[DS]
.globl cpucycles_aix
cpucycles_aix:
.long .cpucycles_aix
.long TOC[tc0]
.long 0
.csect .text[PR]
.globl .cpucycles_aix
.cpucycles_aix:
mftbu 3
mftb 4
mftbu 5
cmpw 3,5
bne .cpucycles_aix
blr
