# poly1305aes Makefile version 20050218
# D. J. Bernstein
# Public domain.


# Test programs:

default: test-aes test-poly1305aes poly1305aes-speed

speedreport: speedreport.do \
poly1305aes.h poly1305.h aes.h cpucycles.h \
poly1305aes-speed test-poly1305aes
	sh speedreport.do > speedreport

test-aes: test-aes.o poly1305aes.a
	$(CC) -o test-aes test-aes.o poly1305aes.a

test-aes.o: test-aes.c \
aes.h \
aes_aix.h \
aes_athlon.h \
aes_big.h \
aes_macos.h \
aes_ppro.h \
aes_sparc.h
	$(CC) -c test-aes.c

test-poly1305aes: test-poly1305aes.o poly1305aes.a
	$(CC) -o test-poly1305aes test-poly1305aes.o poly1305aes.a

test-poly1305aes.o: test-poly1305aes.c \
poly1305aes.h \
poly1305aes_53.h \
poly1305aes_aix.h \
poly1305aes_athlon.h \
poly1305aes_macos.h \
poly1305aes_ppro.h \
poly1305aes_sparc.h
	$(CC) -c test-poly1305aes.c

poly1305aes-speed: poly1305aes-speed.o poly1305aes.a cpucycles.a
	$(CC) -o poly1305aes-speed poly1305aes-speed.o poly1305aes.a cpucycles.a

poly1305aes-speed.o: poly1305aes-speed.c \
poly1305aes.h \
poly1305aes_53.h \
poly1305aes_aix.h \
poly1305aes_athlon.h \
poly1305aes_macos.h \
poly1305aes_ppro.h \
poly1305aes_sparc.h \
cpucycles.h \
cpucycles_aix.h \
cpucycles_athlon.h \
cpucycles_macos.h \
cpucycles_ppro.h \
cpucycles_sparc.h \
cpucycles_zero.h
	$(CC) -c poly1305aes-speed.c

cpucycles.h: poly1305aes.impl \
cpucycles.h.do
	sh -e cpucycles.h.do > cpucycles.h.new
	mv cpucycles.h.new cpucycles.h

cpucycles.a: poly1305aes.impl \
cpucycles.a.do \
cpucycles_aix.h \
cpucycles_aix.s \
cpucycles_athlon.h \
cpucycles_athlon.s \
cpucycles_macos.h \
cpucycles_macos.s \
cpucycles_ppro.h \
cpucycles_ppro.s \
cpucycles_sparc.h \
cpucycles_sparc.s \
cpucycles_zero.c \
cpucycles_zero.h
	sh -e cpucycles.a.do $(CC) > cpucycles.a.new
	mv cpucycles.a.new cpucycles.a


# The poly1305aes library:

poly1305aes: poly1305aes.a poly1305aes.h poly1305.h aes.h

poly1305aes.h: poly1305aes.impl \
poly1305aes.h.do
	sh -e poly1305aes.h.do > poly1305aes.h.new
	mv poly1305aes.h.new poly1305aes.h

poly1305.h: poly1305aes.impl \
poly1305.h.do
	sh -e poly1305.h.do > poly1305.h.new
	mv poly1305.h.new poly1305.h

aes.h: poly1305aes.impl \
aes.h.do
	sh -e aes.h.do > aes.h.new
	mv aes.h.new aes.h

poly1305aes.a: poly1305aes.impl \
poly1305aes.a.do \
aes_aix.h \
aes_aix.s \
aes_aix_constants.s \
aes_athlon.h \
aes_athlon.s \
aes_athlon_constants.s \
aes_big.c \
aes_big.h \
aes_big_constants.c \
aes_macos.h \
aes_macos.s \
aes_macos_constants.s \
aes_ppro.h \
aes_ppro.s \
aes_ppro_constants.s \
aes_sparc.h \
aes_sparc.s \
aes_sparc_constants.c \
poly1305_53.c \
poly1305_53.h \
poly1305_53_constants.c \
poly1305_aix.h \
poly1305_aix.s \
poly1305_aix_constants.c \
poly1305_athlon.h \
poly1305_athlon.s \
poly1305_athlon_constants.s \
poly1305_macos.h \
poly1305_macos.s \
poly1305_macos_constants.s \
poly1305_ppro.h \
poly1305_ppro.s \
poly1305_ppro_constants.s \
poly1305_sparc.h \
poly1305_sparc.s \
poly1305_sparc_constants.c \
poly1305aes_53.h \
poly1305aes_53_authenticate.c \
poly1305aes_53_clamp.c \
poly1305aes_53_isequal.c \
poly1305aes_53_verify.c \
poly1305aes_aix.h \
poly1305aes_aix_authenticate.c \
poly1305aes_aix_clamp.c \
poly1305aes_aix_isequal.s \
poly1305aes_aix_verify.c \
poly1305aes_athlon.h \
poly1305aes_athlon_authenticate.c \
poly1305aes_athlon_clamp.c \
poly1305aes_athlon_isequal.s \
poly1305aes_athlon_verify.c \
poly1305aes_macos.h \
poly1305aes_macos_authenticate.c \
poly1305aes_macos_clamp.c \
poly1305aes_macos_isequal.s \
poly1305aes_macos_verify.c \
poly1305aes_ppro.h \
poly1305aes_ppro_authenticate.c \
poly1305aes_ppro_clamp.c \
poly1305aes_ppro_isequal.s \
poly1305aes_ppro_verify.c \
poly1305aes_sparc.h \
poly1305aes_sparc_authenticate.c \
poly1305aes_sparc_clamp.s \
poly1305aes_sparc_fsr.s \
poly1305aes_sparc_isequal.s \
poly1305aes_sparc_verify.c
	sh -e poly1305aes.a.do $(CC) > poly1305aes.a.new
	mv poly1305aes.a.new poly1305aes.a

poly1305aes.impl: \
poly1305aes.impl.do \
x86cpuid.c \
poly1305aes.impl.check.c \
aes_aix.h \
aes_aix.s \
aes_aix_constants.s \
aes_athlon.h \
aes_athlon.s \
aes_athlon_constants.s \
aes_big.c \
aes_big.h \
aes_big_constants.c \
aes_macos.h \
aes_macos.s \
aes_macos_constants.s \
aes_ppro.h \
aes_ppro.s \
aes_ppro_constants.s \
aes_sparc.h \
aes_sparc.s \
aes_sparc_constants.c \
poly1305_53.c \
poly1305_53.h \
poly1305_53_constants.c \
poly1305_aix.h \
poly1305_aix.s \
poly1305_aix_constants.c \
poly1305_athlon.h \
poly1305_athlon.s \
poly1305_athlon_constants.s \
poly1305_macos.h \
poly1305_macos.s \
poly1305_macos_constants.s \
poly1305_ppro.h \
poly1305_ppro.s \
poly1305_ppro_constants.s \
poly1305_sparc.h \
poly1305_sparc.s \
poly1305_sparc_constants.c \
poly1305aes_53.h \
poly1305aes_53_authenticate.c \
poly1305aes_53_clamp.c \
poly1305aes_53_isequal.c \
poly1305aes_53_verify.c \
poly1305aes_aix.h \
poly1305aes_aix_authenticate.c \
poly1305aes_aix_clamp.c \
poly1305aes_aix_isequal.s \
poly1305aes_aix_verify.c \
poly1305aes_athlon.h \
poly1305aes_athlon_authenticate.c \
poly1305aes_athlon_clamp.c \
poly1305aes_athlon_isequal.s \
poly1305aes_athlon_verify.c \
poly1305aes_macos.h \
poly1305aes_macos_authenticate.c \
poly1305aes_macos_clamp.c \
poly1305aes_macos_isequal.s \
poly1305aes_macos_verify.c \
poly1305aes_ppro.h \
poly1305aes_ppro_authenticate.c \
poly1305aes_ppro_clamp.c \
poly1305aes_ppro_isequal.s \
poly1305aes_ppro_verify.c \
poly1305aes_sparc.h \
poly1305aes_sparc_authenticate.c \
poly1305aes_sparc_clamp.s \
poly1305aes_sparc_fsr.s \
poly1305aes_sparc_isequal.s \
poly1305aes_sparc_verify.c
	sh -e poly1305aes.impl.do $(CC) > poly1305aes.impl.new
	mv poly1305aes.impl.new poly1305aes.impl
