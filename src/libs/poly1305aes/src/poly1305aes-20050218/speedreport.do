# Public domain.

exec 2>&1
echo 'poly1305aes speedreport version 20050218'
echo ''
echo '% uname -a'
uname -a
echo '% echo "$CC"'
echo "$CC"
echo '% gcc --version'
gcc --version
echo '% cat /proc/cpuinfo'
cat /proc/cpuinfo
echo '% sysctl -a hw.model'
sysctl -a hw.model
echo '% /usr/sbin/psrinfo -v'
/usr/sbin/psrinfo -v
echo '% cat x86cpuid.out'
cat x86cpuid.out
echo '% cat poly1305aes.h poly1305.h aes.h cpucycles.h'
cat poly1305aes.h poly1305.h aes.h cpucycles.h
echo '% echo _____; ./poly1305aes-speed; echo _____'
echo _____; ./poly1305aes-speed; echo _____
echo '% ./test-poly1305aes | head -123456789 | tail -1'
./test-poly1305aes | head -123456789 | tail -1
