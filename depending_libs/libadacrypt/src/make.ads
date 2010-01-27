-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with crypto;
with crypto.asymmetric.dsa;
with crypto.asymmetric.prime_tables;
with crypto.asymmetric.rsa;
--with crypto.Symmetric.algorithm.sha1;
--with crypto.Symmetric.algorithm.sha256;
with crypto.Symmetric.algorithm.sha384;
--with crypto.Symmetric.algorithm.sha512;
--with crypto.Symmetric.algorithm.sha_utils;
--with crypto.Symmetric.algorithm.whirlpool;
--with crypto.symmetric.algorithm;
--with crypto.symmetric.algorithm.aes;
--with crypto.symmetric.algorithm.aes.oneway;
--with crypto.symmetric.algorithm.serpent;
--with crypto.symmetric.algorithm.serpent.oneway;
--with crypto.symmetric.algorithm.tripledes;
--with crypto.symmetric.algorithm.tripledes.oneway;
--with crypto.symmetric.algorithm.twofish;
--with crypto.symmetric.algorithm.twofish.oneway;
with crypto.symmetric.blockcipher;
with crypto.symmetric.blockcipher_aes128;
with crypto.symmetric.blockcipher_aes192;
with crypto.symmetric.blockcipher_aes256;
with crypto.symmetric.blockcipher_obsolete_tripledes;
with crypto.symmetric.blockcipher_serpent256;
with crypto.symmetric.blockcipher_tripledes;
with crypto.symmetric.blockcipher_twofish128;
with crypto.symmetric.blockcipher_twofish192;
with crypto.symmetric.blockcipher_twofish256;
with crypto.symmetric.mode.cbc;
with crypto.symmetric.mode.cfb;
with crypto.symmetric.mode.ctr;
with crypto.symmetric.mode.ofb;
with crypto.symmetric.mode.oneway_cfb;
with crypto.symmetric.mode.oneway_ctr;
with crypto.symmetric.mode.oneway_ofb;
with crypto.symmetric.oneway_blockcipher;
with crypto.symmetric.oneway_blockcipher_aes128;
with crypto.symmetric.oneway_blockcipher_aes192;
with crypto.symmetric.oneway_blockcipher_aes256;
with crypto.symmetric.oneway_blockcipher_serpent256;
with crypto.symmetric.oneway_blockcipher_sha1;
with crypto.symmetric.oneway_blockcipher_sha256;
with crypto.symmetric.oneway_blockcipher_sha384;
with crypto.symmetric.oneway_blockcipher_sha512;
with crypto.symmetric.oneway_blockcipher_whirlpool;
with crypto.symmetric.oneway_blockcipher_tripledes;
with crypto.symmetric.oneway_blockcipher_twofish128;
with crypto.symmetric.oneway_blockcipher_twofish192;
with crypto.symmetric.oneway_blockcipher_twofish256;
with crypto.symmetric.Oneway_Blockcipher_Obsolete_Tripledes;
with crypto.Hashfunction_Sha1;
with crypto.Hashfunction_Sha256;
with crypto.Hashfunction_Sha512;
with crypto.Hashfunction_Whirlpool;
with Crypto.Symmetric.MAC.RMAC;
with Crypto.Symmetric.MAC.HMAC_SHA1;
with Crypto.Symmetric.MAC.HMAC_SHA256;
with Crypto.Symmetric.MAC.HMAC_SHA512;
with Crypto.Symmetric.MAC.HMAC_Whirlpool;
with Crypto.Types.Elliptic_Curves.Zp;
with Crypto.Types.Elliptic_Curves.NSS_BF;
with Crypto.Certificate;


package make is
--  null;
end make;
