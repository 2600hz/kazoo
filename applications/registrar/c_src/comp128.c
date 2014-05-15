/* An implementation of the GSM A3A8 algorithm.  (Specifically, COMP128.) */
/* Copyright 1998, Marc Briceno, Ian Goldberg, and David Wagner.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of the authors nor the names of the contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Coded in C merely because C is a much more precise, concise form of
 * expression for these purposes.  See Judge Patel if you have any problems
 * with this...
 * Of course, it's only authentication, so it should be exportable for the
 * usual boring reasons.
 */

typedef unsigned char Byte;

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "erl_nif.h"

/** Version number for load_info. */
#define NIF_LOAD_INFO (101)

/* #define TEST */

/*
 * rand[0..15]: the challenge from the base station
 * key[0..15]: the SIM's A3/A8 long-term key Ki
 * simoutput[0..11]: what you'd get back if you fed rand and key to a real
 * SIM.
 *
 *   The GSM spec states that simoutput[0..3] is SRES,
 *   and simoutput[4..11] is Kc (the A5 session key).
 *   (See GSM 11.11, Section 8.16.  See also the leaked document
 *   referenced below.)
 *   Note that Kc is bits 74..127 of the COMP128 output, followed by 10
 *   zeros.
 *   In other words, A5 is keyed with only 54 bits of entropy. This
 *   represents a deliberate weakening of the key used for voice privacy
 *   by a factor of over 1000.
 *
 * Verified with a Pacific Bell Schlumberger SIM.  Your mileage may vary.
 *
 * Marc Briceno <marc@scard.org>, Ian Goldberg <iang@cs.berkeley.edu>,
 * and David Wagner <daw@cs.berkeley.edu>
 */

void A3A8(/* in */ Byte rand[16], /* in */ Byte key[16],
	/* out */ Byte simoutput[12]);

/* The compression tables. */
static const Byte table_0[512] = {
        102,177,186,162,  2,156,112, 75, 55, 25,  8, 12,251,193,246,188,
        109,213,151, 53, 42, 79,191,115,233,242,164,223,209,148,108,161,
        252, 37,244, 47, 64,211,  6,237,185,160,139,113, 76,138, 59, 70,
         67, 26, 13,157, 63,179,221, 30,214, 36,166, 69,152,124,207,116,
        247,194, 41, 84, 71,  1, 49, 14, 95, 35,169, 21, 96, 78,215,225,
        182,243, 28, 92,201,118,  4, 74,248,128, 17, 11,146,132,245, 48,
        149, 90,120, 39, 87,230,106,232,175, 19,126,190,202,141,137,176,
        250, 27,101, 40,219,227, 58, 20, 51,178, 98,216,140, 22, 32,121,
         61,103,203, 72, 29,110, 85,212,180,204,150,183, 15, 66,172,196,
         56,197,158,  0,100, 45,153,  7,144,222,163,167, 60,135,210,231,
        174,165, 38,249,224, 34,220,229,217,208,241, 68,206,189,125,255,
        239, 54,168, 89,123,122, 73,145,117,234,143, 99,129,200,192, 82,
        104,170,136,235, 93, 81,205,173,236, 94,105, 52, 46,228,198,  5,
         57,254, 97,155,142,133,199,171,187, 50, 65,181,127,107,147,226,
        184,218,131, 33, 77, 86, 31, 44, 88, 62,238, 18, 24, 43,154, 23,
         80,159,134,111,  9,114,  3, 91, 16,130, 83, 10,195,240,253,119,
        177,102,162,186,156,  2, 75,112, 25, 55, 12,  8,193,251,188,246,
        213,109, 53,151, 79, 42,115,191,242,233,223,164,148,209,161,108,
         37,252, 47,244,211, 64,237,  6,160,185,113,139,138, 76, 70, 59,
         26, 67,157, 13,179, 63, 30,221, 36,214, 69,166,124,152,116,207,
        194,247, 84, 41,  1, 71, 14, 49, 35, 95, 21,169, 78, 96,225,215,
        243,182, 92, 28,118,201, 74,  4,128,248, 11, 17,132,146, 48,245,
         90,149, 39,120,230, 87,232,106, 19,175,190,126,141,202,176,137,
         27,250, 40,101,227,219, 20, 58,178, 51,216, 98, 22,140,121, 32,
        103, 61, 72,203,110, 29,212, 85,204,180,183,150, 66, 15,196,172,
        197, 56,  0,158, 45,100,  7,153,222,144,167,163,135, 60,231,210,
        165,174,249, 38, 34,224,229,220,208,217, 68,241,189,206,255,125,
         54,239, 89,168,122,123,145, 73,234,117, 99,143,200,129, 82,192,
        170,104,235,136, 81, 93,173,205, 94,236, 52,105,228, 46,  5,198,
        254, 57,155, 97,133,142,171,199, 50,187,181, 65,107,127,226,147,
        218,184, 33,131, 86, 77, 44, 31, 62, 88, 18,238, 43, 24, 23,154,
        159, 80,111,134,114,  9, 91,  3,130, 16, 10, 83,240,195,119,253
    }, table_1[256] = {
         19, 11, 80,114, 43,  1, 69, 94, 39, 18,127,117, 97,  3, 85, 43,
         27,124, 70, 83, 47, 71, 63, 10, 47, 89, 79,  4, 14, 59, 11,  5,
         35,107,103, 68, 21, 86, 36, 91, 85,126, 32, 50,109, 94,120,  6,
         53, 79, 28, 45, 99, 95, 41, 34, 88, 68, 93, 55,110,125,105, 20,
         90, 80, 76, 96, 23, 60, 89, 64,121, 56, 14, 74,101,  8, 19, 78,
         76, 66,104, 46,111, 50, 32,  3, 39,  0, 58, 25, 92, 22, 18, 51,
         57, 65,119,116, 22,109,  7, 86, 59, 93, 62,110, 78, 99, 77, 67,
         12,113, 87, 98,102,  5, 88, 33, 38, 56, 23,  8, 75, 45, 13, 75,
         95, 63, 28, 49,123,120, 20,112, 44, 30, 15, 98,106,  2,103, 29,
         82,107, 42,124, 24, 30, 41, 16,108,100,117, 40, 73, 40,  7,114,
         82,115, 36,112, 12,102,100, 84, 92, 48, 72, 97,  9, 54, 55, 74,
        113,123, 17, 26, 53, 58,  4,  9, 69,122, 21,118, 42, 60, 27, 73,
        118,125, 34, 15, 65,115, 84, 64, 62, 81, 70,  1, 24,111,121, 83,
        104, 81, 49,127, 48,105, 31, 10,  6, 91, 87, 37, 16, 54,116,126,
         31, 38, 13,  0, 72,106, 77, 61, 26, 67, 46, 29, 96, 37, 61, 52,
        101, 17, 44,108, 71, 52, 66, 57, 33, 51, 25, 90,  2,119,122, 35
    }, table_2[128] = {
         52, 50, 44,  6, 21, 49, 41, 59, 39, 51, 25, 32, 51, 47, 52, 43,
         37,  4, 40, 34, 61, 12, 28,  4, 58, 23,  8, 15, 12, 22,  9, 18,
         55, 10, 33, 35, 50,  1, 43,  3, 57, 13, 62, 14,  7, 42, 44, 59,
         62, 57, 27,  6,  8, 31, 26, 54, 41, 22, 45, 20, 39,  3, 16, 56,
         48,  2, 21, 28, 36, 42, 60, 33, 34, 18,  0, 11, 24, 10, 17, 61,
         29, 14, 45, 26, 55, 46, 11, 17, 54, 46,  9, 24, 30, 60, 32,  0,
         20, 38,  2, 30, 58, 35,  1, 16, 56, 40, 23, 48, 13, 19, 19, 27,
         31, 53, 47, 38, 63, 15, 49,  5, 37, 53, 25, 36, 63, 29,  5,  7
    }, table_3[64] = {
          1,  5, 29,  6, 25,  1, 18, 23, 17, 19,  0,  9, 24, 25,  6, 31,
         28, 20, 24, 30,  4, 27,  3, 13, 15, 16, 14, 18,  4,  3,  8,  9,
         20,  0, 12, 26, 21,  8, 28,  2, 29,  2, 15,  7, 11, 22, 14, 10,
         17, 21, 12, 30, 26, 27, 16, 31, 11,  7, 13, 23, 10,  5, 22, 19
    }, table_4[32] = {
         15, 12, 10,  4,  1, 14, 11,  7,  5,  0, 14,  7,  1,  2, 13,  8,
         10,  3,  4,  9,  6,  0,  3,  2,  5,  6,  8,  9, 11, 13, 15, 12
    }, *table[5] = { table_0, table_1, table_2, table_3, table_4 };

/*
 * This code derived from a leaked document from the GSM standards.
 * Some missing pieces were filled in by reverse-engineering a working SIM.
 * We have verified that this is the correct COMP128 algorithm.
 *
 * The first page of the document identifies it as
 * 	_Technical Information: GSM System Security Study_.
 * 	10-1617-01, 10th June 1988.
 * The bottom of the title page is marked
 * 	Racal Research Ltd.
 * 	Worton Drive, Worton Grange Industrial Estate,
 * 	Reading, Berks. RG2 0SB, England.
 * 	Telephone: Reading (0734) 868601   Telex: 847152
 * The relevant bits are in Part I, Section 20 (pages 66--67).  Enjoy!
 *
 * Note: There are three typos in the spec (discovered by
 * reverse-engineering).
 * First, "z = (2 * x[n] + x[n]) mod 2^(9-j)" should clearly read
 * "z = (2 * x[m] + x[n]) mod 2^(9-j)".
 * Second, the "k" loop in the "Form bits from bytes" section is severely
 * botched: the k index should run only from 0 to 3, and clearly the range
 * on "the (8-k)th bit of byte j" is also off (should be 0..7, not 1..8,
 * to be consistent with the subsequent section).
 * Third, SRES is taken from the first 8 nibbles of x[], not the last 8 as
 * claimed in the document.  (And the document doesn't specify how Kc is
 * derived, but that was also easily discovered with reverse engineering.)
 * All of these typos have been corrected in the following code.
 */

void A3A8(/* in */ Byte rand[16], /* in */ Byte key[16],
	/* out */ Byte simoutput[12])
{
	Byte x[32], bit[128];
	int i, j, k, l, m, n, y, z, next_bit;

	/* ( Load RAND into last 16 bytes of input ) */
	for (i=16; i<32; i++)
		x[i] = rand[i-16];

	/* ( Loop eight times ) */
	for (i=1; i<9; i++) {
		/* ( Load key into first 16 bytes of input ) */
		for (j=0; j<16; j++)
			x[j] = key[j];
		/* ( Perform substitutions ) */
		for (j=0; j<5; j++)
			for (k=0; k<(1<<j); k++)
				for (l=0; l<(1<<(4-j)); l++) {
					m = l + k*(1<<(5-j));
					n = m + (1<<(4-j));
					y = (x[m]+2*x[n]) % (1<<(9-j));
					z = (2*x[m]+x[n]) % (1<<(9-j));
					x[m] = table[j][y];
					x[n] = table[j][z];
				}
		/* ( Form bits from bytes ) */
		for (j=0; j<32; j++)
			for (k=0; k<4; k++)
				bit[4*j+k] = (x[j]>>(3-k)) & 1;
		/* ( Permutation but not on the last loop ) */
		if (i < 8)
			for (j=0; j<16; j++) {
				x[j+16] = 0;
				for (k=0; k<8; k++) {
					next_bit = ((8*j + k)*17) % 128;
					x[j+16] |= bit[next_bit] << (7-k);
				}
			}
	}

	/*
	 * ( At this stage the vector x[] consists of 32 nibbles.
	 *   The first 8 of these are taken as the output SRES. )
	 */

	/* The remainder of the code is not given explicitly in the
	 * standard, but was derived by reverse-engineering.
	 */

	for (i=0; i<4; i++)
		simoutput[i] = (x[2*i]<<4) | x[2*i+1];
	for (i=0; i<6; i++)
		simoutput[4+i] = (x[2*i+18]<<6) | (x[2*i+18+1]<<2)
				| (x[2*i+18+2]>>2);
	simoutput[4+6] = (x[2*6+18]<<6) | (x[2*6+18+1]<<2);
	simoutput[4+7] = 0;
}

/*
int hextoint(char x)
{
	x = toupper(x);
	if (x >= 'A' && x <= 'F')
		return x-'A'+10;
	else if (x >= '0' && x <= '9')
		return x-'0';
	fprintf(stderr, "bad input.\n");
	exit(1);
}

int main(int argc, char **argv)
{
	Byte key[16], rand[16], simoutput[12];
	int i;

	if (argc != 3 || strlen(argv[1]) != 34 || strlen(argv[2]) != 34
			|| strncmp(argv[1], "0x", 2) != 0
			|| strncmp(argv[2], "0x", 2) != 0) {
		fprintf(stderr, "Usage: %s 0x<32-digit-key> 0x<32-digit-rand>\n", argv[0]);
		printf("error\n");
		exit(1);
	}

	for (i=0; i<16; i++)
		key[i] = (hextoint(argv[1][2*i+2])<<4)
			| hextoint(argv[1][2*i+3]);
	for (i=0; i<16; i++)
		rand[i] = (hextoint(argv[2][2*i+2])<<4)
			 | hextoint(argv[2][2*i+3]);
	A3A8(rand, key, simoutput);
	for (i=0; i<12; i++)
		printf("%02X", simoutput[i]);
	printf("\n");
	return 0;
}
*/


/**
 * NIF code of gen_rand_all/1.
 * Also a wrapper of gen_rand_all().
 * @param env ErlNifEnv pointer for the calling process.
 * @param argc Erlang function arity.
 * @param argv ERL_NIF_TERM pointers for the arguments.
 */
static ERL_NIF_TERM
NIF_A3A8(ErlNifEnv *env, int argc,
		      const ERL_NIF_TERM argv[])
{
    ErlNifBinary rand;
    ErlNifBinary key;
    ERL_NIF_TERM r;
    Byte* out;

    if (!enif_inspect_binary(env, argv[0], &rand)
        || rand.size != 16) {
	return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &key)
        || key.size != 16) {
	return enif_make_badarg(env);
    }

    /* make a new binary object first */
    out = (Byte *) enif_make_new_binary(env, 12, &r);
    /* copy the original data first before manipulating */
    /* the new (mutable) q has the new random data */
    A3A8(rand.data, key.data, out);

    return r;
}


/** Number of users of this dynamic library. */
static int lib_refc = 0;

/** An Erlang atom container. */
static ERL_NIF_TERM atom_error;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error1;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error2;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error3;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error_sfmt_nomem;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_ok;


/**
 * Checks the version number of the load info from Erlang.
 * See is_ok_load_info() in c_src/crypto.c of the crypto module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int check_load_info(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    int i;

    /* check the version number of the load info */
    return enif_get_int(env,load_info,&i) &&
	i == NIF_LOAD_INFO;
}

/**
 * Loads NIF module and defines Erlang atoms.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
	return -1;
    }

    /* initializing atoms */
    atom_error = enif_make_atom(env,"error");
    atom_error1 = enif_make_atom(env,"error1");
    atom_error2 = enif_make_atom(env,"error2");
    atom_error3 = enif_make_atom(env,"error3");
    atom_error_sfmt_nomem = enif_make_atom(env,"error_sfmt_nomem");
    atom_ok = enif_make_atom(env,"ok");

    *priv_data = NULL;

    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Reloads NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    /* Don't know how to do this */
    if (*priv_data != NULL) {
	return -1;
    }

    /* No support for real library upgrade.
       The tricky thing is to know
       when to (re)set the callbacks for allocation and locking. */
    if (lib_refc == 0) {
	return -2;
    }

    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
	return -1;
    }

    return 0;
}

/**
 * Upgrades NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param old_priv_data pointing the private data given from the last calls of load() or reload().
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    int i;

    /* Don't know how to do this */
    if (*old_priv_data != NULL) {
	return -1;
    }

    /* reloading first */
    i = reload(env, priv_data, load_info);

    /* return the error code from reload() if necessary */
    if (0 != i) {
	return i;
    }

    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Unloads NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 */
static void unload(ErlNifEnv* env, void* priv_data)
{
    /* this is yet a skeleton code */
    /* perform resource unlock and deallocation here,
       but in this module no resource are retained
       outside of the scope of each function */
    if (--lib_refc <= 0) {
	/* do nothing */
    }
    /*else NIF library still used by other (new) module code */
}

/** Function list passed to the Erlang BEAM for this NIF. */
static ErlNifFunc nif_funcs[] = {
    {"a3a8", 2, NIF_A3A8}
};

/* Function call macro to initialize NIF. */
ERL_NIF_INIT(registrar_crypto, nif_funcs, load, reload, upgrade, unload)

