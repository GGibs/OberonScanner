/* Oberon-S Scanner Implementation - Graham Gibson */

#include "obcScanner.h"

const char* const resWrds[] =
{
  "MODULE", "BEGIN", "END", "CONST", "TYPE", "VAR",
  "PROCEDURE", "ARRAY", "OF", "RECORD", "EXIT",
  "RETURN", "IF", "THEN", "ELSIF", "ELSE", "WHILE",
  "DO", "REPEAT", "UNTIL", "FOR", "TO", "BY",
  "LOOP", "CASE", "OR", "DIV", "MOD"
};

const char* const symbols[] =
{
  "MODULEsym", "BEGINsym", "ENDsym", "CONSTsym", "TYPEsym", "VARsym",
  "PROCEDUREsym", "ARRAYsym", "OFsym", "RECORDsym", "EXITsym",
  "RETURNsym", "IFsym", "THENsym", "ELSIFsym", "ELSEsym", "WHILEsym",
  "DOsym", "REPEATsym", "UNTILsym", "FORsym", "TOsym", "BYsym",
  "LOOPsym", "CASEsym", "ORsym", "DIVsym", "MODsym", "UNKNOWN",
  "plus", "minus", "tilde", "equal", "pound", "lt", "le", "gt", "ge",
  "and", "Integer", "Ident", "mult", "per", "dotdot", "comma",
  "colon", "semic", "lbrac", "rbrac", "lparen", "rparen", "pipe", "assgn"
};

/* Buffers */
char lineBuffer[MAX_LINE_LEN];
char varBuffer[MAX_VAR_LEN];

/* Buffer Pointers */
unsigned int varBuffPtr = 0;
unsigned int lineBuffPtr = 0;
unsigned int prevLinePtr = 0; /* used in parser for error diagnostics */

static short int enable_output = 1; /* used for scanner directives ONLY at this time */
unsigned int curLine = 0; /* used for output and error messages */
int sym;
int endOfLine = 0;
short int EOFflag = 0;
char ch = '\0';

FILE* file;

void nextSym()
{
  varBuffPtr = 0;
  while (ch == '\0' && !EOFflag)
    newLine();
  /* Set sym to the appropriate token - if elseif else */
  isAlpha() ? getAlphaToken() : (isDigit() ? getDigitToken() : getMiscToken());
  prevLinePtr = lineBuffPtr;
  skipSep();
  if (sym == -1 && !EOFflag) /* If in a comment call nextSym until a valid symbol is found */
  {
    if (enable_output)
      printf("IN COMMENTS %*c\b -->\t%s\n", 7, ' ', varBuffer);
    nextSym();
  }
  else if (enable_output)
    printf("%-15s -->\t%s\n", symbols[sym], varBuffer);
}

void newLine()
{
  endOfLine = setLineBuffer();
  if (endOfLine == -1) /* Exceeded lineBuffer capacity */
    scanError(2);
  lineBuffPtr = 0;
  ++curLine;
  if (enable_output)
    printLine();
  ch = lineBuffer[lineBuffPtr++];
  skipSep();
}

int setLineBuffer()
{
  int i = 0;
  char c;
  while (i < MAX_LINE_LEN)
  {
    c = getc(file);
    if (c == '\n')
    {
      lineBuffer[i] = '\0'; /* leftover input ignored */
      c = getc(file);
      if (c == EOF)
        EOFflag = 1;
      ungetc(c, file); /* if not EOF need to put the char back */
      return i; /* used to track where the end of the line is */
    }
    else
      lineBuffer[i++] = c;
  }
  return -1; /* if the input line exceeds MAX_LINE_LEN */
}

void getAlphaToken()
{
  while (isAlpha() || isDigit())
    nextChar();
  if (varBuffPtr > MAX_VAR_LEN)
    scanError(3);
  varBuffer[varBuffPtr] = '\0'; /* if there is left over input, it's ignored */
  #if ENABLE_HASH
  struct resWrd* symStruct; /* use typedef? */
  symStruct = checkResWord(varBuffer, varBuffPtr);
  sym = symStruct ? symStruct->token : Ident; /* set sym to res word if found, otherwise ident */
  #else
  sym = checkResWord(varBuffer);
  #endif
}

void getDigitToken()
{
  while (ch == '0') /* skip leading zeroes */
    ch = lineBuffer[lineBuffPtr++];
  while (isDigit())
    nextChar();
  if (ch >= 'A' && ch <= 'F')
  {
    while (isHexChar())
      nextChar();
    ch != 'H' ? scanError(1) : nextChar(); /* ensure hex chars end in 'H' */
  }
  else if (ch == 'H') /* catch Integers like 10H */
  {
    if (varBuffPtr == 1) /* catch unnecessary hex specifiers like 1H */
      scanWarning(1);
    nextChar();
  }
  if (varBuffPtr > MAX_VAR_LEN)
    scanError(3);
  varBuffer[varBuffPtr] = '\0'; /* if there is left over input, it's ignored */
  sym = Integer;
}

void getMiscToken()
{
  switch(ch)
  {
    case '+':
      sym = PLUS;
      break;
    case '-':
      sym = MINUS;
      break;
    case '~':
      sym = TILDE;
      break;
    case '=':
      sym = EQUAL;
      break;
    case '#':
      sym = POUND;
      break;
    case '&':
      sym = AND;
      break;
    case '*':
      sym = MULT;
      break;
    case ',':
      sym = COMMA;
      break;
    case ';':
      sym = SEMIC;
      break;
    case '[':
      sym = LBRAC;
      break;
    case ']':
      sym = RBRAC;
      break;
    case '|':
      sym = PIPE;
      break;
    case '<':
      doubleToken(LT, LE, '=');
      break;
    case '>':
      doubleToken(GT, GE, '=');
      break;
    case ':':
      doubleToken(COLON, ASSGN, '=');
      break;
    case '.':
      doubleToken(PER, DOTDOT, '.');
      break;
    case '(':
      if (lineBuffer[lineBuffPtr] != '*')
        sym = LPAREN;
      else
      {
        sym = -1;
        do /* If sym == -1, comments have ended */
        {
          sym = inComment();
        } while (sym != -1);
      }
      break;
    case ')':
      sym = RPAREN;
      break;
    default:
      #if ERROR_HANDLING
      sym = UNKNOWN;
      scanWarning(0); /* Non halting Error or warning? */
      #else
      scanError(0);
      #endif
  }
  nextChar();
  varBuffer[varBuffPtr] = '\0';
}

int inComment()
{
  int ret = 0;
  if (sym == -1)
    checkDirective();
  commentSkip();
  commentNewLine();
  if (ch == '*')
    ret = commentCheckClose();
  else if (ch == '(')
    commentCheckOpen();
  return ret;
}

void checkDirective()
{
  if (lineBuffer[lineBuffPtr+1] == 's')
  {
    char c = lineBuffer[lineBuffPtr+2];
    if (c == '+')
    {
      enable_output = 1;
      printf("\t**Scanner Directive found - Scanner output enabled**\n");
    }
    else if (c == '-')
    {
      enable_output = 0;
      printf("\t**Scanner Directive found - Scanner output disabled**\n");
    }
  }
}

void commentSkip()
{
  do
  {
    if (enable_output)
      varBuffer[varBuffPtr++] = ch;
    ch = lineBuffer[lineBuffPtr++];
  } while (!(ch == '\0' || ch == '*' || ch == '('));
}

void commentNewLine()
{
  while (ch == '\0')
  {
    varBuffer[varBuffPtr] = '\0';
    if (enable_output)
      printf("IN COMMENTS %*c\b -->\t%s\n", 7, ' ', varBuffer);
    varBuffPtr = 0;
    newLine();
  }
}

int commentCheckClose()
{
  int ret = 0;
  if (lineBuffer[lineBuffPtr] == ')')
  {
    if (enable_output)
      varBuffer[varBuffPtr++] = ch;
    ch = lineBuffer[lineBuffPtr++];
    ret = -1;
  }
  return ret;
}

void commentCheckOpen()
{
  if (lineBuffer[lineBuffPtr] == '*')
  {
    if (enable_output)
      varBuffer[varBuffPtr++] = ch;
    ch = lineBuffer[lineBuffPtr++];
    do
    {
      sym = inComment();
    } while (sym != -1);
  }
}

#if !ENABLE_HASH
int checkResWord(const char* token)
{
  int i;
  for (i = 0; i < NUM_RES_WORDS; ++i)
  {
    if (cmpWords(token, resWrds[i]))
      return i;
  }
  return Ident;
}
#endif

void doubleToken(int t1, int t2, char ch)
{
  sym = t1;
  if (lineBuffer[lineBuffPtr] == ch)
  {
    sym = t2;
    nextChar();
  }
}

void scanError(int errNum)
{
  if (enable_output)
    printf("\nERROR ORIGINATING FROM THE SCANNER\n");
  switch(errNum)
  {
    case 0:
      printf("Error: Invalid token '%c' on line %d\n", ch, curLine);
      break;
    case 1:
      printf("Error on line %d. Expecting an 'H' but found: %c\n", curLine, lineBuffer[--lineBuffPtr]);
      ++lineBuffPtr;
      break;
    case 2:
      printf("Exceeded maximum line length of %d on line: %d\n", MAX_LINE_LEN, curLine);
      lineBuffPtr = MAX_LINE_LEN + 1;
      break;
    case 3:
      printf("Exceeded maximum variable length of %d on line: %d\n", MAX_VAR_LEN, curLine);
      break;
    case 4:
      printf("Confused - bailing out!\n");
      break;
  }
    printf("%s\n", lineBuffer);
    printf("%*c\b^\n", lineBuffPtr, ' ');
    exit(EXIT_FAILURE);
}

void scanWarning(int warningNum)
{
  static int warningCount = 0;
  if (warningCount > 10)
    scanError(4);
  switch(warningNum)
  {
    case 0:
      printf("Non fatal error: Invalid token '%c' on line %d\n", ch, curLine);
      ++warningCount;
      break;
    case 1:
      printf("Unnecessary hex base specification on line %d\n", curLine);
      break;
  }
    printf("%s\n", lineBuffer);
    printf("%*c\b^\n", lineBuffPtr, ' ');
}

void nextChar()
{
  varBuffer[varBuffPtr++] = ch;
  ch = lineBuffer[lineBuffPtr++];
}

void skipSep()
{
  while (ch == 32 || ch == 11 || ch == 13 || ch == 9)
    ch = lineBuffer[lineBuffPtr++];
}

int isHexChar()
{
  return (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'F');
}

int isAlpha()
{
  return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}

int isDigit()
{
  return (ch >= '0' && ch <= '9');
}

int cmpWords(const char* w1, const char* w2)
{
  const unsigned char* s1 = (const unsigned char *) w1;
  const unsigned char* s2 = (const unsigned char *) w2;
  unsigned char c1, c2; /* Will be comparing these 2 chars */
  int result = 0;

  do
  {
    c1 = (unsigned char)* s1++; /* s1 and s2 can be broken */
    c2 = (unsigned char)* s2++;
    if (c1 == '\0' && c2 == '\0')
    {
      result = 1; /* words are the same */
      break;
    }
  } while (c1 == c2);

  return result; /* words must be different, return 0 */
}

void printLine()
{
  (cmpWords(lineBuffer, "\0")) ? printf("%d: Empty Line\n", curLine)
                               : printf("%d: %s\n", curLine, lineBuffer);
}

#if ENABLE_HASH

#define MIN_WORD_LEN    2   /* 'MULTIPLE'     */
#define MAX_WORD_LEN    9   /* PROCEDURE      */
#define MIN_HASH_VALUE  0   /* TO (not used)  */
#define MAX_HASH_VALUE  27  /* DIV            */
#define NUM_HASH_VALS   24

static unsigned int hash(const char* word, unsigned int len)
{
  static const unsigned char hashVals[] =
  {
    21, 2, 6, 5, 1, 6, 13, 35, 18, 35, 35, 21,
    10, 9, 1, 1, 35, 2, 5, 1, 1, 13, 1, 35, 2
  };
  unsigned int iVal = len;
  unsigned int temp;

  switch (iVal)
  {
    default:
      temp = (unsigned char)word[2]-65;
      if (temp > NUM_HASH_VALS)
        return 0;
      iVal += hashVals[temp];
    case 2:
    case 1:
      temp = (unsigned char)word[0]-65;
      if (temp > NUM_HASH_VALS)
        return 0;
      iVal += hashVals[temp];
      break;
  }
  temp = (unsigned char)word[len-1]-65;
  if (temp > NUM_HASH_VALS)
    return 0;
  iVal += hashVals[temp];
  return (iVal == 34) ? iVal -7 : iVal - 4;
}

struct resWrd *checkResWord(const char* word, unsigned int len)
{
  static struct resWrd wordlist[] =
  {
    {"TO", TOsym},
    {"OR", ORsym},
    {"BY", BYsym},
    {"TYPE", TYPEsym},
    {"DO", DOsym},
    {"OF", OFsym},
    {"REPEAT", REPEATsym},
    {"ELSE", ELSEsym},
    {"PROCEDURE", PROCEDUREsym},
    {"FOR", FORsym},
    {"END", ENDsym},
    {"THEN", THENsym},
    {"CASE", CASEsym},
    {"ELSIF", ELSIFsym},
    {"RETURN", RETURNsym},
    {"RECORD", RECORDsym},
    {"VAR", VARsym},
    {"CONST", CONSTsym},
    {"MODULE", MODULEsym},
    {"MOD", MODsym},
    {"EXIT", EXITsym},
    {"WHILE", WHILEsym},
    {"IF", IFsym},
    {"LOOP", LOOPsym},
    {"UNTIL", UNTILsym},
    {"BEGIN", BEGINsym},
    {"ARRAY", ARRAYsym},
    {"DIV", DIVsym}
  };

  if (len <= MAX_WORD_LEN && len >= MIN_WORD_LEN) /* if not true, not a reserved word */
  {
    unsigned int key = hash (word, len);
    struct resWrd *resWrdStruct;
    if (key <= MAX_HASH_VALUE)
    {
      resWrdStruct = &wordlist[key];
      goto compare; /* +1 (-1) for use of goto */
    }
    return 0; /* not a reserved word */

compare: /* Verifying it is a match */
    {
      const char *s = resWrdStruct->name;
      if (*word == *s && cmpWords(word+1, s+1))
        return resWrdStruct;
    }
  }
  return 0; /* Else it is not a match */
}
#endif

int main(int argc, char** argv)
{
  /* Get file from stdin or input arg if there is one*/
  file = (argc < 2) ? stdin : fopen(argv[1], "r");
  if (!file)
  {
    perror(argv[1]);
    exit(EXIT_FAILURE);
  }
  while (!EOFflag || (unsigned int)endOfLine+1 != lineBuffPtr)
    nextSym();
  fclose(file);
  exit(EXIT_SUCCESS);
}