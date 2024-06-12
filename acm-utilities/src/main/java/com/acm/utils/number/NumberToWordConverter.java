/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.number;

/**
 * the {@link NumberToWordConverter} class.
 *
 * @author MoezMhiri
 * @since 1.0.1
 */
public class NumberToWordConverter implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5735327058520076878L;

	/** The Constant UNITS_EN. */
	private static final String[] UNITS_EN = {"Zero", "One", "Two", "Three", "Four", "Five", "Six",
			"Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen",
			"Sixteen", "Seventeen", "Eighteen", "Nineteen"};
	/** The Constant TENS_EN. */
	private static final String[] TENS_EN =
			{"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"};

	/** The Constant UNITS_FR. */
	private static final String[] UNITS_FR = {"Zero", "Un", "Deux", "Trois", "Quatre", "Cinq",
			"Six", "Sept", "Huit", "Neuf", "Dix", "Onze", "Douze", "Treize", "Quatorze", "Quinze",
			"Seize", "Dix-sept", "Dix-huit", "Dix-neuf"};
	/** The Constant TENS_FR. */
	private static final String[] TENS_FR = {"", "", "Vingt", "Trente", "Quarante", "Cinquante",
			"Soixante", "Soixante-dix", "Quatre-vingt", "Quatre-vingt-dix"};

	/** The Constant WORD_100_AN. */
	public static final String WORD_100_EN = " Hundred";

	/** The Constant WORD_AND_AN. */
	public static final String WORD_AND_EN = " and ";

	/** The Constant WORD_1000_AN. */
	public static final String WORD_1000_EN = " Thousand ";

	/** The Constant WORD_MILLION_AN. */
	public static final String WORD_MILLION_EN = " Million ";

	/** The Constant WORD_100_FR. */
	public static final String WORD_100_FR = " cents";

	/** The Constant WORD_AND_FR. */
	public static final String WORD_AND_FR = " et ";

	/** The Constant WORD_1000_FR. */
	public static final String WORD_1000_FR = " Milles ";

	/** The Constant WORD_MILLION_FR. */
	public static final String WORD_MILLION_FR = " Million ";

	/** The Constant WORD_100. */
	public static final String WORD_100 = "مائه";

	/** The Constant WORD_200. */
	public static final String WORD_200 = "مائتين";

	/** The Constant WORD_1000. */
	public static final String WORD_1000 = "ألف";

	/** The Constant WORD_2000. */
	public static final String WORD_2000 = "ألفين";

	/** The Constant WORD_1000_1. */
	public static final String WORD_1000_1 = " ألاف";

	/**
	 * Convert number to Words by given params.
	 *
	 * @author MoezMhiri
	 * @param number the number
	 * @param lang the lang (EN / FR / AR)
	 * @return the string
	 * @throws IllegalAccessException the illegal access exception
	 * @throws NoSuchFieldException the no such field exception
	 */
	public static String convertNumberToWords(String number, String lang)
			throws IllegalAccessException, NoSuchFieldException {

		Integer i = Integer.parseInt(number);
		String[] units = lang.equals("EN") ? UNITS_EN : UNITS_FR;
		String[] tens = lang.equals("EN") ? TENS_EN : TENS_FR;
		if (lang.equals("AR")) {
			return convertNumberToArabicWords(number);
		}
		return convert(units, tens, i, lang);
	}

	/**
	 * Convert & mapping in case lang = EN or FR.
	 * 
	 * @author MoezMhiri
	 * @param units the units
	 * @param tens the tens
	 * @param number the number
	 * @param lang the lang (EN / FR)
	 * @return the string
	 * @throws IllegalAccessException the illegal access exception
	 * @throws NoSuchFieldException the no such field exception
	 * @throws SecurityException the security exception
	 * @throws IllegalArgumentException the illegal argument exception
	 */
	private static String convert(String[] units, String[] tens, Integer number, String lang)
			throws IllegalAccessException, NoSuchFieldException {

		if (number < 20) {
			return units[number];
		}

		if (number < 100) {
			return tens[number / 10]
					+ ((number % 10 > 0) ? " " + convert(units, tens, number % 10, lang) : "");
		}

		if (number < 1000) {
			return units[number / 100]
					+ NumberToWordConverter.class.getDeclaredField("WORD_100_" + lang).get(null)
							.toString()
					+ ((number % 100 > 0)
							? NumberToWordConverter.class.getDeclaredField("WORD_AND_" + lang)
									.get(null).toString() + convert(units, tens, number % 100, lang)
							: "");
		}

		if (number < 1000000) {
			return convert(units, tens, number / 1000, lang)
					+ NumberToWordConverter.class.getDeclaredField("WORD_1000_" + lang).get(null)
							.toString()
					+ ((number % 1000 > 0) ? " " + convert(units, tens, number % 1000, lang) : "");
		}

		return convert(units, tens, number / 1000000, lang)
				+ NumberToWordConverter.class.getDeclaredField("WORD_MILLION_" + lang).get(null)
						.toString()
				+ ((number % 1000000 > 0) ? " " + convert(units, tens, number % 1000000, lang)
						: "");
	}

	/**
	 * Convert number to arabic words.
	 * 
	 * @author MoezMhiri
	 * @param number the number
	 * @return the string
	 * @throws NumberFormatException the number format exception
	 */
	private static String convertNumberToArabicWords(String number) {

		// check if the input string is number or not
		if (Boolean.TRUE.equals(NumberUtils.isNumeric(number))) {
			// check if its floating point number or not
			if (number.contains(".")) {
				// the number
				String theNumber = number.substring(0, number.indexOf('.'));
				// the floating point number
				String theFloat = number.substring(number.indexOf('.') + 1);
				// check how many digits in the number
				// 1:x | 2:xx | 3:xxx | 4:xxxx | 5:xxxxx | 6:xxxxxx
				switch (theNumber.length()) {
					case 1:
						return convertOneDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					case 2:
						return convertTwoDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					case 3:
						return convertThreeDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					case 4:
						return convertFourDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					case 5:
						return convertFiveDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					case 6:
						return convertSixDigitsToWord(theNumber) + " , "
								+ convertTwoDigitsToWord(theFloat);

					default:
						return "";
				}
			}
			else {
				switch (number.length()) {
					case 1:
						return convertOneDigitsToWord(number);

					case 2:
						return convertTwoDigitsToWord(number);

					case 3:
						return convertThreeDigitsToWord(number);

					case 4:
						return convertFourDigitsToWord(number);

					case 5:
						return convertFiveDigitsToWord(number);

					case 6:
						return convertSixDigitsToWord(number);

					default:
						return "";
				}
			}
		}
		else {
			return "";
		}
	}

	/**
	 * Convert one digits to Arabic word.
	 *
	 * @param oneDigit the one digit
	 * @return the string
	 */
	private static String convertOneDigitsToWord(String oneDigit) {

		switch (Integer.parseInt(oneDigit)) {
			case 1:
				return "واحد";

			case 2:
				return "إثنان";

			case 3:
				return "ثلاثه";

			case 4:
				return "أربعه";

			case 5:
				return "خمسه";

			case 6:
				return "سته";

			case 7:
				return "سبعه";

			case 8:
				return "ثمانيه";

			case 9:
				return "تسعه";

			default:
				return "";
		}
	}

	/**
	 * Convert two digits.
	 *
	 * @param twoDigits the two digits
	 * @return the string
	 */
	private static String convertTwoDigitsToWord(String twoDigits) {

		String returnAlpha = "00";
		// check if the first digit is 0 like 0x
		if (twoDigits.charAt(0) == '0' && twoDigits.charAt(1) != '0') {
			// convert two digits to one
			return convertOneDigitsToWord(String.valueOf(twoDigits.charAt(1)));
		}
		else {
			switch (NumberUtils.getIntVal(twoDigits.charAt(0))) {
				case 1: // 1x
					if (NumberUtils.getIntVal(twoDigits.charAt(1)) == 1) {
						return "إحدى عشر";
					}
					if (NumberUtils.getIntVal(twoDigits.charAt(1)) == 2) {
						return "إثنى عشر";
					}
					else {
						return convertOneDigitsToWord(String.valueOf(twoDigits.charAt(1))) + " "
								+ "عشر";
					}

				case 2: // 2x x:not 0
					returnAlpha = "عشرون";
					break;

				case 3: // 3x x:not 0
					returnAlpha = "ثلاثون";
					break;

				case 4: // 4x x:not 0
					returnAlpha = "أريعون";
					break;

				case 5: // 5x x:not 0
					returnAlpha = "خمسون";
					break;

				case 6: // 6x x:not 0
					returnAlpha = "ستون";
					break;

				case 7: // 7x x:not 0
					returnAlpha = "سبعون";
					break;

				case 8: // 8x x:not 0
					returnAlpha = "ثمانون";
					break;

				case 9: // 9x x:not 0
					returnAlpha = "تسعون";
					break;

				default:
					returnAlpha = "";
					break;
			}
		}

		// 20 - 99
		if (convertOneDigitsToWord(String.valueOf(twoDigits.charAt(1))).length() == 0) {
			return returnAlpha;
		}
		else {
			// xx x:not 0
			return convertOneDigitsToWord(String.valueOf(twoDigits.charAt(1))) + " و "
					+ returnAlpha;
		}
	}

	/**
	 * Convert three digits.
	 *
	 * @param threeDigits the three digits
	 * @return the string
	 */
	private static String convertThreeDigitsToWord(String threeDigits) {

		// check the first digit x00
		switch (NumberUtils.getIntVal(threeDigits.charAt(0))) {

			case 1: // 100 - 199
				if (NumberUtils.getIntVal(threeDigits.charAt(1)) == 0) {
					// 10x
					if (NumberUtils.getIntVal(threeDigits.charAt(2)) == 0) {
						// 100
						return WORD_100;
					}
					else {
						// 10x x: is not 0
						return WORD_100 + " و "
								+ convertOneDigitsToWord(String.valueOf(threeDigits.charAt(2)));
					}
				}
				else {
					// 1xx x: is not 0
					return WORD_100 + " و " + convertTwoDigitsToWord(threeDigits.substring(1, 3));
				}

			case 2: // 200 - 299
				if (NumberUtils.getIntVal(threeDigits.charAt(1)) == 0) {
					// 20x
					if (NumberUtils.getIntVal(threeDigits.charAt(2)) == 0) {
						// 200
						return WORD_200;
					}
					else {
						// 20x x:not 0
						return WORD_200 + " و "
								+ convertOneDigitsToWord(String.valueOf(threeDigits.charAt(2)));
					}
				}
				else {
					// 2xx x:not 0
					return WORD_200 + " و " + convertTwoDigitsToWord(threeDigits.substring(1, 3));
				}

			case 3:
			case 4:
			case 5:
			case 6:
			case 7:
			case 8:

			case 9:// 300 - 999
				if (NumberUtils.getIntVal(threeDigits.charAt(1)) == 0) {
					// x0x x:not 0
					if (NumberUtils.getIntVal(threeDigits.charAt(2)) == 0) {
						// x00 x:not 0
						return convertOneDigitsToWord(
								String.valueOf(threeDigits.charAt(1) + WORD_100));
					}
					else {
						// x0x x:not 0
						return convertOneDigitsToWord(String.valueOf(threeDigits.charAt(0)))
								+ WORD_100 + " و "
								+ convertOneDigitsToWord(String.valueOf(threeDigits.charAt(2)));
					}
				}
				else {
					// xxx x:not 0
					return convertOneDigitsToWord(String.valueOf(threeDigits.charAt(0))) + WORD_100
							+ " و " + convertTwoDigitsToWord(threeDigits.substring(1, 3));
				}

			case 0:// 000 - 099
				if (threeDigits.charAt(1) == '0') {
					// 00x
					if (threeDigits.charAt(2) == '0') {
						// 000
						return "";
					}
					else {
						// 00x x:not 0
						return convertOneDigitsToWord(String.valueOf(threeDigits.charAt(2)));
					}
				}
				else {
					// 0xx x:not 0
					return convertTwoDigitsToWord(threeDigits.substring(1, 3));
				}

			default:
				return "";
		}
	}

	/**
	 * Convert four digits.
	 *
	 * @param fourDigits the four digits
	 * @return the string
	 */
	private static String convertFourDigitsToWord(String fourDigits) {

		switch (NumberUtils.getIntVal(fourDigits.charAt(0))) {

			case 1: // 1000 - 1999
				if (NumberUtils.getIntVal(fourDigits.charAt(1)) == 0) {
					// 10xx x:not 0
					if (NumberUtils.getIntVal(fourDigits.charAt(2)) == 0) {
						// 100x x:not 0
						if (NumberUtils.getIntVal(fourDigits.charAt(3)) == 0) {
							// 1000
							return WORD_1000;
						}
						else {
							// 100x x:not 0
							return WORD_1000 + " و "
									+ convertOneDigitsToWord(String.valueOf(fourDigits.charAt(3)));
						}
					}
					else {
						// 10xx x:not 0
						return WORD_1000 + " و "
								+ convertTwoDigitsToWord(fourDigits.substring(2, 3));
					}
				}
				else {
					// 1xxx x:not 0
					return WORD_1000 + " و " + convertThreeDigitsToWord(fourDigits.substring(1, 4));
				}

			case 2: // 2000 - 2999
				if (NumberUtils.getIntVal(fourDigits.charAt(1)) == 0) {
					// 20xx
					if (NumberUtils.getIntVal(fourDigits.charAt(2)) == 0) {
						// 200x
						if (NumberUtils.getIntVal(fourDigits.charAt(3)) == 0) {
							// 2000
							return WORD_2000;
						}
						else {
							// 200x x:not 0
							return WORD_2000 + " و "
									+ convertOneDigitsToWord(String.valueOf(fourDigits.charAt(3)));
						}
					}
					else {
						// 20xx x:not 0
						return WORD_2000 + " و "
								+ convertTwoDigitsToWord(fourDigits.substring(2, 3));
					}
				}
				else {
					// 2xxx x:not 0
					return WORD_2000 + " و " + convertThreeDigitsToWord(fourDigits.substring(1, 4));
				}

			case 3:
			case 4:
			case 5:
			case 6:
			case 7:
			case 8:

			case 9: // 3000 - 9999
				if (NumberUtils.getIntVal(fourDigits.charAt(1)) == 0) {
					// x0xx x:not 0
					if (NumberUtils.getIntVal(fourDigits.charAt(2)) == 0) {
						// x00x x:not 0
						if (NumberUtils.getIntVal(fourDigits.charAt(3)) == 0) {
							// x000 x:not 0
							return convertOneDigitsToWord(String.valueOf(fourDigits.charAt(0)))
									+ WORD_1000_1;
						}
						else {
							// x00x x:not 0
							return convertOneDigitsToWord(String.valueOf(fourDigits.charAt(0)))
									+ WORD_1000_1 + " و "
									+ convertOneDigitsToWord(String.valueOf(fourDigits.charAt(3)));
						}
					}
					else {
						// x0xx x:not 0
						return convertOneDigitsToWord(String.valueOf(fourDigits.charAt(0)))
								+ WORD_1000_1 + " و "
								+ convertTwoDigitsToWord(fourDigits.substring(2, 3));
					}
				}
				else {
					// xxxx x:not 0
					return convertOneDigitsToWord(String.valueOf(fourDigits.charAt(0)))
							+ WORD_1000_1 + " و "
							+ convertThreeDigitsToWord(fourDigits.substring(1, 4));
				}

			default:
				return "";
		}
	}

	/**
	 * Convert five digits.
	 *
	 * @param fiveDigits the five digits
	 * @return the string
	 */
	private static String convertFiveDigitsToWord(String fiveDigits) {

		if (convertThreeDigitsToWord(fiveDigits.substring(2, 5)).length() == 0) {
			return convertTwoDigitsToWord(fiveDigits.substring(0, 2)) + " ألف ";
		}
		else {
			return convertTwoDigitsToWord(fiveDigits.substring(0, 2)) + " ألفا " + " و "
					+ convertThreeDigitsToWord(fiveDigits.substring(2, 5));
		}
	}

	/**
	 * Convert six digits.
	 *
	 * @param sixDigits the six digits
	 * @return the string
	 */
	private static String convertSixDigitsToWord(String sixDigits) {

		if (convertThreeDigitsToWord(sixDigits.substring(2, 5)).length() == 0) {
			return convertThreeDigitsToWord(sixDigits.substring(0, 3)) + " ألف ";
		}
		else {
			return convertThreeDigitsToWord(sixDigits.substring(0, 3)) + " ألفا " + " و "
					+ convertThreeDigitsToWord(sixDigits.substring(3, 6));
		}
	}
}
