/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.string;

import java.io.Serializable;

import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link StringUtils} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class StringUtils implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8626513331514212573L;

	/**
	 * Char insert.
	 * 
	 * @author HaythemBenizid
	 * @param str the str
	 * @param c the c
	 * @param j the j
	 * @return the string
	 */
	public static String charInsert(String str, char c, int j) {

		String begin = str.substring(0, j);
		String end = str.substring(j);
		return begin + c + end;
	}

	/**
	 * Count number of words.
	 * 
	 * @author HaythemBenizid
	 * @param line the line
	 * @return the integer
	 */
	public static Integer countNumberOfWords(String line) {

		String trimmedLine = line.trim();
		return trimmedLine.isEmpty() ? 0 : trimmedLine.split("\\s+").length;
	}

	/**
	 * Converting string.
	 *
	 * @author HaythemBenizid
	 * @param word the word
	 * @param separator the separator
	 * @return the string[]
	 */
	public static String[] convertingString(String word, String separator) {

		if (!ACMValidationUtils.isNullOrEmpty(word)
				&& !ACMValidationUtils.isNullOrEmpty(separator)) {
			// step 1 : converting String to array of String by given separator
			String[] elements = word.split(separator);
			// step 2 : filter founded data delete space
			for (int i = 0; i < elements.length; i++) {
				elements[i] = elements[i].trim();
			}
			return elements;
		}
		return new String[0];
	}

	/**
	 * This method matches the regular expression for the E-mail and the given input Email and
	 * returns true if they match and false otherwise.
	 *
	 * @author HaythemBenizid
	 * @param email the email
	 * @return the boolean
	 */
	public static Boolean mailIsValid(String email) {

		String regex = "^[\\w-_\\.+]*[\\w-_\\.]\\@([\\w]+\\.)+[\\w]+[\\w]$";
		return email.matches(regex);
	}

	/**
	 * This method matches the regular expression for given String to find out if it's a valid XML
	 * returns true if they match and false otherwise.
	 *
	 * @author HaythemBenizid
	 * @param xml the xml
	 * @return the boolean
	 */
	public static Boolean xmlIsValid(String xml) {

		String regex = "(?s).*(<(\\w+)[^>]*>.*</\\2>|<(\\w+)[^>]*/>).*";
		return xml.matches(regex);
	}
}
