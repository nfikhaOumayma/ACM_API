/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.validation;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.acm.utils.models.GenericModel;

/**
 * {@link ACMValidationUtils} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public final class ACMValidationUtils implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8299462820024465396L;

	/**
	 * Controle is Null Or Empty for collection.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty param type Collection maybe null or empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(final Collection<?> maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || maybeNullOrEmpty.isEmpty();
	}

	/**
	 * Controle is Null Or Empty for Map.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty param type Map maybe null or empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(final Map<?, ?> maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || maybeNullOrEmpty.isEmpty();
	}

	/**
	 * Controle is Null Or Empty for String.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty param type String maybe null or empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(String maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || maybeNullOrEmpty.isEmpty()
				|| "null".equals(maybeNullOrEmpty);
	}

	/**
	 * Controle is Null Or Empty for Object[].
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty param type Object maybe null or empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(Object[] maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || maybeNullOrEmpty.length == 0;
	}

	/**
	 * Controle is Null Or Empty for Date.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty param type Date maybe null or empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(Date maybeNullOrEmpty) {

		return maybeNullOrEmpty == null;
	}

	/**
	 * Controle is Null Or Empty for object or {@link GenericModel}.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty the Object is maybe Null Or Empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(final Object maybeNullOrEmpty) {

		return maybeNullOrEmpty == null;
	}

	/**
	 * Controle is Null Or Empty for List or {@link GenericModel}.
	 *
	 * @author HaythemBenizid
	 * @param maybeNullOrEmpty the List is maybe Null Or Empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(final List<?> maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || maybeNullOrEmpty.isEmpty();
	}

	/**
	 * Checks if is null or empty for Iterable or {@link GenericModel}.
	 *
	 * @author YesserSomai
	 * @param maybeNullOrEmpty the Iterable is maybe Null Or Empty
	 * @return true, if is null or empty
	 */
	public static boolean isNullOrEmpty(final Iterable<?> maybeNullOrEmpty) {

		return maybeNullOrEmpty == null || !maybeNullOrEmpty.iterator().hasNext();
	}

	/**
	 * Checks if is integer.
	 *
	 * @param string the string
	 * @return true, if is integer
	 */
	public static boolean isNumeric(String string) {

		return string.chars().allMatch(Character::isDigit);
	}

	/**
	 * the goal have Not null string.
	 *
	 * @author HaythemBenizid
	 * @param maybeNull the maybeNull
	 * @return empty string if null or string
	 */
	public static String notNullString(String maybeNull) {

		if (maybeNull == null || "null".equals(maybeNull)) {
			return "";
		}
		else {
			return maybeNull.trim();
		}
	}

	/**
	 * Count number from string.
	 *
	 * @author HaythemBenizid
	 * @param string the string value
	 * @return the integer number
	 */
	public static Integer countNumberFromString(String string) {

		Integer number = 0;
		if (!ACMValidationUtils.isNullOrEmpty(string)) {
			for (int i = 0; i < string.length(); i++) {
				if (isNumeric(String.valueOf(string.charAt(i)))) {
					number = number + 1;
				}
			}
		}
		return number;
	}

	/**
	 * Format a double number to display a double with <code>decimalNumber</code> digits after the
	 * decimal point.
	 * 
	 * @author HaythemBenizid
	 * @param number the number to format
	 * @param decimalNumber the decimal number to format with
	 * @return the formatted number
	 */
	public static double decimalFormatter(double number, int decimalNumber) {

		double formatter = Math.pow(10, decimalNumber);
		return (number * formatter) / formatter;
	}

	/**
	 * Format full name.
	 *
	 * @author HaythemBenizid
	 * @param name the name
	 * @param firstName the first name
	 * @return the string like (name , firstName )
	 */
	public static String formatFullName(String name, String firstName) {

		if (("".equals(name) || name == null) && ("".equals(firstName) || firstName == null)) {
			return "";
		}
		return (notNullString(name)) + ", " + notNullString(firstName);
	}

	/**
	 * Format label string without spaces .
	 *
	 * @author HaythemBenizid
	 * @param label the label
	 * @return empty or String like label
	 */
	public static String formatLabel(String label) {

		return label != null ? label.trim() : "";
	}

}
