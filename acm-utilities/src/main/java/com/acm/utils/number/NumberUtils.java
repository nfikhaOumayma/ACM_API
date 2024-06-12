/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.number;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * {@link NumberUtils} class.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public class NumberUtils implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1007862290109475801L;

	/**
	 * Checks if is numeric.
	 * 
	 * @author HaythemBenizid
	 * @param strNum the str num
	 * @return true, if is numeric
	 */
	public static boolean isNumeric(String strNum) {

		if (strNum == null) {
			return false;
		}
		try {
			Double.parseDouble(strNum);
		}
		catch (NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	/**
	 * Gets the Integer value from given "char".
	 * 
	 * @author HaythemBenizid
	 * @param c the c
	 * @return the int val
	 */
	public static Integer getIntVal(char c) {

		return Integer.parseInt(String.valueOf(c));
	}

	/**
	 * Round big decimal : BigDecimal.ROUND_HALF_EVEN / ROUND_HALF_DOWN / ROUND_HALF_UP.
	 *
	 * @author HaythemBenizid
	 * @param bigDecimal the big decimal
	 * @param places the places
	 * @param roundingMode the rounding mode
	 * @return the big decimal
	 */
	public static BigDecimal roundBigDecimal(BigDecimal bigDecimal, int places, int roundingMode) {

		bigDecimal = bigDecimal.setScale(places, roundingMode);
		return bigDecimal;
	}

	/**
	 * Round double :BigDecimal.ROUND_HALF_EVEN / ROUND_HALF_DOWN / ROUND_HALF_UP.
	 *
	 * @author HaythemBenizid
	 * @param d the d
	 * @param places the places
	 * @param roundingMode the rounding mode
	 * @return the double
	 */
	public static double roundDouble(double d, int places, int roundingMode) {

		BigDecimal bigDecimal = new BigDecimal(Double.toString(d));
		bigDecimal = bigDecimal.setScale(places, roundingMode);
		return bigDecimal.doubleValue();
	}

	/**
	 * Round float : BigDecimal.ROUND_HALF_EVEN / ROUND_HALF_DOWN / ROUND_HALF_UP.
	 *
	 * @author HaythemBenizid
	 * @param f the f
	 * @param places the places
	 * @param roundingMode the rounding mode
	 * @return the float
	 */
	public static float roundFloat(float f, int places, int roundingMode) {

		BigDecimal bigDecimal = new BigDecimal(Float.toString(f));
		bigDecimal = bigDecimal.setScale(places, roundingMode);
		return bigDecimal.floatValue();
	}
}
