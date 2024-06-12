/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.number;

import java.text.DecimalFormat;

import org.dozer.DozerConverter;

/**
 * the {@link DoubleToStringConverter} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class DoubleToStringConverter extends DozerConverter<Double, String> {

	/** The formatter. */
	DecimalFormat formatter = new DecimalFormat("#.00");

	/**
	 * Instantiates a new double to string converter.
	 */
	public DoubleToStringConverter() {

		super(Double.class, String.class);
	}

	/**
	 * Instantiates a new double to string converter.
	 *
	 * @param prototypeA the prototype A
	 * @param prototypeB the prototype B
	 */
	public DoubleToStringConverter(Class<Double> prototypeA, Class<String> prototypeB) {

		super(prototypeA, prototypeB);
	}

	/*
	 * (non-Javadoc)
	 * @see org.dozer.DozerConverter#convertTo(java.lang.Object, java.lang.Object)
	 */
	@Override
	public String convertTo(Double source, String destination) {

		if (source == null) {
			return null;
		}
		return formatter.format(source);
	}

	/*
	 * (non-Javadoc)
	 * @see org.dozer.DozerConverter#convertFrom(java.lang.Object, java.lang.Object)
	 */
	@Override
	public Double convertFrom(String source, Double destination) {

		if (source == null) {
			return null;
		}

		return Double.valueOf(formatter.format(destination));
	}
}
