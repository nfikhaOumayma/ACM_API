/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link RiskEntity } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class RiskEntity {

	/** The id. */
	public int id;

	/** The name. */
	public String name;

	/** The label. */
	public String label;

	/** The coef. */
	public double coef;

	/** The format. */
	public String format;

	/** The min interval. */
	public double minInterval;

	/** The max interval. */
	public double maxInterval;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public int getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(int id) {

		this.id = id;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the label.
	 *
	 * @return the label
	 */
	public String getLabel() {

		return label;
	}

	/**
	 * Sets the label.
	 *
	 * @param label the label to set
	 */
	public void setLabel(String label) {

		this.label = label;
	}

	/**
	 * Gets the coef.
	 *
	 * @return the coef
	 */
	public double getCoef() {

		return coef;
	}

	/**
	 * Sets the coef.
	 *
	 * @param coef the coef to set
	 */
	public void setCoef(double coef) {

		this.coef = coef;
	}

	/**
	 * Gets the format.
	 *
	 * @return the format
	 */
	public String getFormat() {

		return format;
	}

	/**
	 * Sets the format.
	 *
	 * @param format the format to set
	 */
	public void setFormat(String format) {

		this.format = format;
	}

	/**
	 * Gets the min interval.
	 *
	 * @return the minInterval
	 */
	public double getMinInterval() {

		return minInterval;
	}

	/**
	 * Sets the min interval.
	 *
	 * @param minInterval the minInterval to set
	 */
	public void setMinInterval(double minInterval) {

		this.minInterval = minInterval;
	}

	/**
	 * Gets the max interval.
	 *
	 * @return the maxInterval
	 */
	public double getMaxInterval() {

		return maxInterval;
	}

	/**
	 * Sets the max interval.
	 *
	 * @param maxInterval the maxInterval to set
	 */
	public void setMaxInterval(double maxInterval) {

		this.maxInterval = maxInterval;
	}

}
