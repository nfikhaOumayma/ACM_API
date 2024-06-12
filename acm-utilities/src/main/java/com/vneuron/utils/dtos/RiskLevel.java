/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link RiskLevel } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class RiskLevel {

	/** The id. */
	public int id;

	/** The label. */
	public String label;

	/** The name. */
	public String name;

	/** The comment. */
	public String comment;

	/** The risk level value. */
	public int riskLevelValue;

	/** The risk level start. */
	public int riskLevelStart;

	/** The risk level end. */
	public int riskLevelEnd;

	/** The color. */
	public String color;

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
	 * Gets the comment.
	 *
	 * @return the comment
	 */
	public String getComment() {

		return comment;
	}

	/**
	 * Sets the comment.
	 *
	 * @param comment the comment to set
	 */
	public void setComment(String comment) {

		this.comment = comment;
	}

	/**
	 * Gets the risk level value.
	 *
	 * @return the riskLevelValue
	 */
	public int getRiskLevelValue() {

		return riskLevelValue;
	}

	/**
	 * Sets the risk level value.
	 *
	 * @param riskLevelValue the riskLevelValue to set
	 */
	public void setRiskLevelValue(int riskLevelValue) {

		this.riskLevelValue = riskLevelValue;
	}

	/**
	 * Gets the risk level start.
	 *
	 * @return the riskLevelStart
	 */
	public int getRiskLevelStart() {

		return riskLevelStart;
	}

	/**
	 * Sets the risk level start.
	 *
	 * @param riskLevelStart the riskLevelStart to set
	 */
	public void setRiskLevelStart(int riskLevelStart) {

		this.riskLevelStart = riskLevelStart;
	}

	/**
	 * Gets the risk level end.
	 *
	 * @return the riskLevelEnd
	 */
	public int getRiskLevelEnd() {

		return riskLevelEnd;
	}

	/**
	 * Sets the risk level end.
	 *
	 * @param riskLevelEnd the riskLevelEnd to set
	 */
	public void setRiskLevelEnd(int riskLevelEnd) {

		this.riskLevelEnd = riskLevelEnd;
	}

	/**
	 * Gets the color.
	 *
	 * @return the color
	 */
	public String getColor() {

		return color;
	}

	/**
	 * Sets the color.
	 *
	 * @param color the color to set
	 */
	public void setColor(String color) {

		this.color = color;
	}

}
