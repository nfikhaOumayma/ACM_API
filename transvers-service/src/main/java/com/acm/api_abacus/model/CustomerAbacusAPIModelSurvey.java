/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.List;

/**
 * {@link CustomerAbacusAPIModel6} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelSurvey implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5504795152035932564L;

	/** The survey ID. */
	private int surveyID;

	/** The user defined field group ID. */
	private int userDefinedFieldGroupID;

	/** The date. */
	private String date;

	/** The total score. */
	private double totalScore;

	/** The udf links. */
	private List<CustomerAbacusAPIModelUdfLink> udfLinks;

	/** The udf table. */
	private int udfTable;

	/**
	 * Gets the survey ID.
	 *
	 * @return the surveyID
	 */
	public int getSurveyID() {

		return surveyID;
	}

	/**
	 * Sets the survey ID.
	 *
	 * @param surveyID the surveyID to set
	 */
	public void setSurveyID(int surveyID) {

		this.surveyID = surveyID;
	}

	/**
	 * Gets the user defined field group ID.
	 *
	 * @return the userDefinedFieldGroupID
	 */
	public int getUserDefinedFieldGroupID() {

		return userDefinedFieldGroupID;
	}

	/**
	 * Sets the user defined field group ID.
	 *
	 * @param userDefinedFieldGroupID the userDefinedFieldGroupID to set
	 */
	public void setUserDefinedFieldGroupID(int userDefinedFieldGroupID) {

		this.userDefinedFieldGroupID = userDefinedFieldGroupID;
	}

	/**
	 * Gets the date.
	 *
	 * @return the date
	 */
	public String getDate() {

		return date;
	}

	/**
	 * Sets the date.
	 *
	 * @param date the date to set
	 */
	public void setDate(String date) {

		this.date = date;
	}

	/**
	 * Gets the total score.
	 *
	 * @return the totalScore
	 */
	public double getTotalScore() {

		return totalScore;
	}

	/**
	 * Sets the total score.
	 *
	 * @param totalScore the totalScore to set
	 */
	public void setTotalScore(double totalScore) {

		this.totalScore = totalScore;
	}

	/**
	 * Gets the udf links.
	 *
	 * @return the udfLinks
	 */
	public List<CustomerAbacusAPIModelUdfLink> getUdfLinks() {

		return udfLinks;
	}

	/**
	 * Sets the udf links.
	 *
	 * @param udfLinks the udfLinks to set
	 */
	public void setUdfLinks(List<CustomerAbacusAPIModelUdfLink> udfLinks) {

		this.udfLinks = udfLinks;
	}

	/**
	 * Gets the udf table.
	 *
	 * @return the udfTable
	 */
	public int getUdfTable() {

		return udfTable;
	}

	/**
	 * Sets the udf table.
	 *
	 * @param udfTable the udfTable to set
	 */
	public void setUdfTable(int udfTable) {

		this.udfTable = udfTable;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelSurvey [surveyID=" + surveyID + ", userDefinedFieldGroupID="
				+ userDefinedFieldGroupID + ", date=" + date + ", totalScore=" + totalScore
				+ ", udfLinks=" + udfLinks + ", udfTable=" + udfTable + "]";
	}

}
