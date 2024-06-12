/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * {@link UDFLinksGroupeFieldsDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class UDFLinksGroupeFieldsDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8846911912467095872L;

	/** The user defined field group ID. */
	private Long userDefinedFieldGroupID;

	/** The user defined field group name. */
	private String userDefinedFieldGroupName;

	/** The date. */
	private Date date;

	/** The total score. */
	private Long totalScore;

	/** The surveys id. */
	private Long surveysId;

	/** The mondatory. */
	private Boolean mondatory;

	/** The udf groupe fields models. */
	private List<UDFLinksGroupeFieldsModelDTO> udfGroupeFieldsModels;

	/**
	 * Instantiates a new UDF groupe fields.
	 */
	public UDFLinksGroupeFieldsDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Gets the user defined field group ID.
	 *
	 * @return the userDefinedFieldGroupID
	 */
	public Long getUserDefinedFieldGroupID() {

		return userDefinedFieldGroupID;
	}

	/**
	 * Sets the user defined field group ID.
	 *
	 * @param userDefinedFieldGroupID the userDefinedFieldGroupID to set
	 */
	public void setUserDefinedFieldGroupID(Long userDefinedFieldGroupID) {

		this.userDefinedFieldGroupID = userDefinedFieldGroupID;
	}

	/**
	 * Gets the date.
	 *
	 * @return the date
	 */
	public Date getDate() {

		return date;
	}

	/**
	 * Sets the date.
	 *
	 * @param date the date to set
	 */
	public void setDate(Date date) {

		this.date = date;
	}

	/**
	 * Gets the total score.
	 *
	 * @return the totalScore
	 */
	public Long getTotalScore() {

		return totalScore;
	}

	/**
	 * Sets the total score.
	 *
	 * @param totalScore the totalScore to set
	 */
	public void setTotalScore(Long totalScore) {

		this.totalScore = totalScore;
	}

	/**
	 * Gets the udf groupe fields models.
	 *
	 * @return the udfGroupeFieldsModels
	 */
	public List<UDFLinksGroupeFieldsModelDTO> getUdfGroupeFieldsModels() {

		return udfGroupeFieldsModels;
	}

	/**
	 * Sets the udf groupe fields models.
	 *
	 * @param udfGroupeFieldsModels the udfGroupeFieldsModels to set
	 */
	public void setUdfGroupeFieldsModels(List<UDFLinksGroupeFieldsModelDTO> udfGroupeFieldsModels) {

		this.udfGroupeFieldsModels = udfGroupeFieldsModels;
	}

	/**
	 * Gets the user defined field group name.
	 *
	 * @return the userDefinedFieldGroupName
	 */
	public String getUserDefinedFieldGroupName() {

		return userDefinedFieldGroupName;
	}

	/**
	 * Sets the user defined field group name.
	 *
	 * @param userDefinedFieldGroupName the userDefinedFieldGroupName to set
	 */
	public void setUserDefinedFieldGroupName(String userDefinedFieldGroupName) {

		this.userDefinedFieldGroupName = userDefinedFieldGroupName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UDFLinksGroupeFieldsDTO [userDefinedFieldGroupID=" + userDefinedFieldGroupID
				+ ", userDefinedFieldGroupName=" + userDefinedFieldGroupName + ", date=" + date
				+ ", totalScore=" + totalScore + ", udfGroupeFieldsModels=" + udfGroupeFieldsModels
				+ "]";
	}

	/**
	 * Gets the surveys id.
	 *
	 * @return the surveysId
	 */
	public Long getSurveysId() {

		return surveysId;
	}

	/**
	 * Sets the surveys id.
	 *
	 * @param surveysId the surveysId to set
	 */
	public void setSurveysId(Long surveysId) {

		this.surveysId = surveysId;
	}

	/**
	 * Gets the mondatory.
	 *
	 * @return the mondatory
	 */
	public Boolean getMondatory() {

		return mondatory;
	}

	/**
	 * Sets the mondatory.
	 *
	 * @param mondatory the new mondatory
	 */
	public void setMondatory(Boolean mondatory) {

		this.mondatory = mondatory;
	}

}
