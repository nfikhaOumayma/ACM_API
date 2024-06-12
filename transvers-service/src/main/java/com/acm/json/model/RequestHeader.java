/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RequestHeader} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RequestHeader {

	/** The user ID. */
	@JsonProperty("USER_ID")
	public String userID;

	/** The pwd. */
	@JsonProperty("PASSWORD")
	public String pwd;

	/** The subject type. */
	@JsonProperty("SUBJECT_TYPE")
	public String subjectType;

	/** The in quiry type. */
	@JsonProperty("INQUIRY_TYPE")
	public String inQuiryType;

	/**
	 * Instantiates a new request header.
	 */
	public RequestHeader() {

		// Empty
	}

	/**
	 * Instantiates a new request header.
	 *
	 * @param userID the user ID
	 * @param pwd the pwd
	 * @param subjectType the subject type
	 * @param inQuiryType the in quiry type
	 */
	public RequestHeader(String userID, String pwd, String subjectType, String inQuiryType) {

		this.userID = userID;
		this.pwd = pwd;
		this.subjectType = subjectType;
		this.inQuiryType = inQuiryType;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestHeader [" + (userID != null ? "userID=" + userID + ", " : "")
				+ (pwd != null ? "pwd=" + pwd + ", " : "")
				+ (subjectType != null ? "subjectType=" + subjectType + ", " : "")
				+ (inQuiryType != null ? "inQuiryType=" + inQuiryType : "") + "]";
	}

}
