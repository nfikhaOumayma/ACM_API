/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ResponseIncomeDakhliApiDTO.
 */
public class ResponseIncomeDakhliApiDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6447233116628302928L;

	/** The request number. */
	@JsonProperty("requestNumber")
	private String requestNumber;

	/** The message. */
	@JsonProperty("message")
	private String message;

	/** The employment status info. */
	@JsonProperty("employmentStatusInfo")
	private List<RespInfoDakhliApiDTO> employmentStatusInfo;

	/**
	 * Gets the request number.
	 *
	 * @return the request number
	 */
	public String getRequestNumber() {

		return requestNumber;
	}

	/**
	 * Sets the request number.
	 *
	 * @param requestNumber the new request number
	 */
	public void setRequestNumber(String requestNumber) {

		this.requestNumber = requestNumber;
	}

	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	public String getMessage() {

		return message;
	}

	/**
	 * Sets the message.
	 *
	 * @param message the new message
	 */
	public void setMessage(String message) {

		this.message = message;
	}

	/**
	 * Gets the employment status info.
	 *
	 * @return the employment status info
	 */
	public List<RespInfoDakhliApiDTO> getEmploymentStatusInfo() {

		return employmentStatusInfo;
	}

	/**
	 * Sets the employment status info.
	 *
	 * @param employmentStatusInfo the new employment status info
	 */
	public void setEmploymentStatusInfo(List<RespInfoDakhliApiDTO> employmentStatusInfo) {

		this.employmentStatusInfo = employmentStatusInfo;
	}

	/**
	 * Instantiates a new response income dakhli api DTO.
	 */
	public ResponseIncomeDakhliApiDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ResponseIncomeDakhliApiDTO [requestNumber=" + requestNumber + ", message=" + message
				+ ", employmentStatusInfo=" + employmentStatusInfo + "]";
	}

}
