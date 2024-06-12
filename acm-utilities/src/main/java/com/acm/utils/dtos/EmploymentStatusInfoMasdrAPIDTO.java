/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class EmploymentStatusInfoMasdrAPIDTO.
 */
public class EmploymentStatusInfoMasdrAPIDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1327078523694601997L;

	/** The employment status info. */
	@JsonProperty("employmentStatusInfo")
	private List<EmploymentStatusEntryMasdrAPIDTO> employmentStatusInfo;

	/**
	 * Instantiates a new employment status info masdr APIDTO.
	 */
	public EmploymentStatusInfoMasdrAPIDTO() {

		super();
	}

	/**
	 * Instantiates a new employment status info masdr APIDTO.
	 *
	 * @param employmentStatusInfo the employment status info
	 */
	public EmploymentStatusInfoMasdrAPIDTO(
			List<EmploymentStatusEntryMasdrAPIDTO> employmentStatusInfo) {

		super();
		this.employmentStatusInfo = employmentStatusInfo;
	}

	/**
	 * Gets the employment status info.
	 *
	 * @return the employment status info
	 */
	public List<EmploymentStatusEntryMasdrAPIDTO> getEmploymentStatusInfo() {

		return employmentStatusInfo;
	}

	/**
	 * Sets the employment status info.
	 *
	 * @param employmentStatusInfo the new employment status info
	 */
	public void setEmploymentStatusInfo(
			List<EmploymentStatusEntryMasdrAPIDTO> employmentStatusInfo) {

		this.employmentStatusInfo = employmentStatusInfo;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "EmploymentStatusInfoMasdrAPIDTO [employmentStatusInfo=" + employmentStatusInfo
				+ "]";
	}

}
