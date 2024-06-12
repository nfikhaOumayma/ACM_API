/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link ReportSearchHistoryDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
public class ReportSearchHistoryDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7399405090476078545L;

	/** The id. */
	private Long id;

	/** The username. */
	private String username;

	/** The search data json. */
	private String searchDataJson;

	/** The label search history. */
	private String labelSearchHistory;

	/** The report name. */
	private String reportName;

	/** The enabled. */
	private Boolean enabled;

	/** The date insertion. */
	private String dateInsertion;

	/** The reporting DTO. */
	private ReportingDTO reportingDTO;

	/**
	 * Instantiates a new report search history DTO.
	 */
	public ReportSearchHistoryDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new report search history DTO.
	 *
	 * @param id the id
	 */
	public ReportSearchHistoryDTO(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the username to set
	 */
	public void setUsername(String username) {

		this.username = username;
	}

	/**
	 * Gets the search data json.
	 *
	 * @return the searchDataJson
	 */
	public String getSearchDataJson() {

		return searchDataJson;
	}

	/**
	 * Sets the search data json.
	 *
	 * @param searchDataJson the searchDataJson to set
	 */
	public void setSearchDataJson(String searchDataJson) {

		this.searchDataJson = searchDataJson;
	}

	/**
	 * Gets the label search history.
	 *
	 * @return the labelSearchHistory
	 */
	public String getLabelSearchHistory() {

		return labelSearchHistory;
	}

	/**
	 * Sets the label search history.
	 *
	 * @param labelSearchHistory the labelSearchHistory to set
	 */
	public void setLabelSearchHistory(String labelSearchHistory) {

		this.labelSearchHistory = labelSearchHistory;
	}

	/**
	 * Gets the report name.
	 *
	 * @return the reportName
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the reportName to set
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public String getStringInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setStringInsertion(String dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the reporting DTO.
	 *
	 * @return the reportingDTO
	 */
	public ReportingDTO getReportingDTO() {

		return reportingDTO;
	}

	/**
	 * Sets the reporting DTO.
	 *
	 * @param reportingDTO the reportingDTO to set
	 */
	public void setReportingDTO(ReportingDTO reportingDTO) {

		this.reportingDTO = reportingDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ReportSearchHistoryDTO [id=" + id + ", username=" + username + ", searchDataJson="
				+ searchDataJson + ", labelSearchHistory=" + labelSearchHistory + ", reportName="
				+ reportName + ", enabled=" + enabled + ", dateInsertion=" + dateInsertion + "]";
	}
}
