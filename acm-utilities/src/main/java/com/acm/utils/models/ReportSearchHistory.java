/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link ReportSearchHistory} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Entity
@Table(name = "ACM_REPORT_SEARCH_HISTORY")
public class ReportSearchHistory extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1206370815924552799L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_REPORT_SEARCH_HISTORY", unique = true, nullable = false)
	private Long id;

	/** The username. */
	@Column(name = "USERNAME", nullable = false)
	private String username;

	/** The search data json. */
	@Column(name = "SEARCH_DATA_JSON", nullable = false)
	private String searchDataJson;

	/** The label search history. */
	@Column(name = "LABEL_SEARCH_HISTORY", nullable = false)
	private String labelSearchHistory;

	/** The report name. */
	@Column(name = "REPORT_NAME", nullable = false)
	private String reportName;

	/**
	 * Instantiates a new report search history.
	 */
	public ReportSearchHistory() {

		/*
		 * EMPTY
		 */
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ReportSearchHistory [id=" + id + ", username=" + username + ", searchDataJson="
				+ searchDataJson + ", labelSearchHistory=" + labelSearchHistory + ", reportName="
				+ reportName + "]";
	}
}
