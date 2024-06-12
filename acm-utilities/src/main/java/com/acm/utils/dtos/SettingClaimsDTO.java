/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

// TODO: Auto-generated Javadoc
/**
 * The Class SettingClaimsDTO.
 */
public class SettingClaimsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5586197244024675290L;

	/** The id. */

	private Long id;

	/** The category. */
	private String category;

	/** The subject. */
	private String subject;

	/** The assignement. */
	private String assignement;

	/** The pripority. */
	private String pripority;

	/** The processing time line. */
	private Integer processingTimeLine;

	/** The enabled. */
	private Boolean enabled;

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the subject.
	 *
	 * @return the subject
	 */
	public String getSubject() {

		return subject;
	}

	/**
	 * Sets the subject.
	 *
	 * @param subject the new subject
	 */
	public void setSubject(String subject) {

		this.subject = subject;
	}

	/**
	 * Gets the assignement.
	 *
	 * @return the assignement
	 */
	public String getAssignement() {

		return assignement;
	}

	/**
	 * Sets the assignement.
	 *
	 * @param assignement the new assignement
	 */
	public void setAssignement(String assignement) {

		this.assignement = assignement;
	}

	/**
	 * Gets the pripority.
	 *
	 * @return the pripority
	 */
	public String getPripority() {

		return pripority;
	}

	/**
	 * Sets the pripority.
	 *
	 * @param pripority the new pripority
	 */
	public void setPripority(String pripority) {

		this.pripority = pripority;
	}

	/**
	 * Gets the processing time line.
	 *
	 * @return the processing time line
	 */
	public Integer getProcessingTimeLine() {

		return processingTimeLine;
	}

	/**
	 * Sets the processing time line.
	 *
	 * @param processingTimeLine the new processing time line
	 */
	public void setProcessingTimeLine(Integer processingTimeLine) {

		this.processingTimeLine = processingTimeLine;
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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

}
