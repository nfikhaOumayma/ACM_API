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
 * {@link SettingClaims} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_SETTING_CLAIMS")
public class SettingClaims extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_CLAIMS", unique = true, nullable = false)
	private Long id;

	/** The category. */
	@Column(name = "CATEGORY", nullable = false)
	private String category;

	/** The subject. */
	@Column(name = "SUBJECT", nullable = false)
	private String subject;

	/** The assignement. */
	@Column(name = "ASSIGNEMENT", nullable = false)
	private String assignement;

	/** The pripority. */
	@Column(name = "PRIPORITY")
	private String pripority;

	/** The code external. */
	@Column(name = "PROCESSING_TIME_LINE")
	private Integer processingTimeLine;

	/**
	 * Instantiates a new setting motif rejets.
	 */
	public SettingClaims() {

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

}
