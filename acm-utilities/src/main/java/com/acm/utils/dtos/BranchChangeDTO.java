/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link BranchChangeDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class BranchChangeDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 497374037766107047L;

	/** The id. */
	private Long id;

	/** The customer id. */
	private Long customerId;

	/** The branch id. */
	private Integer branchId;

	/** The start date. */
	private Date startDate;

	/** The branch name. */
	private String branchName;

	/** The branch description. */
	private String branchDescription;

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
	 * Gets the customer id.
	 *
	 * @return the customer id
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the new customer id
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the branch id.
	 *
	 * @return the branch id
	 */
	public Integer getBranchId() {

		return branchId;
	}

	/**
	 * Sets the branch id.
	 *
	 * @param branchId the new branch id
	 */
	public void setBranchId(Integer branchId) {

		this.branchId = branchId;
	}

	/**
	 * Gets the start date.
	 *
	 * @return the start date
	 */
	public Date getStartDate() {

		return startDate;
	}

	/**
	 * Sets the start date.
	 *
	 * @param startDate the new start date
	 */
	public void setStartDate(Date startDate) {

		this.startDate = startDate;
	}

	/**
	 * Gets the branch name.
	 *
	 * @return the branch name
	 */
	public String getBranchName() {

		return branchName;
	}

	/**
	 * Sets the branch name.
	 *
	 * @param branchName the new branch name
	 */
	public void setBranchName(String branchName) {

		this.branchName = branchName;
	}

	/**
	 * Gets the branch description.
	 *
	 * @return the branch description
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 *
	 * @param branchDescription the new branch description
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
	}

}
