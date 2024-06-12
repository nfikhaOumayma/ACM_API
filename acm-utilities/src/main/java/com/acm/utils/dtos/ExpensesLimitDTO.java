/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The persistent class for the ACM_EXPENSES_LIMITS table. {@link ExpensesLimitDTO} class.
 * 
 * @author YesserSomai
 * @since 1.1.3
 */

public class ExpensesLimitDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1755015690344027683L;

	/** The id. */
	private Long id;

	/** The id branch. */
	private Long idBranch;

	/** The id expenses type. */
	private Long idExpensesType;

	/** The limit. */
	private Long limit;

	/** The rest limit. */
	private Long restLimit;

	/** The credit account. */
	private String cr;

	/** The debit account. */
	private String dr;

	/**
	 * Instantiates a new acm documents.
	 */
	public ExpensesLimitDTO() {

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id branch.
	 *
	 * @return the id branch
	 */
	public Long getIdBranch() {

		return idBranch;
	}

	/**
	 * Sets the id branch.
	 *
	 * @param idBranch the new id branch
	 */
	public void setIdBranch(Long idBranch) {

		this.idBranch = idBranch;
	}

	/**
	 * Gets the id expenses type.
	 *
	 * @return the id expenses type
	 */
	public Long getIdExpensesType() {

		return idExpensesType;
	}

	/**
	 * Sets the id expenses type.
	 *
	 * @param idExpensesType the new id expenses type
	 */
	public void setIdExpensesType(Long idExpensesType) {

		this.idExpensesType = idExpensesType;
	}

	/**
	 * Gets the limit.
	 *
	 * @return the limit
	 */
	public Long getLimit() {

		return limit;
	}

	/**
	 * Sets the limit.
	 *
	 * @param limit the new limit
	 */
	public void setLimit(Long limit) {

		this.limit = limit;
	}

	/**
	 * Gets the rest limit.
	 *
	 * @return the rest limit
	 */
	public Long getRestLimit() {

		return restLimit;
	}

	/**
	 * Sets the rest limit.
	 *
	 * @param restLimit the new rest limit
	 */
	public void setRestLimit(Long restLimit) {

		this.restLimit = restLimit;
	}

	/**
	 * Gets the cr.
	 *
	 * @return the cr
	 */
	public String getCr() {

		return cr;
	}

	/**
	 * Sets the cr.
	 *
	 * @param cr the cr to set
	 */
	public void setCr(String cr) {

		this.cr = cr;
	}

	/**
	 * Gets the dr.
	 *
	 * @return the dr
	 */
	public String getDr() {

		return dr;
	}

	/**
	 * Sets the dr.
	 *
	 * @param dr the dr to set
	 */
	public void setDr(String dr) {

		this.dr = dr;
	}

}
