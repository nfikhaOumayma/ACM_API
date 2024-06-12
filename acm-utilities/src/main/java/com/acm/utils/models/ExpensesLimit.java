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
 * The persistent class for the ACM_EXPENSES_LIMITS table. {@link ExpensesLimit} class.
 * 
 * @author YesserSomai
 * @since 1.1.3
 */
@Entity
@Table(name = "ACM_EXPENSES_LIMITS")
public class ExpensesLimit extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4382555459185662773L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_EXPENSES_LIMITS", unique = true, nullable = false)
	private Long id;

	/** The id branch. */
	@Column(name = "ID_BRANCH")
	private Long idBranch;

	/** The id expenses type. */
	@Column(name = "ID_EXPENSES_TYPE")
	private Long idExpensesType;

	/** The limit. */
	@Column(name = "LIMIT")
	private Long limit;

	/** The rest limit. */
	@Column(name = "RESTLIMIT")
	private Long restLimit;

	/** The credit account. */
	@Column(name = "CREDIT_ACCOUNT")
	private String cr;

	/** The debit account. */
	@Column(name = "DEBIT_ACCOUNT")
	private String dr;

	/**
	 * Instantiates a new expenses limit.
	 */
	public ExpensesLimit() {

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
