/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link CustomerDecision} class.
 *
 * @author YesserSomai
 * @since 0.5.0
 */
@Entity
@Table(name = "ACM_CUSTOMER_DESICION")
public class CustomerDecision extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3464419717945693364L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CUSTOMER_DESICION", unique = true, nullable = false)
	private Long id;

	/** The contact date. */
	@Column(name = "CONTACT_DATE")
	private Date contactDate;

	/** The comments. */
	@Column(name = "COMMENTS")
	private String comments;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/** The status id. */
	@Column(name = "STATUS_ID")
	private Integer statusId;

	/** The status libelle. */
	@Column(name = "STATUS_LIBELLE")
	private String statusLibelle;

	/** The amount. */
	@Column(name = "AMOUNT")
	private BigDecimal amount;

	/**
	 * Instantiates a new customer decision.
	 */
	public CustomerDecision() {

		/*
		 * 
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
	 * Gets the contact date.
	 *
	 * @return the contactDate
	 */
	public Date getContactDate() {

		return contactDate;
	}

	/**
	 * Sets the contact date.
	 *
	 * @param contactDate the contactDate to set
	 */
	public void setContactDate(Date contactDate) {

		this.contactDate = contactDate;
	}

	/**
	 * Gets the comments.
	 *
	 * @return the comments
	 */
	public String getComments() {

		return comments;
	}

	/**
	 * Sets the comments.
	 *
	 * @param comments the comments to set
	 */
	public void setComments(String comments) {

		this.comments = comments;
	}

	/**
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the loan to set
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
	}

	/**
	 * Gets the status id.
	 *
	 * @return the statusId
	 */
	public Integer getStatusId() {

		return statusId;
	}

	/**
	 * Sets the status id.
	 *
	 * @param statusId the statusId to set
	 */
	public void setStatusId(Integer statusId) {

		this.statusId = statusId;
	}

	/**
	 * Gets the status libelle.
	 *
	 * @return the statusLibelle
	 */
	public String getStatusLibelle() {

		return statusLibelle;
	}

	/**
	 * Sets the status libelle.
	 *
	 * @param statusLibelle the statusLibelle to set
	 */
	public void setStatusLibelle(String statusLibelle) {

		this.statusLibelle = statusLibelle;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

}
