/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link CustomerDecisionDTO} class.
 *
 * @author YesserSomai
 * @since 0.5.0
 */
public class CustomerDecisionDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3445530348923810400L;

	/** The id. */
	private Long id;

	/** The contact date. */
	private Date contactDate;

	/** The comments. */
	private String comments;

	/** The id loan. */
	@Mapping("loan.idLoan")
	private Long idLoan;

	/** The status id. */
	private Integer statusId;

	/** The status libelle. */
	private String statusLibelle;

	/** The amount. */
	private BigDecimal amount;

	/** The insert by. */
	private String insertBy;

	/**
	 * Instantiates a new customer desicion DTO.
	 */
	public CustomerDecisionDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new CustomerDecision DTO.
	 * 
	 * @param contactDate the contact Date
	 * @param comments the comments
	 * @param idLoan the loan id
	 * @param statusId the status id
	 * @param amount the amount
	 */
	public CustomerDecisionDTO(Date contactDate, String comments, Long idLoan, Integer statusId,
			BigDecimal amount) {

		this.contactDate = contactDate;
		this.comments = comments;
		this.idLoan = idLoan;
		this.statusId = statusId;
		this.amount = amount;
	}

	/**
	 * Instantiates a new customer decision DTO.
	 *
	 * @param idLoan the id loan
	 */
	public CustomerDecisionDTO(Long idLoan) {

		this.idLoan = idLoan;
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
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
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

	/**
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

}
