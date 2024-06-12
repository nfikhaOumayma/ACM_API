/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class ExceptionRequest.
 */
@Entity
@Table(name = "ACM_EXCEPTION_REQUEST")
public class ExceptionRequest extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8626784125177535786L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The possessor name. */
	@Column(name = "MAKER_NAME", nullable = false)
	private String MakerName;

	/** The possessor username. */
	@Column(name = "MAKER_USERNAME", nullable = false)
	private String makerUsername;

	/** The customer id. */
	@Column(name = "CUSTOMER_ID", nullable = false)
	private Long customerId;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME", nullable = false)
	private String customerName;

	/** The product name. */
	@Column(name = "PRODUCT_NAME", nullable = false)
	private String productName;

	/** The product id. */
	@Column(name = "PRODUCT_ID", nullable = false)
	private Long productId;

	/** The product limit. */
	@Column(name = "PRODUCT_LIMIT", nullable = false)
	private BigDecimal productLimit;

	/** The allowed amount. */
	@Column(name = "ALLOWED_AMOUNT", nullable = false)
	private BigDecimal allowedAmount;

	/** The requested amount. */
	@Column(name = "REQUESTED_AMOUNT", nullable = false)
	private BigDecimal requestedAmount;

	/** The statut. */
	@Column(name = "STATUT", nullable = false)
	private Integer statut;

	/** The group owner code. */
	@Column(name = "GROUP_OWNER_CODE", nullable = false)
	private String groupOwnerCode;

	/** The group owner code. */
	@Column(name = "OWNER_NAME", nullable = false)
	private String ownerName;

	/** The group owner code. */
	@Column(name = "OWNER_USERNAME", nullable = false)
	private String ownerUsername;

	/** The note. */
	@Column(name = "NOTE", nullable = false)
	private String note;

	/** The reject note. */
	@Column(name = "REJECT_NOTE")
	private String rejectNote;

	/** The branchID. */
	@Column(name = "BRANCH_ID")
	private Integer branchId;

	/**
	 * Instantiates a new exception request.
	 */
	public ExceptionRequest() {

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
	 * Gets the product name.
	 *
	 * @return the product name
	 */
	public String getProductName() {

		return productName;
	}

	/**
	 * Sets the product name.
	 *
	 * @param productName the new product name
	 */
	public void setProductName(String productName) {

		this.productName = productName;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the product id
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the new product id
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the product limit.
	 *
	 * @return the product limit
	 */
	public BigDecimal getProductLimit() {

		return productLimit;
	}

	/**
	 * Sets the product limit.
	 *
	 * @param productLimit the new product limit
	 */
	public void setProductLimit(BigDecimal productLimit) {

		this.productLimit = productLimit;
	}

	/**
	 * Gets the allowed amount.
	 *
	 * @return the allowed amount
	 */
	public BigDecimal getAllowedAmount() {

		return allowedAmount;
	}

	/**
	 * Sets the allowed amount.
	 *
	 * @param allowedAmount the new allowed amount
	 */
	public void setAllowedAmount(BigDecimal allowedAmount) {

		this.allowedAmount = allowedAmount;
	}

	/**
	 * Gets the requested amount.
	 *
	 * @return the requested amount
	 */
	public BigDecimal getRequestedAmount() {

		return requestedAmount;
	}

	/**
	 * Sets the requested amount.
	 *
	 * @param requestedAmount the new requested amount
	 */
	public void setRequestedAmount(BigDecimal requestedAmount) {

		this.requestedAmount = requestedAmount;
	}

	/**
	 * Gets the statut.
	 *
	 * @return the statut
	 */
	public Integer getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the new statut
	 */
	public void setStatut(Integer statut) {

		this.statut = statut;
	}

	/**
	 * Gets the group owner code.
	 *
	 * @return the group owner code
	 */
	public String getGroupOwnerCode() {

		return groupOwnerCode;
	}

	/**
	 * Sets the group owner code.
	 *
	 * @param groupOwnerCode the new group owner code
	 */
	public void setGroupOwnerCode(String groupOwnerCode) {

		this.groupOwnerCode = groupOwnerCode;
	}

	/**
	 * Gets the owner name.
	 *
	 * @return the owner name
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the new owner name
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the owner username.
	 *
	 * @return the owner username
	 */
	public String getOwnerUsername() {

		return ownerUsername;
	}

	/**
	 * Sets the owner username.
	 *
	 * @param ownerUsername the new owner username
	 */
	public void setOwnerUsername(String ownerUsername) {

		this.ownerUsername = ownerUsername;
	}

	/**
	 * Gets the maker name.
	 *
	 * @return the maker name
	 */
	public String getMakerName() {

		return MakerName;
	}

	/**
	 * Sets the maker name.
	 *
	 * @param makerName the new maker name
	 */
	public void setMakerName(String makerName) {

		MakerName = makerName;
	}

	/**
	 * Gets the maker username.
	 *
	 * @return the maker username
	 */
	public String getMakerUsername() {

		return makerUsername;
	}

	/**
	 * Sets the maker username.
	 *
	 * @param makerUsername the new maker username
	 */
	public void setMakerUsername(String makerUsername) {

		this.makerUsername = makerUsername;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customer name
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the new customer name
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the note.
	 *
	 * @return the note
	 */
	public String getNote() {

		return note;
	}

	/**
	 * Sets the note.
	 *
	 * @param note the new note
	 */
	public void setNote(String note) {

		this.note = note;
	}

	/**
	 * Gets the reject note.
	 *
	 * @return the reject note
	 */
	public String getRejectNote() {

		return rejectNote;
	}

	/**
	 * Sets the reject note.
	 *
	 * @param rejectNote the new reject note
	 */
	public void setRejectNote(String rejectNote) {

		this.rejectNote = rejectNote;
	}

	/**
	 * Gets the branch id.
	 *
	 * @return the branchId
	 */
	public Integer getBranchId() {

		return branchId;
	}

	/**
	 * Sets the branch id.
	 *
	 * @param branchId the branchId to set
	 */
	public void setBranchId(Integer branchId) {

		this.branchId = branchId;
	}

}
