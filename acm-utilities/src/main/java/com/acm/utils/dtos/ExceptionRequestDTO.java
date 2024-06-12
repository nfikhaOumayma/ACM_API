/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * The Class ExceptionRequestDTO.
 */
public class ExceptionRequestDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6595877344637642200L;

	/** The id. */
	private Long id;

	/** The Maker name. */
	private String MakerName;

	/** The maker username. */
	private String makerUsername;

	/** The customer id. */
	private Long customerId;

	/** The customer name. */
	private String customerName;
	/** The product name. */
	private String productName;

	/** The product id. */
	private Long productId;

	/** The product limit. */
	private BigDecimal productLimit;

	/** The allowed amount. */
	private BigDecimal allowedAmount;

	/** The requested amount. */
	private BigDecimal requestedAmount;

	/** 0 : NEW ; 1 : ACCEPTED ; -1 : REJECTED ; -2 :CANCELLED ; 2 : CLOSED. */
	/** The statut. */
	private Integer statut;

	/** The group owner code. */
	private String groupOwnerCode;

	/** The owner name. */
	private String ownerName;

	/** The owner username. */
	private String ownerUsername;

	/** The list statut. */
	private List<Integer> listStatut;

	/** The note. */
	private String note;

	/** The enabled. */
	private Boolean enabled;

	/** The date insertion. */
	private Date dateInsertion;

	/** The date insertion. */
	private Date dateLastUpdate;

	/** The reject note. */
	private String rejectNote;

	/** The branch id. */
	private Integer branchId;

	/**
	 * Instantiates a new exception request DTO.
	 */
	public ExceptionRequestDTO() {

	}

	/**
	 * Instantiates a new exception request DTO.
	 *
	 * @param customerId the customer id
	 * @param statut the statut
	 */
	public ExceptionRequestDTO(Long customerId, Integer statut) {

		this.customerId = customerId;
		this.statut = statut;
	}

	/**
	 * Instantiates a new exception request DTO.
	 *
	 * @param customerId the customer id
	 * @param listStatut the list statut
	 */
	public ExceptionRequestDTO(Long customerId, List<Integer> listStatut) {

		this.customerId = customerId;
		this.listStatut = listStatut;
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
	 * Gets the list statut.
	 *
	 * @return the list statut
	 */
	public List<Integer> getListStatut() {

		return listStatut;
	}

	/**
	 * Sets the list statut.
	 *
	 * @param listStatut the new list statut
	 */
	public void setListStatut(List<Integer> listStatut) {

		this.listStatut = listStatut;
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

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the date last update.
	 *
	 * @return the date last update
	 */
	public Date getDateLastUpdate() {

		return dateLastUpdate;
	}

	/**
	 * Sets the date last update.
	 *
	 * @param dateLastUpdate the new date last update
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {

		this.dateLastUpdate = dateLastUpdate;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ExceptionRequestDTO [MakerName=" + MakerName + ", makerUsername=" + makerUsername
				+ ", customerId=" + customerId + ", customerName=" + customerName + ", productName="
				+ productName + ", productId=" + productId + ", productLimit=" + productLimit
				+ ", allowedAmount=" + allowedAmount + ", requestedAmount=" + requestedAmount
				+ ", statut=" + statut + ", groupOwnerCode=" + groupOwnerCode + ", ownerName="
				+ ownerName + ", ownerUsername=" + ownerUsername + ", listStatut=" + listStatut
				+ ", note=" + note + "]";
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
