/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link AcmMezaCardDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public class AcmMezaCardDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7760138786768267160L;

	/** The Id meza card. */
	private Long idMezaCard;

	/** The merchant ID. */
	private BigDecimal merchantID;

	/** The card type. */
	private String cardType;

	/** The card number. */
	private String cardNumber;

	/** The account. */
	private String account;

	/** The expirty date. */
	private Date expirtyDate;

	/** The activity date. */
	private Date activityDate;

	/** The embossed name. */
	private String embossedName;

	/** The status. */
	private String status;

	/** The branch ID. */
	private Long branchID;

	/** The branch name. */
	private String branchName;

	/** The customer ID. */
	@Mapping("customer")
	private CustomerDTO customerDTO;

	/** The list status used for find multiple status. */
	private List<String> listStatus;

	/** The acces branch used for find access branch for user. */
	private Boolean accessBranch;

	/**
	 * Instantiates a new acm meza card.
	 */
	public AcmMezaCardDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm meza card DTO.
	 *
	 * @param idMezaCard the id meza card
	 * @param status the status
	 * @param customerDTO the customer DTO
	 */
	public AcmMezaCardDTO(Long idMezaCard, String status, CustomerDTO customerDTO) {

		this.idMezaCard = idMezaCard;
		this.status = status;
		this.customerDTO = customerDTO;
	}

	/**
	 * Instantiates a new acm meza card DTO.
	 *
	 * @param idMezaCard the id meza card
	 * @param status the status
	 */
	public AcmMezaCardDTO(Long idMezaCard, String status) {

		this.idMezaCard = idMezaCard;
		this.status = status;
	}

	/**
	 * Instantiates a new acm meza card DTO.
	 *
	 * @param customerDTO the customer DTO
	 */
	public AcmMezaCardDTO(CustomerDTO customerDTO) {

		this.customerDTO = customerDTO;
	}

	/**
	 * Gets the id meza card.
	 *
	 * @return the id meza card
	 */
	public Long getIdMezaCard() {

		return idMezaCard;
	}

	/**
	 * Sets the id meza card.
	 *
	 * @param idMezaCard the new id meza card
	 */
	public void setIdMezaCard(Long idMezaCard) {

		this.idMezaCard = idMezaCard;
	}

	/**
	 * Gets the merchant ID.
	 *
	 * @return the merchant ID
	 */
	public BigDecimal getMerchantID() {

		return merchantID;
	}

	/**
	 * Sets the merchant ID.
	 *
	 * @param merchantID the new merchant ID
	 */
	public void setMerchantID(BigDecimal merchantID) {

		this.merchantID = merchantID;
	}

	/**
	 * Gets the card type.
	 *
	 * @return the card type
	 */
	public String getCardType() {

		return cardType;
	}

	/**
	 * Sets the card type.
	 *
	 * @param cardType the new card type
	 */
	public void setCardType(String cardType) {

		this.cardType = cardType;
	}

	/**
	 * Gets the card number.
	 *
	 * @return the card number
	 */
	public String getCardNumber() {

		return cardNumber;
	}

	/**
	 * Sets the card number.
	 *
	 * @param cardNumber the new card number
	 */
	public void setCardNumber(String cardNumber) {

		this.cardNumber = cardNumber;
	}

	/**
	 * Gets the account.
	 *
	 * @return the account
	 */
	public String getAccount() {

		return account;
	}

	/**
	 * Sets the account.
	 *
	 * @param account the new account
	 */
	public void setAccount(String account) {

		this.account = account;
	}

	/**
	 * Gets the expirty date.
	 *
	 * @return the expirty date
	 */
	public Date getExpirtyDate() {

		return expirtyDate;
	}

	/**
	 * Sets the expirty date.
	 *
	 * @param expirtyDate the new expirty date
	 */
	public void setExpirtyDate(Date expirtyDate) {

		this.expirtyDate = expirtyDate;
	}

	/**
	 * Gets the activity date.
	 *
	 * @return the activity date
	 */
	public Date getActivityDate() {

		return activityDate;
	}

	/**
	 * Sets the activity date.
	 *
	 * @param activityDate the new activity date
	 */
	public void setActivityDate(Date activityDate) {

		this.activityDate = activityDate;
	}

	/**
	 * Gets the embossed name.
	 *
	 * @return the embossed name
	 */
	public String getEmbossedName() {

		return embossedName;
	}

	/**
	 * Sets the embossed name.
	 *
	 * @param embossedName the new embossed name
	 */
	public void setEmbossedName(String embossedName) {

		this.embossedName = embossedName;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the new status
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branch ID
	 */
	public Long getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the new branch ID
	 */
	public void setBranchID(Long branchID) {

		this.branchID = branchID;
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
	 * Gets the customer DTO.
	 *
	 * @return the customerDTO
	 */
	public CustomerDTO getCustomerDTO() {

		return customerDTO;
	}

	/**
	 * Sets the customer DTO.
	 *
	 * @param customerDTO the customerDTO to set
	 */
	public void setCustomerDTO(CustomerDTO customerDTO) {

		this.customerDTO = customerDTO;
	}

	/**
	 * Gets the list status.
	 *
	 * @return the list status
	 */
	public List<String> getListStatus() {

		return listStatus;
	}

	/**
	 * Sets the list status.
	 *
	 * @param listStatus the new list status
	 */
	public void setListStatus(List<String> listStatus) {

		this.listStatus = listStatus;
	}

	/**
	 * Gets the access branch.
	 *
	 * @return the access branch
	 */
	public Boolean getAccessBranch() {

		return accessBranch;
	}

	/**
	 * Sets the access branch.
	 *
	 * @param accessBranch the new access branch
	 */
	public void setAccessBranch(Boolean accessBranch) {

		this.accessBranch = accessBranch;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmMezaCardDTO [idMezaCard=" + idMezaCard + ", merchantID=" + merchantID
				+ ", cardType=" + cardType + ", cardNumber=" + cardNumber + ", account=" + account
				+ ", expirtyDate=" + expirtyDate + ", activityDate=" + activityDate
				+ ", embossedName=" + embossedName + ", status=" + status + ", branchID=" + branchID
				+ ", branchName=" + branchName + ", customerDTO=" + customerDTO + ", listStatus="
				+ listStatus + ", accessBranch=" + accessBranch + "]";
	}

}
