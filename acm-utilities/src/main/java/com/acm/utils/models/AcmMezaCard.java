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
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.acm.utils.audit.AuditTrailListener;

/**
 * {@link AcmMezaCard} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_MEZA_CARD")
@EntityListeners(AuditTrailListener.class)
public class AcmMezaCard extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5543903305504584318L;

	/** The Id meza card. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_MEZA_CARD", unique = true, nullable = false)
	private Long idMezaCard;

	/** The merchant ID. */
	@Column(name = "MERCHANT_ID")
	private BigDecimal merchantID;

	/** The card type. */
	@Column(name = "CARD_TYPE")
	private String cardType;

	/** The card number. */
	@Column(name = "CARD_NUMBER")
	private String cardNumber;

	/** The account. */
	@Column(name = "ACCOUNT")
	private String account;

	/** The expirty date. */
	@Column(name = "EXPIRTY_DATE")
	private Date expirtyDate;

	/** The activity date. */
	@Column(name = "ACTIVITY_DATE")
	private Date activityDate;

	/** The embossed name. */
	@Column(name = "EMBOSSED_NAME")
	private String embossedName;

	/** The status. */
	@Column(name = "STATUS")
	private String status;

	/** The branch ID. */
	@Column(name = "BRANCH_ID")
	private Long branchID;

	/** The branch name. */
	@Column(name = "BRANCH_NAME")
	private String branchName;

	/** The acm meza cards. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ACM_ID_CUSTOMER", nullable = true)
	private Customer customer;

	/**
	 * Instantiates a new acm meza card.
	 */
	public AcmMezaCard() {

		/*
		 * EMPTY
		 */
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
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public Customer getCustomer() {

		return customer;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the customer to set
	 */
	public void setCustomer(Customer customer) {

		this.customer = customer;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmMezaCard [" + (idMezaCard != null ? "idMezaCard=" + idMezaCard + ", " : "")
				+ (merchantID != null ? "merchantID=" + merchantID + ", " : "")
				+ (cardType != null ? "cardType=" + cardType + ", " : "")
				+ (cardNumber != null ? "cardNumber=" + cardNumber + ", " : "")
				+ (account != null ? "account=" + account + ", " : "")
				+ (expirtyDate != null ? "expirtyDate=" + expirtyDate + ", " : "")
				+ (activityDate != null ? "activityDate=" + activityDate + ", " : "")
				+ (embossedName != null ? "embossedName=" + embossedName + ", " : "")
				+ (status != null ? "status=" + status + ", " : "")
				+ (branchID != null ? "branchID=" + branchID + ", " : "")
				+ (branchName != null ? "branchName=" + branchName + ", " : "") + "]";
	}
}
