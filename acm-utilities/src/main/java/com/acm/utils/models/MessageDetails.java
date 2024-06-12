/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class MessageDetails.
 */
@Entity
@Table(name = "ACM_MESSAGE_DETAILS")
public class MessageDetails {

	/** The id message details. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_MESSAGE_DETAILS", unique = true, nullable = false)
	private Long idMessageDetails;

	/** The message body. */
	@Column(name = "MESSAGE_BODY")
	private String messageBody;

	/** The list id. */
	@Column(name = "LIST_ID")
	private int listId;

	/** The sending source. */
	@Column(name = "SENDING_SOURCE")
	private String sendingSource;

	/** The to. */
	@Column(name = "TO_SENDER")
	private String toSender;

	/** The date envoi. */
	@Column(name = "DATE_ENVOI")
	private Date dateEnvoi;

	/** The sms campaign name. */
	@Column(name = "SMS_CAMPAIGN_NAME")
	/** The sms campaign name. */
	private String smsCampaignName;

	/** The category. */
	@Column(name = "CATEGORY")
	/** The sms campaign name. */
	private String category;

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
	 * Gets the to.
	 *
	 * @return the to
	 */

	/**
	 * Gets the date envoi.
	 *
	 * @return the date envoi
	 */
	public Date getDateEnvoi() {

		return dateEnvoi;
	}

	/**
	 * Gets the id message details.
	 *
	 * @return the id message details
	 */
	public Long getIdMessageDetails() {

		return idMessageDetails;
	}

	/**
	 * Sets the id message details.
	 *
	 * @param idMessageDetails the new id message details
	 */
	public void setIdMessageDetails(Long idMessageDetails) {

		this.idMessageDetails = idMessageDetails;
	}

	/**
	 * Gets the to sender.
	 *
	 * @return the to sender
	 */
	public String getToSender() {

		return toSender;
	}

	/**
	 * Sets the to sender.
	 *
	 * @param toSender the new to sender
	 */
	public void setToSender(String toSender) {

		this.toSender = toSender;
	}

	/**
	 * Sets the date envoi.
	 *
	 * @param dateEnvoi the new date envoi
	 */
	public void setDateEnvoi(Date dateEnvoi) {

		this.dateEnvoi = dateEnvoi;
	}

	/**
	 * Sets the to.
	 *
	 * @return the message body
	 */

	/**
	 * Gets the message body.
	 *
	 * @return the message body
	 */
	public String getMessageBody() {

		return messageBody;
	}

	/**
	 * Sets the message body.
	 *
	 * @param messageBody the new message body
	 */
	public void setMessageBody(String messageBody) {

		this.messageBody = messageBody;
	}

	/**
	 * Gets the list id.
	 *
	 * @return the list id
	 */
	public int getListId() {

		return listId;
	}

	/**
	 * Sets the list id.
	 *
	 * @param listId the new list id
	 */
	public void setListId(int listId) {

		this.listId = listId;
	}

	/**
	 * Gets the sending source.
	 *
	 * @return the sending source
	 */
	public String getSendingSource() {

		return sendingSource;
	}

	/**
	 * Sets the sending source.
	 *
	 * @param sendingSource the new sending source
	 */
	public void setSendingSource(String sendingSource) {

		this.sendingSource = sendingSource;
	}

	/**
	 * Gets the sms campaign name.
	 *
	 * @return the sms campaign name
	 */
	public String getSmsCampaignName() {

		return smsCampaignName;
	}

	/**
	 * Sets the sms campaign name.
	 *
	 * @param smsCampaignName the new sms campaign name
	 */
	public void setSmsCampaignName(String smsCampaignName) {

		this.smsCampaignName = smsCampaignName;
	}

}
