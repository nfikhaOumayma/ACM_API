/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * The Class MessageDetailsDTO.
 */
public class MessageDetailsDTO {

	/** The id message details. */
	private Long idMessageDetails;

	/** The message body. */
	private String messageBody;

	/** The list id. */
	private int listId;

	/** The sending source. */
	private String sendingSource;

	/** The sms campaign name. */
	private String smsCampaignName;

	/** The to sender. */
	private String toSender;

	/** The category. */
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
