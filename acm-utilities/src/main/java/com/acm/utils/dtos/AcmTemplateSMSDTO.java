/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class AcmTemplateSMSDTO.
 */
public class AcmTemplateSMSDTO implements Serializable {

	/** The id acm template SMS. */
	private Long idAcmTemplateSMS;

	/** The code SMS event. */
	private String codeSMSEvent;

	/** The message body. */
	private String messageBody;

	/** The Date envoi. */
	private String DateEnvoi;

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
	 * Gets the id acm template SMS.
	 *
	 * @return the id acm template SMS
	 */
	public Long getIdAcmTemplateSMS() {

		return idAcmTemplateSMS;
	}

	/**
	 * Instantiates a new acm template SMSDTO.
	 */
	public AcmTemplateSMSDTO() {

		super();
	}

	/**
	 * Sets the id acm template SMS.
	 *
	 * @param idAcmTemplateSMS the new id acm template SMS
	 */
	public void setIdAcmTemplateSMS(Long idAcmTemplateSMS) {

		this.idAcmTemplateSMS = idAcmTemplateSMS;
	}

	/**
	 * Gets the code SMS event.
	 *
	 * @return the code SMS event
	 */
	public String getCodeSMSEvent() {

		return codeSMSEvent;
	}

	/**
	 * Sets the code SMS event.
	 *
	 * @param codeSMSEvent the new code SMS event
	 */
	public void setCodeSMSEvent(String codeSMSEvent) {

		this.codeSMSEvent = codeSMSEvent;
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
	 * Gets the date envoi.
	 *
	 * @return the date envoi
	 */
	public String getDateEnvoi() {

		return DateEnvoi;
	}

	/**
	 * Sets the date envoi.
	 *
	 * @param dateEnvoi the new date envoi
	 */
	public void setDateEnvoi(String dateEnvoi) {

		DateEnvoi = dateEnvoi;
	}

	/**
	 * Gets the phone number sender.
	 *
	 * @return the phone number sender
	 */
	public String getPhoneNumberSender() {

		return phoneNumberSender;
	}

	/**
	 * Sets the phone number sender.
	 *
	 * @param phoneNumberSender the new phone number sender
	 */
	public void setPhoneNumberSender(String phoneNumberSender) {

		this.phoneNumberSender = phoneNumberSender;
	}

	/** The phone number sender. */
	private String phoneNumberSender;

	/**
	 * Instantiates a new acm template SMSDTO.
	 *
	 * @param idAcmTemplateSMS the id acm template SMS
	 * @param codeSMSEvent the code SMS event
	 * @param messageBody the message body
	 * @param dateEnvoi the date envoi
	 * @param phoneNumberSender the phone number sender
	 */
	public AcmTemplateSMSDTO(Long idAcmTemplateSMS, String codeSMSEvent, String messageBody,
			String dateEnvoi, String phoneNumberSender) {

		super();
		this.idAcmTemplateSMS = idAcmTemplateSMS;
		this.codeSMSEvent = codeSMSEvent;
		this.messageBody = messageBody;
		DateEnvoi = dateEnvoi;
		this.phoneNumberSender = phoneNumberSender;
	}

}
